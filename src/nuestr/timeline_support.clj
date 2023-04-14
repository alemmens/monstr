(ns nuestr.timeline-support
  (:require
   [clojure.tools.logging :as log]
   [nuestr.domain :as domain]
   [nuestr.parse :as parse]
   [nuestr.util :as util]
   #_[loom.graph :as loom]
   #_[loom.attr :as loom-attr]
   )
  (:import
   (nuestr.domain TextNote TextNoteWrapper #_UITextNote #_ UITextNoteWrapper)))

#_
(defn- create-node->num-predecessors
  [graph]
  (reduce (fn [acc n]
            (assoc acc n (count (loom/predecessors graph n))))
    {} (loom/nodes graph)))

#_
(defn- likely-root
  [graph]
  (let [num-predecessors (create-node->num-predecessors graph)]
    (first (apply max-key val num-predecessors))))

#_
(defn max-path-length [a b edges]
  (if (some #{[a b]} edges)
    (max 1 (max-path-length a b (remove #{[a b]} edges)))
    (let [starts (filter (fn [[x _]] (= x a))
                         edges)]
      (if (seq starts)
        (apply max
               (map (fn [[x y]]
                      (+ 1 (max-path-length y b
                                            (remove #{[x y]} edges))))
                    
                    starts))
        -1000000))))

#_
;; Example graph that needs to be simplifed.
(["a" "8"]
 ["3" "a"]
 ["3" "8"] ; needs to be removed
 ["8" "1"])

#_
(defn- simplify-graph [g]
  ;; Clean up by removing edges 1->3 if there's also an edge 1->2->3  
  (apply loom/remove-edges g
         ;; If a child A is reachable from one of the other children B of node N, remove
         ;; the edge from N to A.
         (let [edges (loom/edges g)]
           (filter (fn [[a b]] (> (max-path-length a b edges) 1))
                   edges))))

#_
(defn- simplify-graph [g] g)

#_
(defn- contribute!*
  [graph id parent-ids]
  (as-> graph G
    (apply loom/add-nodes G (cons id parent-ids))
    (apply loom/add-edges G (map #(vector id %1) parent-ids))
    ;; Expect parent-ids to be in order and we'll create edges for [:a :b :c].
    ;; Like so: :c -> :b -> :a
    (apply loom/add-edges G (map vec (partition 2 1 (reverse parent-ids))))))

#_
(defn- ->note
  ^UITextNote [pruned-graph n seen?]
  (let [seen? (conj seen? n)
        kids (mapv #(->note pruned-graph % seen?)
                   (filter (complement seen?) (loom/predecessors pruned-graph n)))]
    (if-let [{:keys [id pubkey created_at content tags etag-ids ptag-ids]}
             (loom-attr/attr pruned-graph n ::data)]
      (domain/->UITextNote id pubkey content created_at tags etag-ids ptag-ids kids false)
      ;; We don't have the data for this node yet, so we create a special :missing
      ;; UITextNote.
      (domain/->UITextNote n nil (format "<missing:%s>" n) nil [] [] [] kids true))))

#_
(defn show-graph [g]
  (log/debugf "Graph %s" (pr-str (loom/edges g))))

#_
(defn- build*
  [graph]
  (let [use-root (likely-root graph)
        graph' (simplify-graph graph)]
    #_(show-graph graph')
    (->note graph' use-root #{})))

#_
(defn contribute!
  ^TextNoteWrapper
  [^TextNoteWrapper wrapper {:keys [id created_at] :as event-obj} etag-ids ptag-ids]
  ;; NOTE: we expect one of id or etag-ids to exist in the provided wrapper's
  ;; :loom-graph (or the :loom-graph should be empty) and we expect the
  ;; :loom-graph to be connected; this implies that our contributions here
  ;; will also leave the graph fully connected.
  #_(log/debugf "Contributing %s with content %s" id (:content event-obj))
  (let [parents etag-ids
        graph (contribute!* (:loom-graph wrapper) id parents)
        graph (loom-attr/add-attr graph id
                ::data (-> event-obj
                           (select-keys [:id :pubkey :created_at :content :tags])
                           (assoc :etag-ids parents)
                           (assoc :ptag-ids ptag-ids)))]
    #_(log/debugf "Graph now has %d nodes" (count (loom/nodes graph)))
    (assoc wrapper
           :loom-graph graph
           :max-timestamp (max (or (:max-timestamp wrapper) 0) (or created_at 0))
           :root (build* graph))))

#_
(defn init!
  ^UITextNoteWrapper [event-obj etag-ids ptag-ids]
  (let [x (domain/->UITextNoteWrapper (loom/digraph) 0 -1 nil)]
    (contribute! x event-obj etag-ids ptag-ids)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building a thread tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Node
    [id child-ids parent-ids descendant-ids])

;;;
;;; Adding descendants
;;;

(defn add-descendant
  "Returns an updated id->node map."
  [id->node parent-id descendant]
  (if-let [parent (get id->node parent-id)]
    (let [new-parent (->Node (:id parent)
                             '()
                             (:parent-ids parent)
                             (conj (:descendant-ids parent) (:id descendant)))]
      (assoc id->node (:id parent) new-parent))
    id->node))

(defn process-parents
  "Returns an updated id->node map."
  [id->node descendant parent-ids]
  (loop [parent-ids parent-ids
         id->node id->node]
    (if (empty? parent-ids)
      id->node
      (recur (rest parent-ids)
             (add-descendant id->node (first parent-ids) descendant)))))

(defn add-descendants
  "Returns an updated id->node map."
  [id->node nodes]
  (loop [nodes nodes
         id->node id->node]
    (if (empty? nodes)
      id->node
      (let [descendant (first nodes)
            parent-ids (:parent-ids descendant)]
        (recur (rest nodes)
               (process-parents id->node descendant parent-ids))))))

;;;
;;; Adding direct children
;;;

(defn is-descendant-id? [id node]
  (.contains (:descendant-ids node) id))

(defn is-direct-child-id?
  "Returns true iff `descendant-id` is a direct child id of `node`.
   A descendant D is a direct child if none of the other descendants have
 D as descendant."
  [descendant-id node id->node]
  (let [other-descendant-ids (remove #{descendant-id} (:descendant-ids node))
        other-descendants (map #(get id->node %) other-descendant-ids)]
    (not-any? #(is-descendant-id? descendant-id %)
              other-descendants)))

(defn add-children
  "Takes an id->node map where the descendants have already been computed.
  Returns an id->node map with the direct children computed for each node.
  A descendant D is a direct child if none of the other descendants have
  D as descendant."
  [id->node]
  (loop [ids (keys id->node)
         id->node id->node]
    (if (empty? ids)
      id->node
      (let [id (first ids)
            node (get id->node id)
            child-ids (remove #(not (is-direct-child-id? % node id->node))
                              (:descendant-ids node))
            new-node (->Node (:id node)
                             child-ids
                             (:parent-ids node)
                             (:descendant-ids node))]
        (recur (rest ids)
               (assoc id->node id new-node))))))

;;;
;;; Building the thread data structure
;;;

(defn e-tags [event]
  (parse/parse-tags event "e"))

(defn build-thread
  "Returns an id->node map."
  [events]
  (let [nodes (map #(->Node (:id %) '() (e-tags %) '())
                   events)
        id->node (into {} (map (fn [node] [(:id node) node])
                               nodes))]
    (add-children (add-descendants id->node nodes))))

(defn is-root? [node depth]
  (= depth (count (:parent-ids node))))

(defn thread-roots
  "Returns a sequence of root nodes, i.e. nodes without any parents."
  [id->node depth]
  (filter #(is-root? % depth) (vals id->node)))

(defn build-note ^TextNote [^Node parent id->node id->event]
  (let [child-ids (:child-ids parent)
        children (remove nil? (map #(build-note (get id->node %) id->node id->event)
                                   child-ids))
        id (:id parent)
        parent-event (get id->event id)
        child-events (map #(get id->event %) child-ids)
        timestamp (or (:created_at parent-event) 0)]
    (when parent-event
      (domain/->TextNote id
                         (:pubkey parent-event)
                         (:content parent-event)
                         timestamp
                         (:tags parent-event)
                         (parse/e-tags parent-event)
                         (parse/p-tags parent-event)
                         children
                         (:missing? parent-event)))))


(defn tree-ids [^TextNote note]
  (cons (:id note)
        (mapcat tree-ids (:children note))))

(defn tree-max-timestamp [^TextNote note]
  (max (or (:timestamp note) 0)
       (reduce max 0 (map tree-max-timestamp (:children note)))))

(defn build-notes [roots id->node id->event]
  (let [note (build-note (first roots) id->node id->event)]
    (if (= 1 (count roots))
      (list note)
      (let [ids (tree-ids note)]
        (cons note
              (build-notes (rest roots)
                           ;; Make sure we don't include events more than once.
                           (apply dissoc id->node ids)
                           (apply dissoc id->event ids)))))))
          
(defn build-text-note-wrappers
  "Returns a vector with [1] a sequence of TextNoteWrapper, [2]
  an id->node map, [3] an id->event map."
  [events]
  (let [id->node (build-thread events)
        id->event (into {} (map (fn [e] [(:id e) e]) events))
        roots (or (seq (thread-roots id->node 0))
                  ;; Apparently we didn't find any root event.
                  ;; Then use the 'second level' roots (i.e. nodes
                  ;; with only 1 parent) instead.
                  (seq (thread-roots id->node 1))
                  ;; punt: just use all nodes as roots.
                  (vals id->node))
        notes (build-notes roots id->node id->event)]
    [(map #(domain/->TextNoteWrapper (tree-max-timestamp %) %)
          notes)
     id->node
     id->event]))


(defn tree-size [^TextNote note]
  (+ 1
     (reduce + (map tree-size (:children note)))))

(defn wrapper-tree-size [^TextNoteWrapper wrapper]
  (tree-size (:root wrapper)))
