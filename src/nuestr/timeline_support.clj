(ns nuestr.timeline-support
  (:require
   [clojure.tools.logging :as log]
   [nuestr.domain :as domain]
   [loom.graph :as loom]
   [loom.attr :as loom-attr])
  (:import
   (nuestr.domain UITextNote UITextNoteWrapper)))

(defn- create-node->num-predecessors
  [graph]
  (reduce (fn [acc n]
            (assoc acc n (count (loom/predecessors graph n))))
    {} (loom/nodes graph)))

(defn- likely-root
  [graph]
  (let [num-predecessors (create-node->num-predecessors graph)]
    (first (apply max-key val num-predecessors))))


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

(defn- simplify-graph [g]
  ;; Clean up by removing edges 1->3 if there's also an edge 1->2->3  
  (apply loom/remove-edges g
         ;; If a child A is reachable from one of the other children B of node N, remove
         ;; the edge from N to A.
         (let [edges (loom/edges g)]
           (filter (fn [[a b]] (> (max-path-length a b edges) 1))
                   edges))))

(defn- contribute!*
  [graph id parent-ids]
  (as-> graph G
    (apply loom/add-nodes G (cons id parent-ids))
    (apply loom/add-edges G (map #(vector id %1) parent-ids))
    ;; Expect parent-ids to be in order and we'll create edges for [:a :b :c].
    ;; Like so: :c -> :b -> :a
    (apply loom/add-edges G (map vec (partition 2 1 (reverse parent-ids))))))

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

(defn show-graph [g]
  (log/debugf "Graph %s" (pr-str (loom/edges g))))

(defn- build*
  [graph]
  (let [use-root (likely-root graph)
        graph' (simplify-graph graph)]
    #_(show-graph graph')
    (->note graph' use-root #{})))

(defn contribute!
  ^UITextNoteWrapper
  [^UITextNoteWrapper wrapper {:keys [id created_at] :as event-obj} etag-ids ptag-ids]
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
           :note-count (count (loom/nodes graph))
           :max-timestamp (max (or (:max-timestamp wrapper) 0) (or created_at 0))
           :root (build* graph))))

(defn init!
  ^UITextNoteWrapper [event-obj etag-ids ptag-ids]
  (let [x (domain/->UITextNoteWrapper (loom/digraph) 0 -1 nil)]
    (contribute! x event-obj etag-ids ptag-ids)))
