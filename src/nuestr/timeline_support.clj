(ns nuestr.timeline-support
  (:require
   [clojure.tools.logging :as log]
   [nuestr.domain :as domain]
   [nuestr.parse :as parse]
   [nuestr.util :as util])
  (:import
   (nuestr.domain TextNote TextNoteWrapper)))

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
