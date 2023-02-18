(ns monstr.timeline-support
  (:require
   [clojure.tools.logging :as log]
   [monstr.domain :as domain]
   [loom.graph :as loom]
   [loom.attr :as loom-attr])
  (:import
   (monstr.domain UITextNote UITextNoteWrapper)))

(defn- create-node->num-predecessors
  [graph]
  (reduce (fn [acc n]
            (assoc acc n (count (loom/predecessors graph n))))
    {} (loom/nodes graph)))

(defn- likely-root
  [graph]
  (let [num-predecessors (create-node->num-predecessors graph)]
    (first (apply max-key val num-predecessors))))

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

(defn- build*
  [graph]
  (let [use-root (likely-root graph)
        graph' (apply loom/remove-edges graph
                      ;; Shed all edges to root for nodes deeper than one.
                      (keep (fn [n]
                              (let [out (loom/out-edges graph n)]
                                (when (> (count out) 1)
                                  [n use-root])))
                            (loom/nodes graph)))]
    (->note graph' use-root #{})))

(defn contribute!
  ^UITextNoteWrapper
  [^UITextNoteWrapper wrapper {:keys [id created_at] :as event-obj} etag-ids ptag-ids]
  ;; NOTE: we expect one of id or etag-ids to exist in the provided wrapper's
  ;; :loom-graph (or the :loom-graph should be empty) and we expect the
  ;; :loom-graph to be connected; this implies that our contributions here
  ;; will also leave the graph fully connected.
  (let [graph (contribute!* (:loom-graph wrapper) id etag-ids)
        graph (loom-attr/add-attr graph id
                ::data (-> event-obj
                           (select-keys [:id :pubkey :created_at :content :tags])
                           (assoc :etag-ids etag-ids)
                           (assoc :ptag-ids ptag-ids)))]
    (assoc wrapper
           :loom-graph graph
           :note-count (count (loom/nodes graph))
           :max-timestamp (max (:max-timestamp wrapper) created_at)
           :root (build* graph))))

(defn init!
  ^UITextNoteWrapper [event-obj etag-ids ptag-ids]
  (let [x (domain/->UITextNoteWrapper (loom/digraph) 0 -1 nil)]
    (contribute! x event-obj etag-ids ptag-ids)))
