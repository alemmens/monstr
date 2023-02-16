(ns monstr.timeline
  (:require
    [cljfx.api :as fx]
    [clojure.set :as set]
    [clojure.tools.logging :as log]
    [monstr.domain :as domain]
    [monstr.parse :as parse]
    [monstr.timeline-support :as timeline-support]
    [monstr.flat-timeline :as flat-timeline]
    [monstr.util :as util]
    [monstr.util-java :as util-java])
  (:import (javafx.collections FXCollections ObservableList)
           (java.util HashMap HashSet Set)
           (monstr.domain UITextNote UITextNoteWrapper)
           (javafx.scene.control ListView)
           (javafx.collections.transformation FilteredList)))

(defrecord Timeline
  ;; these field values are only ever mutated on fx thread
  [^ObservableList adapted-list
   ^ObservableList observable-list ;; contains UITextNoteWrapper
   ^HashMap author-pubkey->item-id-set
   ^HashMap item-id->index
   ^HashSet item-ids
   timeline-epoch-vol])

(defn new-timeline
  []
  ;; NOTE: we're querying and subscribing to all of time but for now, for ux
  ;; experience, we filter underlying data by n days
  ;; todo we'll really wish to query/subscribe at an epoch and only update it on scroll etc.
  (let [init-timeline-epoch (-> (util/days-ago 20) .getEpochSecond)
        timeline-epoch-vol (volatile! init-timeline-epoch)
        observable-list (FXCollections/observableArrayList)
        filtered-list (FilteredList. observable-list
                        (util-java/->Predicate #(> (:max-timestamp %) init-timeline-epoch)))
        adapted-list (.sorted
                       filtered-list
                       ;; latest wrapper entries first:
                       (comparator #(< (:max-timestamp %2) (:max-timestamp %1))))]
    (->Timeline
      adapted-list
      observable-list
      (HashMap.)
      (HashMap.)
      (HashSet.)
      timeline-epoch-vol)))

(defn dispatch-metadata-update!
  [*state {:keys [pubkey] :as _event-obj}]
  (fx/run-later
    (let [{:keys [identity-timeline]} @*state]
      (doseq [[_identity-pubkey timeline] identity-timeline]
        (let [{:keys [^ObservableList observable-list
                      ^HashMap author-pubkey->item-id-set
                      ^HashMap item-id->index]} timeline]
          (doseq [item-id (seq (.get author-pubkey->item-id-set pubkey))]
            (when-let [item-idx (.get item-id->index item-id)]
              (let [curr-wrapper (.get observable-list item-idx)]
                ;; todo why doesn't this refresh timeline immediately?
                (.set observable-list item-idx
                  (assoc curr-wrapper :touch-ts (System/currentTimeMillis)))))))))))

(defn dispatch-text-note!
  [*state {:keys [id pubkey created_at content] :as event-obj}]
  {:pre [(some? pubkey)]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce
  (fx/run-later
    (let [{:keys [identity-timeline] :as _state-snap} @*state]
      (doseq [[identity-pubkey timeline] identity-timeline]
        (let [{:keys [^ObservableList observable-list
                      ^HashMap author-pubkey->item-id-set
                      ^HashMap item-id->index
                      ^HashSet item-ids]} timeline]
          (when-not (.contains item-ids id)
            (let [ptag-ids (parse/parse-tags event-obj "p")]
              (when (or
                      ;; our item-id->index map will have a key for any id that
                      ;; has been referenced by any other accepted text note.
                      ;; so we also want to accept those "missing" notes:
                      (.containsKey item-id->index id)
                      (flat-timeline/accept-text-note? *state identity-pubkey ptag-ids event-obj))
                (.add item-ids id)
                (.merge author-pubkey->item-id-set pubkey (HashSet. [id])
                  (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
                (let [etag-ids (parse/parse-tags event-obj "e") ;; order matters
                      id-closure (cons id etag-ids)
                      existing-idx (first (keep #(.get item-id->index %) id-closure))]
                  (if (some? existing-idx)
                    (let [curr-wrapper (.get observable-list existing-idx)
                          new-wrapper (timeline-support/contribute!
                                        curr-wrapper event-obj etag-ids ptag-ids)]
                      (doseq [x id-closure]
                        (.put item-id->index x existing-idx))
                      (.set observable-list existing-idx new-wrapper))
                    (let [init-idx (.size observable-list)
                          init-wrapper (timeline-support/init! event-obj etag-ids ptag-ids)]
                      (doseq [x id-closure]
                        (.put item-id->index x init-idx))
                      (.add observable-list init-wrapper))))))))))))

(defn update-active-timeline!
  [*state public-key] ;; note public-key may be nil!
  (fx/run-later
    (swap! *state
      (fn [{:keys [^ListView home-ux identity-timeline] :as curr-state}]
        (.setItems home-ux
          ^ObservableList (or
                            (:adapted-list (get identity-timeline public-key))
                            (FXCollections/emptyObservableList)))
        (assoc curr-state :active-key public-key)))))
