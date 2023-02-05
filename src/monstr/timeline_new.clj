(ns monstr.timeline-new
  (:require [cljfx.api :as fx]
            [clojure.set :as set]
            [monstr.domain :as domain]
            [monstr.parse :as parse]
            [monstr.util :as util]
            [monstr.util-java :as util-java])
  (:import (java.util HashMap HashSet)
           (javafx.collections FXCollections ObservableList)
           (javafx.collections.transformation FilteredList)
           (javafx.scene.control ListView)))

(defrecord TimelineNew
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
    (->TimelineNew
      adapted-list
      observable-list
      (HashMap.)
      (HashMap.)
      (HashSet.)
      timeline-epoch-vol)))

(defn accept-text-note?
  [*state identity-pubkey parsed-ptags {:keys [pubkey] :as _event-obj}]
  (let [{:keys [contact-lists]} @*state
        {:keys [parsed-contacts] :as _contact-list} (get contact-lists identity-pubkey)
        ;; consider: optimization--not having to create contact set each note
        contact-keys-set (into #{} (map :public-key) parsed-contacts)
        ptag-keys-set (set parsed-ptags)]
    (or
      ;; identity's own note
      (= pubkey identity-pubkey)
      ;; the text-note's pubkey matches an identity's contact
      (contact-keys-set pubkey)
      ;; the text-note's ptags reference identity itself
      (ptag-keys-set identity-pubkey)
      ;; the text-note's ptags references one of identities contacts
      (not-empty (set/intersection contact-keys-set ptag-keys-set)))))

(defn dispatch-metadata-update!
  [*state {:keys [pubkey] :as _event-obj}]
  (fx/run-later
    (let [{:keys [identity-timeline-new]} @*state]
      (doseq [[_identity-pubkey timeline] identity-timeline-new]
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
    (let [{:keys [identity-timeline-new] :as _state-snap} @*state]
      (doseq [[identity-pubkey timeline] identity-timeline-new]
        (let [{:keys [^ObservableList observable-list
                      ^HashMap author-pubkey->item-id-set
                      ^HashMap item-id->index
                      ^HashSet item-ids]} timeline]
          (when-not (.contains item-ids id)
            (let [ptag-ids (parse/parse-tags event-obj "p")]
              (when (accept-text-note? *state identity-pubkey ptag-ids event-obj)
                (.add item-ids id)
                (.merge author-pubkey->item-id-set pubkey (HashSet. [id])
                  (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
                (let [init-idx (.size observable-list)
                      init-note (domain/->UITextNoteNew event-obj created_at)]
                  (.put item-id->index id init-idx)
                  (.add observable-list init-note))))))))))

(defn update-active-timeline!
  [*state public-key] ;; note public-key may be nil!
  (fx/run-later
    (swap! *state
      (fn [{:keys [^ListView home-ux-new identity-timeline-new] :as curr-state}]
        ;; oops this is a misuse here of swap! - we probably shouldn't setItems here
        ;; *within* the swap! *state b/c we can get invoked multiple times by
        ;; swap!. should move this .setItems outside of swap! really and just
        ;; get the value by derefing the state.
        (.setItems home-ux-new
          ^ObservableList (or
                            (:adapted-list (get identity-timeline-new public-key))
                            (FXCollections/emptyObservableList)))
        (assoc curr-state :active-key public-key)))))
