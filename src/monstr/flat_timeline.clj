(ns monstr.flat-timeline
  (:require [cljfx.api :as fx]
            [clojure.set :as set]
            [clojure.tools.logging :as log]
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
   timeline-epoch-vol
   relays        ; Sequence of relay urls for this timeline.
   show-thread?  ; If true, show the current thread instead of a flat timeline.
   note-id       ; The note that is the focus of the thread. Only relevant when showing a thread.
   ])

(defn new-timeline
  [relays show-thread?]
  ;; NOTE: we're querying and subscribing to all of time but for now, for ux
  ;; experience, we filter underlying data by n days
  ;; todo we'll really wish to query/subscribe at an epoch and only update it on scroll etc.
  (let [init-timeline-epoch (-> (util/days-ago 2) .getEpochSecond)
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
      timeline-epoch-vol
      relays
      show-thread?
      nil
      )))

(defn new-timelines
  [relays]
  (map #(new-timeline (list %) false) relays))

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
   (doseq [[identity-pubkey timelines] (:identity->timelines @*state)]
     (doseq [timeline timelines]
       (let [{:keys [^ObservableList observable-list
                     ^HashMap author-pubkey->item-id-set
                     ^HashMap item-id->index]}
             timeline]
         (doseq [item-id (seq (.get author-pubkey->item-id-set pubkey))]
           (when-let [item-idx (.get item-id->index item-id)]
             (let [curr-wrapper (.get observable-list item-idx)]
               ;; todo why doesn't this refresh timeline immediately?
               (.set observable-list item-idx
                     (assoc curr-wrapper :touch-ts (System/currentTimeMillis)))))))))))

(defn event-is-relevant-for-timeline?
  "An event is relevant for a timeline if the timeline's relays have some overlap with the
   event's relays."
  [event timeline]
  (not-empty (set/intersection (set (:relays timeline))
                               (set (:relays event)))))

(defn dispatch-text-note!
  "Dispatch a text note to all timelines for which the given event is relevant."
  [*state {:keys [id pubkey created_at content relays] :as event-obj}]
  {:pre [(some? pubkey)]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce?
  (fx/run-later
   #_(log/debugf "Dispatching text note %s" event-obj)
   (doseq [[identity-pubkey timelines] (:identity->timelines @*state)]
     (doseq [timeline timelines]
       (when (event-is-relevant-for-timeline? event-obj timeline)
         (let [{:keys [^ObservableList observable-list
                       ^HashMap author-pubkey->item-id-set
                       ^HashMap item-id->index
                       ^HashSet item-ids]}
               timeline]
           #_(log/debugf "Found timeline for event %s and relays %s"
                       event-obj
                       (:relays timeline))
           (when-not (.contains item-ids id)
             (let [ptag-ids (parse/parse-tags event-obj "p")]
               (when (accept-text-note? *state identity-pubkey ptag-ids event-obj)
                 ; (log/debugf "Adding id %s to item-ids %s" id item-ids)
                 (.add item-ids id)
                 (.merge author-pubkey->item-id-set pubkey (HashSet. [id])
                         (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
                 (let [init-idx (.size observable-list)
                       init-note (domain/->UITextNoteNew event-obj created_at)]
                   (.put item-id->index id init-idx)
                   (.add observable-list init-note)))))))))))

(defn update-active-timelines!
  "Update the active timelines for the identity with the given public key."
  [*state public-key] ;; note public-key may be nil!
  (fx/run-later
   (log/debugf "Updating active flat timelines for %s" public-key)
   (doseq [timeline (get (:identity->timelines @*state) public-key)]
     ;; Find the relevant home (listview) and update it.
     #_(log/debugf "Relay url list %s" (domain/relay-urls timeline))
     (let [relay-urls (set (:relays timeline))
           home (get (:homes @*state) relay-urls)]
       #_(log/debugf "Found home %s for relays %s" home relay-urls)
       (when home
         (.setItems home
                    ^ObservableList (or (:adapted-list timeline)
                                        (FXCollections/emptyObservableList))))))
   ;; Update the state's active public key.
   (swap! *state
          (fn [state]
            (assoc state :active-key public-key)))))

