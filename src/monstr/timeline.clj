(ns monstr.timeline
  (:require [cljfx.api :as fx]
            [clojure.set :as set]
            [clojure.tools.logging :as log]
            [monstr.domain :as domain]
            [monstr.parse :as parse]
            [monstr.timeline-support :as timeline-support]
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
   relays        ; Set of relay urls for this timeline.
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
      nil)))

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

#_
(defn dispatch-metadata-update!
  [*state {:keys [pubkey] :as _event-obj}]
  (fx/run-later
   (doseq [[identity-pubkey columns] (:identity->columns @*state)]
     (doseq [column columns]
       (doseq [timeline (list (:flat-timeline column) (:thread-timeline column))]
         (let [{:keys [^ObservableList observable-list
                       ^HashMap author-pubkey->item-id-set
                       ^HashMap item-id->index]}
               timeline]
           (doseq [item-id (seq (.get author-pubkey->item-id-set pubkey))]
             (when-let [item-idx (.get item-id->index item-id)]
               (let [curr-wrapper (.get observable-list item-idx)]
                 ;; todo why doesn't this refresh timeline immediately?
                 (.set observable-list item-idx
                       (assoc curr-wrapper :touch-ts (System/currentTimeMillis))))))))))))

(defn dispatch-metadata-update!
  [*state {:keys [pubkey] :as _event-obj}]
  (fx/run-later
   ;; TODO: Add thread pane.
   (doseq [timeline (domain/flat-timelines @*state)]
     (let [{:keys [^ObservableList observable-list
                   ^HashMap author-pubkey->item-id-set
                   ^HashMap item-id->index]}
           timeline]
       (doseq [item-id (seq (.get author-pubkey->item-id-set pubkey))]
         (when-let [item-idx (.get item-id->index item-id)]
           (let [curr-wrapper (.get observable-list item-idx)]
             ;; todo why doesn't this refresh timeline immediately?
             (.set observable-list item-idx
                   (assoc curr-wrapper :touch-ts (System/currentTimeMillis))))))))))

(defn- event-is-relevant-for-timeline?
  "An event is relevant for a timeline if the timeline's relays have some overlap with the
   event's relays."
  [event timeline]
  #_(log/debugf "Checking relevance for timeline %s and event %s"
                (set (:relays timeline))
                (set (:relays event)))
  (not-empty (set/intersection (set (:relays timeline))
                               (set (:relays event)))))

(defn flat-dispatch!
  [*state timeline identity-pubkey
   {:keys [id pubkey created_at content] :as event-obj}]
  #_(log/debugf "Flat dispatch for %s" event-obj)
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids]}
        timeline]
    (when-not (.contains item-ids id)
      (let [ptag-ids (parse/parse-tags event-obj "p")]
        (when (accept-text-note? *state identity-pubkey ptag-ids event-obj)
          #_(log/debugf "Adding %s to observable list" id)
          (.add item-ids id)
          (.merge author-pubkey->item-id-set pubkey (HashSet. [id])
                  (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
          (let [init-idx (.size observable-list)
                init-note (domain/->UITextNoteNew event-obj created_at)]
            (.put item-id->index id init-idx)
            (.add observable-list init-note)))))))

#_
(defn thread-dispatch!
  [*state force-acceptance timeline identity-pubkey
   {:keys [id pubkey created_at content relays] :as event-obj}]
  {:pre [(some? pubkey)]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids]}
        timeline]
    (when-not (.contains item-ids id)
      (let [ptag-ids (parse/parse-tags event-obj "p")]
        (when (or force-acceptance
                  ;; Our item-id->index map will have a key for any id that
                  ;; has been referenced by any other accepted text note.
                  ;; So we also want to accept those "missing" notes:
                  (.containsKey item-id->index id)
                  (accept-text-note? *state identity-pubkey ptag-ids event-obj))
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
                (.add observable-list init-wrapper)))))))))

(defn add-item-to-thread-timeline!
  [timeline ptag-ids {:keys [id pubkey] :as event-obj}]
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids]}
        timeline]
    (.add item-ids id)
    (.merge author-pubkey->item-id-set pubkey (HashSet. [id])
            (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
    (let [etag-ids (parse/parse-tags event-obj "e") ;; order matters
          id-closure (cons id etag-ids)
          existing-index (first (keep #(.get item-id->index %) id-closure))]
      (if (some? existing-index)
        ;; We have a wrapper already. Update it with the new data about 'e' and 'p' tags.
        (let [curr-wrapper (.get observable-list existing-index)
              new-wrapper (timeline-support/contribute!
                           curr-wrapper event-obj etag-ids ptag-ids)]
          (doseq [x id-closure]
            (.put item-id->index x existing-index))
          (.set observable-list existing-index new-wrapper))
        ;; We don't have a wrapper yet. Create one and add it to the end of the observable
        ;; list.
        (let [init-index (.size observable-list)
              init-wrapper (timeline-support/init! event-obj etag-ids ptag-ids)]
          (doseq [x id-closure]
            (.put item-id->index x init-index))
          (.add observable-list init-wrapper))))))

(defn clear-timeline!
  [timeline]
  (doseq [property [:adapted-list
                    :observable-list
                    :author-pubkey->item-id-set
                    :item-id->index
                    :item-ids]]
    (.clear (property timeline))))

(defn reset-thread-timeline!
  "Clears the timeline and then adds the given event-obj."
  [timeline event-obj]
  (clear-timeline! timeline)
  (let [ptag-ids (parse/parse-tags event-obj "p")]
    (add-item-to-thread-timeline! timeline ptag-ids event-obj)))

(defn thread-dispatch!
  [*state timeline event-obj]
  {:pre [(some? (:pubkey event-obj))]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce
  (when-not (.contains (:item-ids timeline) (:id event-obj))
    (let [ptag-ids (parse/parse-tags event-obj "p")]
      (add-item-to-thread-timeline! timeline ptag-ids event-obj))))

(defn dispatch-text-note!
  "Dispatch a text note to all timelines for which the given event is relevant.
  If FORCE-ACCEPTANCE is true, we don't check if the note is acceptable. This is necessary
  for the threaded view."
  [*state force-acceptance {:keys [id pubkey created_at content relays] :as event-obj}]
  {:pre [(some? pubkey)]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce?
  (fx/run-later
   ;; Dispatch to flat timelines.
   #_(log/debugf "Dispatching text note for %d identities." (count (:identity->columns @*state)))
   (doseq [[identity-pubkey columns] (:identity->columns @*state)]
     #_(log/debugf "Dispatching text note for pubkey %s with %d columns"
                 identity-pubkey
                 (count columns))
     (doseq [column columns]
       #_(log/debugf "Considering dispatch for column %s" (:name (:view column)))
       (let [flat-timeline (:flat-timeline column)]
         (when (event-is-relevant-for-timeline? event-obj flat-timeline)
           (flat-dispatch! *state flat-timeline identity-pubkey event-obj)))))
   ;; Dispatch to thread timeline.
   (when-let [thread-timeline (:thread-timeline @*state)]
     (when force-acceptance
       (thread-dispatch! *state thread-timeline event-obj)))))

(defn update-active-timelines!
  "Update the active timelines for the identity with the given public key."
  [*state public-key] ;; note public-key may be nil!
  (fx/run-later
   (log/debugf "Updating active timelines for %s" public-key)
   ;; Update the listviews.
   (doseq [column (get (:identity->columns @*state) public-key)]
     (log/debugf "Setting items for listview for %s" (:name (:view column)))
     (.setItems (:flat-listview column)
                ^ObservableList (or (:adapted-list (:flat-timeline column))
                                    (FXCollections/emptyObservableList))))
   ;; Clear the thread timeline.
   (when-let [thread-timeline (:thread-timeline @*state)]
     (clear-timeline! thread-timeline))
   ;; Update the state's active public key.
   (swap! *state
          (fn [state]
            (assoc state :active-key public-key)))))


