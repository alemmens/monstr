(ns monstr.timeline
  (:require [cljfx.api :as fx]
            [clojure.set :as set]
            [clojure.tools.logging :as log]
            [monstr.domain :as domain]
            [monstr.parse :as parse]
            [monstr.relay-conn :as relay-conn]
            [monstr.store :as store]
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
   ])

(defn new-timeline
  [relays show-thread?]
  ;; NOTE: we're querying and subscribing to all of time but for now, for ux
  ;; experience, we filter underlying data by n days
  ;; todo we'll really wish to query/subscribe at an epoch and only update it on scroll etc.
  (let [init-timeline-epoch (-> (util/days-ago 1) .getEpochSecond)
        timeline-epoch-vol (volatile! init-timeline-epoch)
        observable-list (FXCollections/observableArrayList)
        filtered-list (FilteredList. observable-list
                        (util-java/->Predicate #(> (:max-timestamp %) init-timeline-epoch)))
        adapted-list (.sorted filtered-list
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
      show-thread?)))

(defn accept-text-note?
  [*state parsed-ptags {:keys [pubkey] :as _event-obj}]
  (let [identity-pubkey (:active-key @*state)
        {:keys [contact-lists]} @*state
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
  [*state {:keys [pubkey]}]
  (fx/run-later
   #_(log/debugf "Updating metadata for %d timelines" (count (domain/all-timelines @*state)))
   (doseq [timeline (domain/all-timelines @*state)]
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
  "An event is relevant for a timeline in a column if the timeline's relays have some
  overlap with the event's relays and if the column's filters also allow the event."
  [event timeline column]
  ;; TODO: Check pubkeys etc.
  (not-empty (set/intersection (set (:relays timeline))
                               (set (:relays event)))))

(defn flat-dispatch!
  [*state timeline {:keys [id pubkey created_at content] :as event-obj}]
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids]}
        timeline]
    (when-not (.contains item-ids id)
      (let [ptag-ids (parse/parse-tags event-obj "p")]
        (when (accept-text-note? *state ptag-ids event-obj)
          (.add item-ids id)
          (.merge author-pubkey->item-id-set
                  pubkey
                  (HashSet. [id])
                  (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
          (let [init-idx (.size observable-list)
                init-note (domain/->UITextNoteNew event-obj created_at)]
            (.put item-id->index id init-idx)
            (.add observable-list init-note)))))))

(defn- add-item-to-thread-timeline!
  [timeline ptag-ids {:keys [id pubkey] :as event-obj}] 
  #_(log/debugf "Adding event with id %s to thread timeline" id)
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

(defn- update-column!
  [*state new-column]
  (let [active-key (:active-key @*state)
        columns (:all-columns @*state)]
    (swap! *state assoc
           :columns (conj (remove #(= (:id %) (:id new-column)) columns)
                          new-column))))

(defn- clear-column-thread!
  [*state column]
  (let [listview (:thread-listview column)
        timeline (:thread-timeline column)]
    (log/debugf "Clearing listview %s and timeline %s" listview timeline)
    ;; Clear the column's thread timeline/listview
    (doseq [property [:observable-list :adapted-list :author-pubkey->item-id-set :item-id->index :item-ids]]
      (.clear (property timeline)))
    (.setItems listview (:adapted-list timeline))))

(defn events-share-etags?
  [event-a event-b]
  (not-empty (set/intersection (set (cons (:id event-a) (parse/parse-tags event-a "e")))
                               (set (cons (:id event-b) (parse/parse-tags event-b "e"))))))

(defn- thread-dispatch!
  [*state column event-obj check-relevance?]
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce
  (let [timeline (:thread-timeline column)]
    (when-not (.contains (:item-ids timeline) (:id event-obj))
      (when (or (not check-relevance?)
                ;; TODO: Try to find a more precise way to check if an event should be
                ;; added to a thread.
                (events-share-etags? event-obj (:thread-focus column)))
        (add-item-to-thread-timeline! timeline (parse/parse-tags event-obj "p") event-obj)))))

(defn dispatch-text-note!
  "Dispatch a text note to all timelines for which the given event is relevant.
  If COLUMN-ID is a string, the note is supposed to be for a thread view and
  it's only dispatched to the specified column."
  [*state column-id event-obj check-relevance?]
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce?
  (fx/run-later
   (if (string? column-id)
     (let [column (domain/find-column-by-id column-id)]
       (thread-dispatch! *state column event-obj check-relevance?))
     (doseq [column (:all-columns @*state)]
       (let [flat-timeline (:flat-timeline column)]
         (when (event-is-relevant-for-timeline? event-obj flat-timeline column)
           (flat-dispatch! *state flat-timeline event-obj)))))))

(defn update-active-timelines!
  "Update the active timelines for the identity with the given public key."
  [*state public-key] ;; note public-key may be nil!
  (fx/run-later
   (log/debugf "Updating active timelines for %s" public-key)
   ;; Update the listviews.
   (doseq [column (:all-columns @*state)]
     (.setItems (:flat-listview column)
                ^ObservableList (or (:adapted-list (:flat-timeline column))
                                    (FXCollections/emptyObservableList)))
     (.setItems (:thread-listview column)
                ^ObservableList (or (:adapted-list (:thread-timeline column))
                                    (FXCollections/emptyObservableList))))
   ;; Update the state's active public key.
   (swap! *state
          (fn [state]
            (assoc state :active-key public-key)))))

(defn- load-from-store [db event-id]
  (when-let [event (store/load-event db event-id)]
    (assoc event :relays (store/load-relays-for-event db event-id))))

(defn async-load-event!
  "Load the event with the given id from either the database or from the relays."
  [*state db column-id event-id]
  (if-let [event-from-store (load-from-store db event-id)]
    (dispatch-text-note! *state column-id event-from-store false)
    ;; Create a unique subscription id to load the event and subscribe to all relays in
    ;; the hope that we find the event.  We'll unsubscribe automatically when we get an
    ;; EOSE event.
    (let [subscription-id (format "monstr:%s:%s" column-id (rand-int 1000000000))]
      (relay-conn/subscribe-all! subscription-id
                                 [(domain/->subscription-filter
                                   [event-id] [1] nil nil nil nil nil)]))))

(defn refresh-thread-events!
  [*state column-id]
  (let [timestamp (:thread-refresh-timestamp @*state)
        events (store/load-events-since store/db timestamp)]
    (doseq [e events]
      (dispatch-text-note! *state column-id e true))
    ;; TODO: Also load recent events from relays.
    ))

  
(defn show-column-thread!
  [*state column event-obj]
  (fx/run-later  
   (let [column-id (:id column)]
     ;; Clear the column's thread.
     (clear-column-thread! *state column)
     ;; Update the :show-thread? and :thread-focus properties of the column.    
     (update-column! *state (assoc column
                                   :show-thread? true
                                   :thread-focus event-obj))
     ;; Add the given event-obj.
     (let [ptag-ids (parse/parse-tags event-obj "p")]
       (add-item-to-thread-timeline! (:thread-timeline (domain/find-column-by-id column-id))
                                     ptag-ids event-obj))
     ;; Refresh events to find any children.
     (when-not (:thread-refresh-timestamp @*state)
       (swap! *state assoc
              :thread-refresh-timestamp (:created_at event-obj))
       (refresh-thread-events! *state column-id)
       (swap! *state dissoc :thread-refresh-timestamp)))))

(defn unshow-column-thread!
  [*state column]
  (update-column! *state (assoc column
                                :show-thread? false
                                :thread-focus nil)))

  
