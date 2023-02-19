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
  "An event is relevant for a timeline if the timeline's relays have some overlap with the
   event's relays."
  [event timeline]
  (not-empty (set/intersection (set (:relays timeline))
                               (set (:relays event)))))

(defn flat-dispatch!
  [*state timeline identity-pubkey
   {:keys [id pubkey created_at content] :as event-obj}]
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids]}
        timeline]
    (when-not (.contains item-ids id)
      (let [ptag-ids (parse/parse-tags event-obj "p")]
        (when (accept-text-note? *state identity-pubkey ptag-ids event-obj)
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
          #_(log/debugf "Adding new wrapper for %s to index %s" id init-index)
          (doseq [x id-closure]
            (.put item-id->index x init-index))
          (.add observable-list init-wrapper))))))

(defn- update-column!
  [*state new-column]
  (let [active-key (:active-key @*state)
        identity->columns (:identity->columns @*state)
        columns (get identity->columns active-key)]
    (swap! *state assoc
           :identity->columns (assoc identity->columns active-key
                                     (conj (remove #(= (:id %) (:id new-column)) columns)
                                           new-column)))))

(defn- clear-column-thread!
  [*state column]
  (let [listview (:thread-listview column)
        timeline (:thread-timeline column)]
    (log/debugf "Clearing listview %s and timeline %s" listview timeline)
    ;; Clear the column's thread timeline/listview
    (doseq [property [:observable-list :adapted-list :author-pubkey->item-id-set :item-id->index :item-ids]]
      (.clear (property timeline)))
    (.setItems listview (:adapted-list timeline))))
  
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
                                     ptag-ids event-obj)))))

(defn unshow-column-thread!
  [*state column]
  (update-column! *state (assoc column
                                :show-thread? false
                                :thread-focus nil)))

  
(defn- thread-dispatch!
  [*state column event-obj]
  {:pre [(some? (:pubkey event-obj))]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce
  (let [timeline (:thread-timeline column)]
    (log/debugf "Thread dispatch to column %s with timeline %s" column timeline)
    (when-not (.contains (:item-ids timeline) (:id event-obj))
      (let [ptag-ids (parse/parse-tags event-obj "p")]
        (add-item-to-thread-timeline! timeline ptag-ids event-obj)))))

(defn dispatch-text-note!
  "Dispatch a text note to all timelines for which the given event is relevant.
  If COLUMN-ID is a string, the note is supposed to be for a thread view and
  it's only dispatched to the specified column."
  [*state column-id event-obj]
  {:pre [(some? (:pubkey event-obj))]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce?
  (fx/run-later
   (if (string? column-id)   
     (do (log/debugf "Dispatching to column %s" column-id)
         (thread-dispatch! *state (domain/find-column-by-id column-id) event-obj))
     (doseq [[identity-pubkey columns] (:identity->columns @*state)]
       (doseq [column columns]
         (let [flat-timeline (:flat-timeline column)]
           (when (event-is-relevant-for-timeline? event-obj flat-timeline)
             (flat-dispatch! *state flat-timeline identity-pubkey event-obj))))))))

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
                                    (FXCollections/emptyObservableList)))
     (.setItems (:thread-listview column)
                ^ObservableList (or (:adapted-list (:thread-timeline column))
                                    (FXCollections/emptyObservableList))))
   ;; Update the state's active public key.
   (swap! *state
          (fn [state]
            (assoc state :active-key public-key)))))


