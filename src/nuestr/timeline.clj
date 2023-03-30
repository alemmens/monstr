(ns nuestr.timeline
  (:require [cljfx.api :as fx]
            [clojure.set :as set]
            [clojure.tools.logging :as log]
            [nuestr.domain :as domain]
            [nuestr.file-sys :as file-sys]
            [nuestr.metadata :as metadata]
            [nuestr.parse :as parse]
            [nuestr.relay-conn :as relay-conn]
            [nuestr.status-bar :as status-bar]
            [nuestr.store :as store]
            [nuestr.timeline-support :as timeline-support]
            [nuestr.util :as util]
            [nuestr.util-java :as util-java])
  (:import (java.util HashMap HashSet)
           (javafx.collections FXCollections ObservableList)
           (javafx.collections.transformation FilteredList)
           (javafx.scene.control ListView)))


(defn user-matches-event? [user-pubkey event-pubkey parsed-ptags]
  (or
   ;; The note was written by the user.
   (= event-pubkey user-pubkey)
   ;; The note's ptags reference the user.
   (some #(= % user-pubkey) parsed-ptags)))


(defn- accept-text-note?
  "COLUMN should be nil for timelines in profile tabs instead of columns."
  [*state column identity-pubkey parsed-ptags {:keys [pubkey] :as _event-obj}]
  (if column
    (let [view (:view column)]
      (case (:follow view)
        :all true
        :use-list (some #(= % pubkey)  ; only show notes where user is the author
                        (:follow-set view))
        :use-identity (or (user-matches-event? identity-pubkey pubkey parsed-ptags)
                          (let [contacts (:parsed-contacts (get (:contact-lists @domain/*state)
                                                                identity-pubkey))]
                            (some #(user-matches-event? % pubkey parsed-ptags)
                                  (map :public-key contacts))))))
    true))

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

(defn- event-is-relevant-for-column?
  "An event is relevant for a column if the column's relays have some overlap with the
  event's relays and if the column's filters also allow the event."
  [event timeline column]
  ;; TODO: Check pubkeys etc.
  #_(log/debugf "Checking relevance for column with view '%s', column relays %s and event relays %s"
                (:name (:view column))
                (:relay-urls (:view column))
                (:relays event))
  (not-empty (set/intersection (set (:relay-urls (:view column)))
                               (set (:relays event)))))



(defn flat-dispatch!
  [*state timeline identity-pubkey {:keys [id pubkey created_at content] :as event-obj} column]
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids]}
        timeline]
    #_
    (log/debugf "Flat dispatch of event %s for column %s with view %s"
                id
                (:id column)
                (:name (:view column)))
    (when-not (.contains item-ids id)
      (let [ptag-ids (parse/parse-tags event-obj "p")]
        (when (accept-text-note? *state column identity-pubkey ptag-ids event-obj)
          #_(log/debugf "Adding event %s '%s'" id content)
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
    (when-not (get item-ids id)
      #_(log/debugf "Adding %s to thread with %d items"
                    id
                    (count item-ids))
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
            #_(log/debugf "Updating wrapper at index %d" existing-index)
            (doseq [id id-closure]
              (.put item-id->index id existing-index))
            (.set observable-list existing-index new-wrapper))
          ;; We don't have a wrapper yet. Create one and add it to the end of the observable
          ;; list.
          (let [init-index (.size observable-list)
                init-wrapper (timeline-support/init! event-obj etag-ids ptag-ids)]
            #_(log/debugf "Created wrapper at index %s" init-index)
            (doseq [id id-closure]
              (.put item-id->index id init-index))
            (.add observable-list init-wrapper)))))))

(defn- update-column!
  [*state new-column]
  (let [active-key (:active-key @*state)
        columns (:all-columns @*state)]
    (swap! *state assoc
           :all-columns (conj (remove #(= (:id %) (:id new-column)) columns)
                              new-column))))

(defn- update-profile-state!
  [*state pubkey new-profile-state]
  #_(log/debugf "Updating profile state for %s" pubkey)
  (swap! *state assoc-in
         [:open-profile-states pubkey]
         new-profile-state))

(defn- clear-timeline-pair! [pair thread?]
  (let [listview ((if thread? :thread-listview :flat-listview) pair)
        timeline ((if thread? :thread-timeline :flat-timeline) pair)]
    ;; Clear the pair's timeline and listview    
    #_(log/debugf "Clearing listview %s and timeline %s" listview timeline)    
    (doseq [property [:observable-list :adapted-list :author-pubkey->item-id-set :item-id->index :item-ids]]
      (.clear (property timeline)))
    (.setItems listview (:adapted-list timeline))))
                     
  
(defn clear-column! [column thread?] 
  (log/debugf "Clearing column %s" (:id column))
  (doseq [pair (vals (:identity->timeline-pair column))]
    (clear-timeline-pair! pair thread?)))
                     
(defn- clear-column-thread!
  [*state column]
  (clear-column! column true))

(defn events-share-etags?
  [event-a event-b]
  (not-empty (set/intersection (set (cons (:id event-a) (parse/parse-tags event-a "e")))
                               (set (cons (:id event-b) (parse/parse-tags event-b "e"))))))

(defn thread-dispatch!
  [*state column profile-state event-obj check-relevance?]
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce
  (doseq [pair
          (if column
            (vals (:identity->timeline-pair column))
            (list (:timeline-pair profile-state)))]
    (let [timeline (:thread-timeline pair)]
      #_(log/debugf "Thread dispatch for column %s and profile-state %s"
                  (:id column)
                  (:id profile-state))
      (when-not (.contains (:item-ids timeline) (:id event-obj))
        (when (or (not check-relevance?)
                  ;; TODO: Try to find a more precise way to check if an event should be
                  ;; added to a thread.
                  (events-share-etags? event-obj (:thread-focus (or column profile-state))))
          #_(log/debugf "Definitely adding thread event %s for focus %s"
                      (:id event-obj)
                      (:thread-focus (or column profile-state)))
          (add-item-to-thread-timeline! timeline (parse/parse-tags event-obj "p") event-obj))))))

(defn- flat-dispatch-for-column [*state event-obj column]
  ;; TODO: We probably don't need to dispatch for all identities?
  #_(log/debugf "Dispatching event %s from %s to column %s with view '%s' and relays %s"
              (:id event-obj)
              (:relays event-obj)
              (:id column)
              (:name (:view column))
              (:relay-urls (:view column)))
  (doseq [[identity-pubkey pair] (:identity->timeline-pair column)]
    (let [timeline (:flat-timeline pair)]
      (when (event-is-relevant-for-column? event-obj timeline column)
        (flat-dispatch! *state timeline identity-pubkey event-obj column)))))
  
(defn dispatch-text-note!
  "Dispatch a text note to all timelines for which the given event is relevant.
  If COLUMN-ID is a string and THREAD? is true, the note is supposed to be for a thread
  view and it's only thread-dispatched to the specified column. If COLUMN-ID is a string
  and THREAD? is false, the note is flat-dispatched to the specified column. If COLUMN-ID
  is not a string, the note is dispatched to all columns."
  [*state column-id event-obj check-relevance? thread?]
  ;; CONSIDER if this is too much usage of on-fx-thread - do we need to batch/debounce?
  (fx/run-later
   (if (string? column-id)
     (let [column (domain/find-column-by-id column-id)]     
       (if thread?
         (thread-dispatch! *state column nil event-obj check-relevance?)
         (flat-dispatch-for-column *state event-obj column)))
     (doseq [column (:all-columns @*state)]
       (flat-dispatch-for-column *state event-obj column)))))

(defn- update-timeline-pair! [pair]
  (.setItems (:flat-listview pair)
             ^ObservableList (or (:adapted-list (:flat-timeline pair))
                                 (FXCollections/emptyObservableList)))
  (.setItems (:thread-listview pair)
             ^ObservableList (or (:adapted-list (:thread-timeline pair))
                                 (FXCollections/emptyObservableList))))
  
(defn update-column-timelines! [column]
  (log/debugf "Updating timelines for column %s with view '%s'" (:id column) (:name (:view column)))
  (doseq [[pubkey pair] (:identity->timeline-pair column)]
    (update-timeline-pair! pair)))


(defn add-profile-notes [pubkey]
  (fx/run-later
   (log/debugf "Adding profile notes for %s, relays=%s"
               pubkey
               (pr-str (domain/relay-urls @domain/*state)))
   (when-let [profile-state (get (:open-profile-states @domain/*state) pubkey)]
     (update-timeline-pair! (:timeline-pair profile-state))
     (doseq [r (domain/relay-urls @domain/*state)]
       (let [events (store/load-relay-events store/db r [pubkey])]
         (status-bar/message! (format "Loaded %d events for %s from %s from database"
                                      (count events)
                                      pubkey
                                      r))
         (doseq [e events]
           #_(log/debugf "Dispatching '%s' for %s" (:content e) pubkey)
           (flat-dispatch! domain/*state
                           (:flat-timeline (:timeline-pair profile-state))
                           pubkey
                           e
                           nil)))))))

(defn remove-open-profile-state! [pubkey]
  (swap! domain/*state util/dissoc-in
         [:open-profile-states pubkey]))

(defn maybe-add-open-profile-state!
  [pubkey]
  (when-not (get (:open-profile-states @domain/*state) pubkey)
    #_(log/debugf "Adding open-profile-state for %s" pubkey)
    ;; Use a trick to be able to refer to nuestr.view-home/create-list-view
    ;; without getting cyclical reference problems.
    (let [view-home (find-ns 'nuestr.view-home)
          list-creator (ns-resolve view-home (symbol "create-list-view"))
          thread-creator (ns-resolve view-home (symbol "create-thread-view"))]
    (swap! domain/*state assoc-in
           [:open-profile-states pubkey]
           (domain/new-profile-state pubkey
                                     (fn []
                                       (list-creator nil
                                                     domain/*state
                                                     store/db
                                                     metadata/cache
                                                     domain/daemon-scheduled-executor))
                                     (fn []
                                       (thread-creator nil pubkey
                                                       domain/*state
                                                       store/db
                                                       metadata/cache
                                                       domain/daemon-scheduled-executor))))
    (add-profile-notes pubkey))))

(defn update-active-timelines!
  "Update the active timelines for the identity with the given public key."
  [*state public-key] ;; note public-key may be nil!
  (fx/run-later
   (log/debugf "Updating active timelines for %s" public-key)
   ;; Update the listviews.
   (doseq [column (:all-columns @*state)]
     (update-column-timelines! column))
   ;; Update the state's active public key.
   (remove-open-profile-state! (:active-key @*state))
   (maybe-add-open-profile-state! public-key)
   (swap! *state assoc
          :active-key public-key)
   (file-sys/save-active-key)))


(defn show-column-thread!
  "If `column` is false, then we assume that the thread is in the profile tab for `pubkey`."
  [*state column pubkey event-obj]
  (fx/run-later
   (let [column-id (:id column)
         profile-state (get (:open-profile-states @*state) pubkey)
         root (parse/event-root event-obj)]
     #_(log/debugf "Show thread for event %s with root %s" event-obj root)
     ;; Clear the thread and update the :show-thread? and :thread-focus properties.
     (if column
       (do (clear-column-thread! *state column)
           (update-column! *state (assoc column
                                         :show-thread? true
                                         :thread-focus event-obj)))
       (do (clear-timeline-pair! (:timeline-pair profile-state) true)
           (update-profile-state! *state
                                  pubkey
                                  (assoc profile-state
                                         :show-thread? true
                                         :thread-focus event-obj))))
     ;; Load and dispatch all events for the thread.
     (let [events (cons (store/load-event store/db root) ; can be nil if root is not in db
                        (store/load-events-with-etag store/db root))]
       (log/debugf "Loaded %d thread events for root %s" (count events) root)
       (doseq [e (sort-by :created_at events)]
         (when e
           (thread-dispatch! *state column (get (:open-profile-states @*state) pubkey) e false)))
       ;; NOTE: we don't need to load recent events from relays here, because
       ;; that will be done by async-load-event! in view_home.clj.
       ))))

(defn- unshow-thread!
  [*state column pubkey]
  (if column
    (update-column! *state (assoc column
                                  :show-thread? false
                                  :thread-focus nil))
    (update-profile-state! *state
                           pubkey
                           (assoc (get (:open-profile-states @*state) pubkey)
                                  :show-thread? false
                                  :thread-focus nil))))



(defn back-from-thread-button
  [column pubkey]
  {:fx/type :button
   :padding 5
   :on-mouse-pressed (fn [e] (unshow-thread! domain/*state column pubkey))
   :text (str " " (char 0x2190) " ") ; left arrow
   })
