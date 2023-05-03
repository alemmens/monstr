(ns nuestr.timeline
  (:require [cljfx.api :as fx]
            [clojure.set :as set]
            [clojure.tools.logging :as log]
            [nuestr.domain :as domain]
            [nuestr.file-sys :as file-sys]
            [nuestr.metadata :as metadata]
            [nuestr.modal :as modal]
            [nuestr.parse :as parse]
            [nuestr.relay-conn :as relay-conn]
            [nuestr.status-bar :as status-bar]
            [nuestr.store :as store]
            [nuestr.timeline-support :as timeline-support]
            [nuestr.util :as util]
            [nuestr.util-java :as util-java])
  (:import (java.util HashMap HashSet)
           (java.util.concurrent ConcurrentLinkedQueue PriorityBlockingQueue)
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

(defn timeline-pairs [column-id pubkey]
  (if-let [column (domain/find-column-by-id column-id)]
    (vals (:identity->timeline-pair column))
    (list (:timeline-pair (get (:open-profile-states @domain/*state) pubkey)))))

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

(defn add-event-to-timeline! [timeline {:keys [id pubkey created_at content] :as event} depth]
  #_(log/debugf "Adding event %s '%s'" id content)
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids
                max-size
                queue]}
        timeline]
    (cond (.contains item-ids id)
          false
          (<= (.size observable-list) @max-size)
          ;; If there's room on the timeline, add the event immediately.
          (do (.add item-ids id)
              (.merge author-pubkey->item-id-set
                      pubkey
                      (HashSet. [id])
                      (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
              (let [index (.size observable-list)
                    note (domain/->TextNoteNew event created_at depth)]
                (.put item-id->index id index)
                (.add observable-list note))
              true)
          :else
          ;; Otherwise add it to the queue, so it can be added later.
          (do (domain/add-or-update-event-in-queue queue event)
              true))))

(defn grow-timeline! [column-id pubkey]
  ;; This is called when the user scrolls to the bottom of a timeline listview.  We
  ;; increase the max size of the timeline and move events from the waiting queue to the
  ;; timeline.
  (doseq [pair (timeline-pairs column-id pubkey)]
    (when-let [timeline (:flat-timeline pair)]
      (log/debugf "Growing timeline %s, column %s, pubkey %s" timeline column-id pubkey)
      (when (<= @(:max-size timeline)
                (.size ^ObservableList (:observable-list timeline)))
        ;; Increase the timeline's max size.          
        (swap! (:max-size timeline) + domain/max-timeline-size-increment)          
        (fx/run-later
         (let [^PriorityBlockingQueue q (:queue timeline)]
           ;; And move events from the queue to the timeline.
           (loop [i 0]
             (when-not (or (domain/event-queue-is-empty? q) (>= i domain/max-timeline-size-increment))
               (do (let [event (domain/event-queue-poll q)
                         added? (add-event-to-timeline! timeline event 0.0)]
                     (recur (if added? (inc i) i))))))))))))

(defn flat-dispatch!
  [*state timeline identity-pubkey {:keys [id pubkey created_at content] :as event-obj} column]
  (let [{:keys [^ObservableList observable-list
                ^HashMap author-pubkey->item-id-set
                ^HashMap item-id->index
                ^HashSet item-ids]}
        timeline]
    (when-not (.contains (:item-ids timeline) id)
      (let [ptag-ids (parse/p-tags event-obj)]
        (when (accept-text-note? *state column identity-pubkey ptag-ids event-obj)
          (add-event-to-timeline! timeline event-obj 0.0))))))

(defn print-column [c]
  (format "ID %s, show-thread? %s, thread-focus %s, missing ids %s, found-ids %s"
          (:id c) (:show-thread? c) (:thread-focus c) (:missing-ids c) (:found-ids c)))

(defn- update-column!
  [*state new-column]
  #_(status-bar/debug! (format "Updating column to %s" (print-column new-column)))
  ;; Make sure we get the most recent updates by 'reloading' the column and profile-state.
  (let [columns (:all-columns @*state)]
    (swap! *state assoc
           :all-columns (conj (remove #(= (:id %) (:id new-column)) columns)
                              new-column))))

(defn- update-profile-state!
  [*state pubkey new-profile-state]
  #_(log/debugf "Updating profile state for %s" pubkey)
  (swap! *state assoc-in
         [:open-profile-states pubkey]
         new-profile-state))

(defn- update-thread-info! [*state column profile-state map]
  (if column
    (update-column! *state (merge column map))
    (let [p (domain/find-profile-state-by-id (:id profile-state))]
      (update-profile-state! *state (:pubkey p) (merge p map)))))

(defn thread-property [*state column profile-state key]
  ;; Make sure we get the most recent updates by 'reloading' the column and profile-state.
  (let [c (domain/find-column-by-id (:id column))
        p (domain/find-profile-state-by-id (:id profile-state))]
    (get (or c p) key)))

(defn- clear-timeline-pair! [pair thread?]
  (let [listview ((if thread? :thread-listview :flat-listview) pair)
        timeline ((if thread? :thread-timeline :flat-timeline) pair)]
    ;; Clear the pair's timeline and listview    
    #_(log/debugf "Clearing listview %s and timeline %s" listview timeline)    
    (doseq [property [:observable-list :adapted-list :author-pubkey->item-id-set
                      :item-id->index :item-ids]]
      (.clear (property timeline)))
    (.clear (:id->event (:queue timeline)))
    (.clear (:queue (:queue timeline)))
    (reset! (:max-size timeline) domain/initial-max-timeline-size)
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
  (not-empty (set/intersection (set (cons (:id event-a) (parse/e-tags event-a)))
                               (set (cons (:id event-b) (parse/e-tags event-b))))))

(declare refresh-column-thread!)

(defn thread-dispatch!
  [*state column profile-state event]
  (let [id (:id event)]
    ;; Add the event id to the set of found ids if it was missing.
    (when (get (thread-property column profile-state :missing-ids) id)
      (update-thread-info! *state column profile-state
                           {:found-ids (conj (thread-property column profile-state :found-ids)
                                             id)}))
    (refresh-column-thread! column profile-state)))

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
  If COLUMN-ID is a string, the note is flat-dispatched to the specified column. Otherwise
  the note is dispatched to all columns."
  [*state column-or-profile-id event-obj profile?]
  ;; CONSIDER if this is too much usage of on-fx-thread - do we need to batch/debounce?
  (fx/run-later
   (if profile?
     (when-let [profile-state (domain/find-profile-state-by-id column-or-profile-id)]
       (flat-dispatch! domain/*state
                       (:flat-timeline (:timeline-pair profile-state))
                       (:pubkey profile-state)
                       event-obj
                       nil))
     (if (string? column-or-profile-id)
       (let [column (domain/find-column-by-id column-or-profile-id)]     
         (flat-dispatch-for-column *state event-obj column))
       (doseq [column (:all-columns @*state)]
         (flat-dispatch-for-column *state event-obj column))))))

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


(defn- get-profile-events [pubkey]
  (fx/run-later
   (log/debugf "Getting profile events for %s, relays=%s"
               pubkey
               (pr-str (domain/relay-urls @domain/*state)))
   (when-let [profile-state (get (:open-profile-states @domain/*state) pubkey)]
     (update-timeline-pair! (:timeline-pair profile-state))
     (doseq [r (domain/relay-urls @domain/*state)]
       (let [events (store/load-relay-events store/db r [pubkey])]
         (doseq [e events]
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
                                         (list-creator nil pubkey
                                                       domain/*state
                                                       store/db
                                                       metadata/cache
                                                       domain/daemon-scheduled-executor))
                                       (fn []
                                         (thread-creator nil pubkey
                                                         domain/*state
                                                         store/db
                                                         metadata/cache
                                                         domain/daemon-scheduled-executor)))))
    (get-profile-events pubkey)
    (util/submit! domain/daemon-scheduled-executor
                  (fn []
                    (let [subscription-id (format "profile:%s:%s"
                                                  (:id (get (:open-profile-states @domain/*state) pubkey))
                                                  (rand-int 1000000000))]
                      (relay-conn/subscribe-all! subscription-id
                                                 [{:authors [pubkey] :kinds [0 1] :limit 5000}]
                                                 #(or (:read? %) (:meta? %))))))))


(defn open-profile [ui-event pubkey]
  ;; Add a new profile tab.
  (maybe-add-open-profile-state! pubkey)
  ;; Select the tab that we just added.  
  (fx/run-later ; run later, otherwise the new tab doesn't exist yet
   (let [scene (.getScene (.getSource ui-event))
         tab-pane (.lookup scene "#nuestr-tabs")
         index (+ 4 ; HACK: 4 is the number of tabs before the first Profile tab.
                  (util/position pubkey (keys (:open-profile-states @domain/*state))))]
     (.select (.getSelectionModel tab-pane) index))))

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

(defn fetch-events-with-ids [column-id pubkey ids]
  ;; Create a unique subscription id to fetch all events with the given ids and
  ;; subscribe to all relays in the hope that we find the events.  We'll unsubscribe
  ;; automatically when we get an EOSE event.
  (when (seq ids)
    (let [profile-state (get (:open-profile-states @domain/*state) pubkey)
          subscription-id (format "%s:%s:%s"
                                  (if column-id "thread" "pt")  ; pt = profile thread
                                  (or column-id (:id profile-state))
                                  (rand-int 1000000000))]
      ;; TODO: Don't subscribe to all relays, but try to use only relevant relays.
      #_(status-bar/debug! (format "Fetching %d ids %s" (count ids) subscription-id))
      (relay-conn/subscribe-all! subscription-id
                                 [{:ids ids :kinds [1]}]
                                 #(or (:read? %) (:meta? %))))))

(defn connect-note-to-listview! [text-note depth id->event timeline listview]
  (let [id (:id text-note)]
    (when-not (get (:item-ids timeline) id)
      (add-event-to-timeline! timeline (get id->event id) depth)
      (doseq [child (sort-by :timestamp (:children text-note))]
        (connect-note-to-listview! child (inc depth) id->event timeline listview)))))

(defn connect-wrappers-to-listview! [wrappers id->event column-id pubkey]
  (doseq [pair (timeline-pairs column-id pubkey)]
    (let [timeline (:thread-timeline pair)
          listview (:thread-listview pair)]
      (doseq [wrapper wrappers]
        (connect-note-to-listview! (:root wrapper) 0.0 id->event timeline listview)))))

(defn load-thread-events
  "Tries to load all events in the thread of which `ids` are a part.
  Returns a vector with the loaded events (including missing events, i.e.
  events that could not be found in the database).
  `ids` and `existing-ids` are sets."
  [ids existing-ids]
  (let [direct-events (store/load-events store/db (seq ids))
        missing-event-ids (set/difference ids (set (map :id direct-events)))
        missing-events (map (fn [id]
                              {:id id
                               :missing? true
                               :tags ()
                               :created_at (.getEpochSecond (domain/days-ago 10)) ; just an arbitrary date in the past
                               :content (format "Missing %s" (util/format-pubkey-short id))})
                            missing-event-ids)
        referenced-events (store/load-events-with-etags store/db (seq ids))
        events (concat direct-events referenced-events missing-events)
        e-tags (set (mapcat parse/e-tags events))
        new-existing-ids (set/union ids existing-ids)
        new-ids (set/difference e-tags new-existing-ids)]
    (if (seq new-ids)
      ;; Keep loading events until we've reached closure.      
      (let [new-events (load-thread-events new-ids new-existing-ids)]
        (distinct (concat new-events events)))
      ;; We have all needed events. Return them.
      events)))

(defn select-thread-focus [thread-focus column-id pubkey]
  (doseq [pair (timeline-pairs column-id pubkey)]
    (let [timeline (:thread-timeline pair)
          listview ^ListView (:thread-listview pair)
          item-id->index (:item-id->index timeline)
          item-id (:id thread-focus)]
      (when-let [index (.get item-id->index item-id)]
        (let [item (.get (:observable-list timeline) index)]
          ;; Scroll to the thread focus item and select it.
          (.scrollTo ^ListView listview
                     ;; Index is a java.long.Integer but scrollTo
                     ;; seems to need a long or it won't do anything!
                     (long index))
          (.select (.getSelectionModel listview) (long index)))))))



(defn show-column-thread!
  "If `column` is false, then we assume that the thread is in the profile tab for `pubkey`."
  [*state column pubkey event-obj]
  (fx/run-later
   (let [column-id (:id column)
         profile-state (get (:open-profile-states @*state) pubkey)]
     #_
     (status-bar/debug! (format "Show thread for event %s" (:id event-obj)))
     ;; Clear the thread and update the :show-thread? and :thread-focus properties.
     (if column
       (clear-column-thread! *state column)
       (clear-timeline-pair! (:timeline-pair profile-state) true))
     (update-thread-info! *state (domain/find-column-by-id column-id) profile-state
                          {:show-thread? true
                           :thread-focus event-obj})
     ;; Load and dispatch all events for the thread.
     (let [events (load-thread-events (set [(:id event-obj)])
                                      #{})
           missing-ids (set (map :id (filter :missing? events)))
           old-missing-ids (thread-property *state column profile-state :missing-ids)
           new-missing-ids (set/difference missing-ids old-missing-ids missing-ids)]
       (update-thread-info! *state (domain/find-column-by-id column-id) profile-state
                            {:missing-ids missing-ids
                             :found-ids #{}})       
       (when (seq new-missing-ids)
         ;; We discovered new missing events (presumably because they are referenced by
         ;; events that we found in the previous round), so we try to fetch them from the
         ;; relays.
         (util/submit! domain/daemon-scheduled-executor ; get off of fx thread
                       (fn [] (fetch-events-with-ids column-id pubkey new-missing-ids))))
       (let [[wrappers id->node id->event] (timeline-support/build-text-note-wrappers events)]
         #_
         (status-bar/debug! (format "Loaded %d events, %d wrappers, %s notes, %d nodes, show?:%s"
                                    (count events)
                                    (count wrappers)
                                    (pr-str (map timeline-support/tree-size wrappers))
                                    (count id->node)
                                    (thread-property *state column profile-state :show-thread?)
                                    ))
         (connect-wrappers-to-listview! wrappers id->event column-id pubkey)
         (select-thread-focus event-obj column-id pubkey))))))

(defn find-event-with-id
  "Returns nil if the event can't be found."
  [id]
  (or (store/load-event store/db id)
      ;; TODO: try to fetch the event from relays.
      nil))

(defn refresh-column-thread!
  [*state column pubkey]
  (let [thread-focus (thread-property *state column pubkey :thread-focus)]
    (show-column-thread! *state column pubkey thread-focus)
    (update-thread-info! *state column
                         (get (:open-profile-states @*state) pubkey)
                         {:found-ids #{}})))

(defn- unshow-thread!
  [*state column pubkey]
  (update-thread-info! *state column
                       (get (:open-profile-states @*state) pubkey)
                       {:show-thread? false
                        :thread-focus nil
                        :missing-ids #{}
                        :found-ids #{}}))

(defn back-from-thread-button
  [column pubkey]
  {:fx/type :button
   :padding 5
   :on-mouse-pressed (fn [e] (unshow-thread! domain/*state column pubkey))
   :text (str " " (char 0x2190) " ") ; left arrow
   })
