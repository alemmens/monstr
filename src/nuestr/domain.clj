(ns nuestr.domain
  (:require
   [clojure.set :as set]
   [clojure.tools.logging :as log]
   [nuestr.util-java :as util-java])
  (:import
   (java.time ZonedDateTime Instant)
   (java.util.concurrent ThreadFactory Executors ScheduledExecutorService TimeUnit)
   (java.util HashMap HashSet UUID)
   (javafx.collections FXCollections ObservableList)
   (javafx.collections.transformation FilteredList)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: changes to active-key and mutations to home-ux, timelines must be done within a
;; mutex, i.e. on the fx thread!

(defn initial-state
  []
  {;; Dialog related
   :show-new-identity? false
   :show-add-column-dialog? false   
   :new-identity-error ""
   :active-reply-context nil  ; for the reply dialog
   ;; Identities and contacts
   :active-key nil           ; the public key of the active identity
   :identities []             ; sequence of Identity
   :identity-metadata {}      ; map from pubkey to ParsedMetadata
   :identity-active-contact {}   
   :contact-lists {}          ; pubkey -> ContactList
   ;; Relays
   :relays []                ; list of Relay
   :relays-sorted-by nil     ; for the Relays tab (one of nil, :url, :read?, :write?)
   :relay-search-text ""     ; string with search text in the Relays tab
   :connected-info {}
   ;; Views and columns
   :views {}                 ; map from view names to views
   :selected-view nil        ; the view name that is currently selected in the Views tab
   :temp-view nil            ; the view that's being edited in the Views tab
   :temp-view-changed? false ; if true, we activate the Save button in the Views tab
   :all-columns nil          ; a list with all columns
   :visible-column-ids nil   ; a list with the ids of the visible columns
   :new-timeline nil         ; relay url to be added to the visible timelines
   ;; Profiles
   :open-profile-states {}   ; a map from pubkeys (of open profile tabs) to ProfileState
   ;; Refresh
   :last-refresh nil         ; Java Instant indicating when the most recent refresh started
   ;; Status bar and debugging
   :status-message nil
   :status-message-timestamp nil ; seconds since start of epoch
   :debug-message nil
   })

(defonce *state
  (atom (initial-state)))

(defn relay-urls [state]
  (doall (map :url
              (filter #(or (:write? %) (:read? %))
                      (:relays state)))))

(defn all-relay-urls [state]
  (sort (map :url (:relays state))))

(defn columns [state]
  (:all-columns state))

(defn all-column-ids []
  (map :id (:all-columns @*state)))

(defn flat-timelines [state]
  (map :flat-timeline
       (vals (:identity->timeline-pair (columns state)))))

(defn thread-timelines [state]
  (map :thread-timeline
       (vals (:identity->timeline-pair (columns state)))))

(defn all-timelines [state]
  (concat (flat-timelines state) (thread-timelines state)))

(defn update-state! [keys new-value]
  (swap! *state update-in keys (constantly new-value)))

(defn find-view [name]
  (get (:views @*state) name))

(defn identity-name [id]
  (or (:name (get (:identity-metadata @*state) (:public-key id)))
      (:public-key id)))

(defn find-identity-by-pubkey [pubkey]
  (first (filter #(= (:public-key %) pubkey)
                 (:identities @*state))))

(defn active-identity []
  (find-identity-by-pubkey (:active-key @*state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Executor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^ScheduledExecutorService daemon-scheduled-executor
  (let [factory (reify ThreadFactory
                  (newThread [_ runnable]
                    (let [thread-name "nostr-desk-scheduled-executor-thread"]
                      (doto (Thread. runnable thread-name)
                        (.setDaemon true)))))]
    (Executors/newSingleThreadScheduledExecutor factory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Views and columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord View
    ;; A view defines what is shown in a column.
    [id            ; a random number
     name          ; a string
     show-pictures ; a boolean that indicates if pictures must be shown
     relay-urls    ; a set of relay urls
     follow        ; Either :use-identity \(i.e. follow the contacts of the active
                   ; identity) or :all \(i.e. global) or :use-list.
     follow-set    ; A set of (pubkeys of) authors the user wants to follow for this view.
                   ; Applicable if FOLLOW is :use-list.
     friends-of-friends ; Integer that indicates to which degree follows of follows must
                        ; also be followed. Default is 1, meaning only follows themselves.
     mute-authors  ; a set of (pubkeys of) authors to be muted
     words         ; a set of words, at least one of which must occur in the text note
     mute-words    ; a set of words to be muted
     channels      ; a set of (pubkeys of) Nostr channels (defined by kind 40 and 41 events)
     ])

(defrecord TimelinePair
    [flat-timeline
     thread-timeline
     flat-listview
     thread-listview])

(defn timeline [pair show-thread?]
  ((if show-thread? :thread-timeline :flat-timeline)
   pair))

(defrecord Column
    [id           ; a random UUID
     view
     identity->timeline-pair ; a map from identity pubkeys to TimelinePair records
     show-thread?
     thread-focus  ; The note (event-obj) that is the focus of the thread. Only relevant when showing a thread.
     missing-ids   ; A set with ids of events that are missing in the thread.
     found-ids     ; A set with ids of events that were missing but have been fetched from relays now.
     ])

(defn column-matches-relay-urls?
  "RELAY-URLS is a set of relay urls."
  [column relay-urls]
  (= (:relay-urls (:view column))
     relay-urls))

(defn find-column-by-id
  [id]
  (first (filter #(= (:id %) id)
                 (:all-columns @*state))))

(defn column-uses-view? [column view-name]
  (= (:name (:view column)) view-name))

(defn columns-using-view [view-name]
  (filter #(column-uses-view? % view-name)
          (:all-columns @*state)))

(defn find-column-with-view-name [view-name]
  (first (columns-using-view view-name)))

(defn follows-all? [column]
  (= (:follow (:view column))
     :all))

(defn update-view! [name property value]
  (swap! *state assoc-in
         [:views name property]
         value))

(defn make-view
  [name relay-urls
   {:keys [show-pictures follow follow-set friends-of-friends
           mute-authors words mute-words channels]}]
  (->View (rand-int 100000000)
          name
          (boolean show-pictures)
          relay-urls
          (or follow :use-identity)
          (or follow-set #{})
          (or friends-of-friends 1)
          (or mute-authors #{})
          (or words #{})
          (or mute-words #{})
          (or channels #{})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Timeline
  ;; these field values are only ever mutated on fx thread
  [^ObservableList adapted-list
   ^ObservableList observable-list ;; contains TextNoteWrapper
   ^HashMap author-pubkey->item-id-set
   ^HashMap item-id->index
   ^HashSet item-ids
   timeline-epoch-vol
   ])


(defn days-ago
  ^Instant [n]
  (-> (ZonedDateTime/now)
    (.minusDays n)
    .toInstant))

(defn new-timeline [thread?]
  ;; NOTE: we're querying and subscribing to all of time but for now, for ux
  ;; experience, we filter underlying data by n days
  ;; todo we'll really wish to query/subscribe at an epoch and only update it on scroll etc.
  (let [init-timeline-epoch (-> (days-ago 365) .getEpochSecond)
        timeline-epoch-vol (volatile! init-timeline-epoch)
        observable-list (FXCollections/observableArrayList)
        filtered-list (FilteredList. observable-list
                        (util-java/->Predicate #(> (:max-timestamp %) init-timeline-epoch)))
        adapted-list (if thread?
                       filtered-list
                       (.sorted filtered-list
                                ;; latest wrapper entries first:
                                (comparator #(< (:max-timestamp %2) (:max-timestamp %1)))))]
    (->Timeline
      adapted-list
      observable-list
      (HashMap.)
      (HashMap.)
      (HashSet.)
      timeline-epoch-vol)))

(defrecord Identity
    [public-key secret-key])

(defrecord Relay
    [url
     read?
     write?
     ;; A meta relay is a read relay that's only used for getting metadata.  Its websocket
     ;; connection will be closed as soon as there are no more subscriptions.
     meta?
     ])

(defn find-relay
  "Returns the Relay with the given `url` (or nil if no relay was found)."
  [url]
  (first (filter #(= (:url %) url) (:relays @*state))))

(defrecord ParsedContact
    [public-key main-relay-url petname])

(defrecord ContactList
    [pubkey created-at
     ;; Sequence of ParsedContact.
     parsed-contacts])

(defrecord ParsedMetadata
    [name about picture-url nip05-id created-at])

(defrecord Channel
    [id pubkey name about picture-url recommended-relay-url])

(defrecord TextNote
    [id pubkey content timestamp tags e-tags p-tags children missing?])

(defrecord TextNoteWrapper
    [max-timestamp ^TextNote root])

(defrecord TextNoteNew
    [event-obj max-timestamp depth])

(defrecord UIReplyContext
    [root-event-id event-id])

(defrecord ProfileState
    [id
     pubkey
     ;; If changed? is true, the Save button will be enabled.
     followers-changed?
     following-views-changed ; set of (names of) following views that have changed
     ;; A set of pubkeys (normally 0 or 1) of identities for which the profile's author is
     ;; / must be followed.
     followers
     ;; A set of view names for which the profile's author is / must be in the set of follows.
     following-views
     ;; Timelines
     timeline-pair
     show-thread?
     thread-focus  ; The note (event-obj) that is the focus of the thread. Only relevant when showing a thread.
     missing-ids   ; A set with ids of events that are missing in the thread.
     found-ids     ; A set with ids of events that were missing but have been fetched from relays now.
     ])

(defn new-profile-state [pubkey list-creator thread-creator]
  (let [followers (filter (fn [k]
                            (when-let [contact-list (get (:contact-lists @*state) k)]
                              ;; One of the contacts (i.e. follows) of K is the given
                              ;; pubkey, so K is one of pubkey's followers.
                              (some #(= pubkey (:public-key %))
                                    (:parsed-contacts contact-list))))
                          (map :public-key (:identities @*state)))
        following-views (map :name
                             (filter (fn [v] (get (:follow-set v) pubkey))
                                     (vals (:views @*state))))]
    (log/debugf "New profile state with following-views %s" (pr-str following-views))
    (->ProfileState (.toString (UUID/randomUUID))
                    pubkey
                    false
                    #{}
                    (set followers)
                    (set following-views)
                    (->TimelinePair (new-timeline false) (new-timeline true)
                                    (list-creator) (thread-creator))
                    false
                    nil
                    #{}
                    #{})))

(defn find-profile-state-by-id [id]
  (first (filter #(= (:id %) id)
                 (vals (:open-profile-states @*state)))))


;; --

; https://github.com/fiatjaf/nostr/blob/master/nips/01.md
;{
;  "ids": <a list of event ids>,
;  "kinds": <a list of kind numbers>,
;  "#e": <a list of event ids that are referenced in an "e" tag>,
;  "#p": <a list of pubkeys that are referenced in a "p" tag>,
;  "since": <a timestamp, events must be newer than this to pass>,
;  "until": <a timestamp, events must be older than this to pass>,
;  "authors": <a list of pubkeys, the pubkey of an event must be one of these>
;}
(defn ->subscription-filter
  [ids kinds e# p# since until authors]
  ;; directly serializable payload (note the #e and #p keys):
  {:ids ids :kinds kinds :#e e# :#p p# :since since :until until :authors authors})
