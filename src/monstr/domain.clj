(ns monstr.domain
  (:require
   [clojure.set :as set]))

;; NOTE: changes to active-key and mutations to home-ux, timelines must be done within a
;; mutex, i.e. on the fx thread!

(defn initial-state
  []
  {;; Dialog related
   :show-relays? false        ; indicates if the relays dialog must be shown
   :show-new-identity? false
   :show-add-timeline-dialog? false   
   :new-identity-error ""
   :active-reply-context nil  ; for the reply dialog
   ;; Identities and contacts
   :active-key nil           ; the public key of the active identity
   :identities []             ; sequence of Identity
   :identity-metadata {}      ; map from pubkey to ParsedMetadata
   :identity-active-contact {}   
   :contact-lists {}          ; pubkey -> ContactList
   ;; Relays
   :relays []                 ; list of Relay
   :connected-info {}
   ;; Views and columns
   :views {}                 ; map from view names to views
   :selected-view nil        ; the view name that is currently selected in the Views tab
   :temp-view nil            ; the view that's being edited in the Views tab
   :temp-view-changed? false ; if true, we activate the Save button in the Views tab
   :all-columns nil          ; a list with all columns
   :visible-column-ids nil   ; a list with the ids of the visible columns
   :new-timeline nil         ; relay url to be added to the visible timelines
   ;; Refresh
   :last-refresh nil         ; Java Instant indicating when the most recent refresh started
   ;; Status bar
   :status-message nil
   :status-message-timestamp nil ; seconds since start of epoch
   })

(defonce *state
  (atom (initial-state)))

(defn relay-urls [state]
  (doall (map :url (:relays state))))

(defn columns [state]
  (:all-columns state))

(defn flat-timelines [state]
  (map :flat-timeline (columns state)))

(defn thread-timelines [state]
  (map :thread-timeline (columns state)))

(defn all-timelines [state]
  (concat (flat-timelines state) (thread-timelines state)))

(defn update-state! [keys new-value]
  (swap! *state update-in keys (constantly new-value)))

(defn find-view [name]
  (get (:views @*state) name))

;; --

(defrecord View
    ;; A view defines what is shown in a column.
    [name         ; a string
     relay-urls   ; a set of relay urls
     follow       ; either :use-identity \(i.e. follow the contacts of the active
                  ; identity) or :all \(i.e. global) or a set of (pubkeys of) authors the
                  ; user wants to follow for this view
     friends-of-friends? ; Integer that indicates to which degree follows of follows must
                         ; also be followed. Default is 1, meaning only follows themselves.
     mute-authors ; a set of (pubkeys of) authors to be muted
     words        ; a set of words, at least one of which must occur in the text note
     mute-words   ; a set of words to be muted
     channels     ; a set of (pubkeys of) Nostr channels (defined by kind 40 and 41 events)
     ])

(defn make-view
  [name relay-urls
   {:keys [follow friends-of-friends? mute-authors words mute-words channels]}]
  (->View name
          relay-urls
          (or follow :use-identity)
          (or friends-of-friends? 1)
          (or mute-authors #{})
          (or words #{})
          (or mute-words #{})
          (or channels #{})))

(defrecord Column
    [id           ; a random UUID
     view
     flat-timeline
     thread-timeline
     flat-listview
     thread-listview
     show-thread?
     thread-focus  ; The note (event-obj) that is the focus of the thread. Only relevant when showing a thread.
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

(defrecord Identity
  [public-key secret-key])

(defrecord Relay
  [url read? write?])

(defrecord ParsedContact
  [public-key main-relay-url petname])

(defrecord ContactList
  [pubkey created-at parsed-contacts])

(defrecord ParsedMetadata
  [name about picture-url nip05-id created-at])

(defrecord UITextNote
  [id pubkey content timestamp tags e-tags p-tags children missing?])

(defrecord UITextNoteWrapper
  [loom-graph note-count max-timestamp ^UITextNote root])

(defrecord UITextNoteNew
  [event-obj max-timestamp])

(defrecord UIReplyContext
  [root-event-id event-id])

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
