(ns monstr.domain)

(defn initial-state
  []
  {:show-relays? false        ; indicates if the relays dialog must be shown
   :show-new-identity? false
   :new-identity-error ""
   :active-reply-context nil  ; for the reply dialog
   :identities []             ; sequence of Identity
   :identity-metadata {}      ; map from pubkey to ParsedMetadata
   :contact-lists {}          ; pubkey -> ContactList
   :identity-active-contact {}
   :relays []                 ; list of Relay
   :connected-info {}
   ;; note: changes to active-key and mutations to home-ux, timelines
   ;;   must be done w/in mutex--ie on fx thread!
   :active-key nil ; the public key of the active identity
   :show-add-timeline-dialog? false
   :new-timeline nil       ; relay url to be added to the visible timelines
   :relay-timelines []     ; sequence with the relay urls of the visible timelines   
   :homes nil              ; map from sets of relay urls to Listviews
   :identity->columns {}   ; map from identity pubkeys to lists of Column
   })

(defonce *state
  (atom (initial-state)))

(defn relay-urls [state]
  (doall (map :url (:relays state))))

;; --

(defrecord View
    ;; A view defines what is shown in a column.
    ;; TODO: Add more ways to define a view: hash tags, followed pubkeys, etc.
    [name         ; a string
     relay-urls   ; a list of relay urls
     ])

(defrecord Column
    [view
     flat-timeline
     thread-timeline
     show-thread?])
  
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
  [id pubkey #_... content timestamp tags e-tags p-tags children missing?])

(defrecord UITextNoteWrapper
  [loom-graph expanded? note-count max-timestamp ^UITextNote root])

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
