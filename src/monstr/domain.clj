(ns monstr.domain)

(defn initial-state
  []
  {:show-relays? false  ; indicates if the relays dialog must be shown
   :show-new-identity? false
   :new-identity-error ""
   :active-reply-context nil
   :identities [] ; [domain/Identity]
   :identity-metadata {} ; pubkey -> domain/ParsedMetadata
   :contact-lists {} ; pubkey -> domain/ContactList
   :identity-active-contact {}
   :relays [] ;; [domain/Relay]
   :connected-info {}
   ;; note: changes to active-key and mutations to home-ux, timelines
   ;;   must be done w/in mutex--ie on fx thread!
   :active-key nil ; the public key of the active identity
   :homes nil ; a map from sets of relay urls to Listviews
   :identity-timeline-new {} ; a map from identity pubkeys to lists of TimelineNew.
   })

(defn relay-urls [state]
  (doall (map :url (:relays state))))

;; --

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
