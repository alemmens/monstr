(ns nuestr.parse
  (:require [nuestr.domain :as domain]
            [nuestr.json :as json]))

(defn raw-event-tuple->event-obj
  [raw-event-tuple]
  (-> raw-event-tuple json/parse (nth 2)))

(defn parse-contacts*
  [{:keys [tags] :as _event-obj}]
  (->> tags
    (filter #(= "p" (first %)))
    (mapv (fn [[_ arg0 arg1 arg2]]
            (domain/->ParsedContact arg0 arg1 arg2)))))

(defn parse-tags
  [{:keys [tags] :as _event-obj} tag-str]
  ;; note: result order should match tag order
  (->> tags
       (filter #(= tag-str (first %)))
       ;; TODO: THIS IS TOO SIMPLE. FOR SOME TAGS WE CAN NOW
       ;; HAVE MORE THAN ONE VALUE.
       (mapv second)))

(defn e-tags [event]
  (parse-tags event "e"))

(defn p-tags [event]
  (parse-tags event "p"))

(defn new-etags
  "Returns a sequence of e-tags, where each e-tag is a vector
  that starts with the string 'e', followed by an event id,
  optionally followed by a relay url, optionally followed by
  a marker ('reply', 'root', or 'mention'). See NIP 10.

  Example result:
   ([\"e\"
   \"ee11d86dfa871f363886b95eacb7e60c599e87021cbfcbf007e92f34bac9ed9e\"
   \"wss://relay.damus.io\"
   \"root\"]
  [\"e\"
   \"8a384db6e13368e966d93f3b021b2e98113f1a7c34e34a22dbf0887f5312c654\"
   \"wss://nostr-pub.wellorder.net\"
   \"reply\"])
  "
  [event]
  (filter #(= "e" (first %))
          (:tags event)))

(defn relay-hint [etag]
  (get etag 2))

(defn relay-hints [event]
  (remove empty? (map relay-hint (new-etags event))))

(defn event-has-relay-hints? [event]
  (not (empty? (relay-hints event))))

(defn event-root
  "Returns the event id of the root event associated with an event."
  [event]
  ;; For now we assume that the root is the first e-tag.
  ;; Note that NIP 10 deprecates this assumption, but current relays don't seem to implement
  ;; NIP 10 yet. TODO: MAKE THIS WORK FOR BOTH PRE AND POST NIP 10.
  (or (first (e-tags event))
      (:id event)))
