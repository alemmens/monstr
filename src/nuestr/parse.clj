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
    (mapv second)))

(defn e-tags [event]
  (parse-tags event "e"))

(defn p-tags [event]
  (parse-tags event "p"))

(defn event-root
  "Returns the event id of the root event associated with an event."
  [event]
  ;; For now we assume that the root is the first e-tag.
  ;; Note that NIP 10 deprecates this assumption, but current relays don't seem to implement
  ;; NIP 10 yet. TODO: MAKE THIS WORK FOR BOTH PRE AND POST NIP 10.
  (or (first (e-tags event))
      (:id event)))
