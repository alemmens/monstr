(ns me.untethr.nostr.parse
  (:require [me.untethr.nostr.domain :as domain]
            [me.untethr.nostr.json :as json]))

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
