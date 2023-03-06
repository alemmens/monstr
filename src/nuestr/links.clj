(ns nuestr.links
  (:require [clojure.tools.logging :as log]
            [clojure.string :as str])
  (:import (java.util.regex Matcher)))

(def ^:private http-basic-regex-str
  #"(https?)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]")

(defn detect
  [^String content]
  (try
    (let [m (re-matcher http-basic-regex-str content)
          result-vol (volatile! [])]
      (while (.find m)
        (let [m-start (.start m) m-end (.end m)]
          (when (or (empty? @result-vol)
                  ;; no overlapping--
                  (>= m-start (second (peek @result-vol))))
            (vswap! result-vol conj [m-start m-end]))))
      @result-vol)
    (catch Exception e
      [])))

(def ^:private nostr-tag-regex-str
  #"\#\[(\d+)\]")

(defn detect-nostr-tags
  ;; answers [start end int-value]
  [^String content]
  (try
    (let [m (re-matcher nostr-tag-regex-str content)
          result-vol (volatile! [])]
      (while (.find m)
        (let [m-start (.start m) m-end (.end m) m-group (Integer/parseInt (.group m 1))]
          (when (or (empty? @result-vol)
                  ;; no overlapping--
                  (>= m-start (second (peek @result-vol))))
            (vswap! result-vol conj [m-start m-end m-group]))))
      @result-vol)
    (catch Exception e
      [])))
