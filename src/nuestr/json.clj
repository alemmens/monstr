(ns monstr.json
  (:require [jsonista.core :as json]))

(def json-mapper
  (json/object-mapper
    {:encode-key-fn name
     :decode-key-fn keyword}))

(defn parse
  [payload]
  (json/read-value payload json-mapper))

(defn write-str*
  ^String [o]
  (json/write-value-as-string o json-mapper))
