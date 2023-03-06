(ns nuestr.file-sys
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [nuestr.domain :as domain]
   )
  (:import (java.io File)))

(defn nostr-desk-dir
  ^File []
  (doto
    (->
      (System/getProperty "user.home")
      ;;!!!! note app logs configure to go in this dir too:
      (io/file ".nostr-desk"))
    io/make-parents
    .mkdir))

(defn db-path
  ^File []
  (io/file (nostr-desk-dir) "nd.db"))

(defn nuestr-file [filename-only]
  (io/file (nostr-desk-dir) filename-only))

(defn write-nuestr-data
  "Serializes an object to a nuestr file so it can be opened again later."
  [obj filename-only]
  (with-open [w (io/writer (nuestr-file filename-only))]
    (.write w (pr-str obj))))

(defn read-nuestr-data
  [filename-only]
  (edn/read-string (slurp (nuestr-file filename-only))))

(defn nuestr-file-exists? [filename-only]
  (.exists (nuestr-file filename-only)))

(defn record-to-map [record]
  (into {} record))

(defn save-views
  [views]
  (let [clean (into {}
                    (for [[name view] views]
                      [name (record-to-map view)]))]
    (write-nuestr-data clean "views.clj")))

(defn load-views
  "Returns nil if there is no view file."
  []
  (when (nuestr-file-exists? "views.clj")
    (let [raw (read-nuestr-data "views.clj")]
      (into {}
            (for [[name map] raw]
              [name (domain/map->View map)])))))
