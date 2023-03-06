(ns monstr.file-sys
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [monstr.domain :as domain]
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

(defn monstr-file [filename-only]
  (io/file (nostr-desk-dir) filename-only))

(defn write-monstr-data
  "Serializes an object to a monstr file so it can be opened again later."
  [obj filename-only]
  (with-open [w (io/writer (monstr-file filename-only))]
    (.write w (pr-str obj))))

(defn read-monstr-data
  [filename-only]
  (edn/read-string (slurp (monstr-file filename-only))))

(defn monstr-file-exists? [filename-only]
  (.exists (monstr-file filename-only)))

(defn record-to-map [record]
  (into {} record))

(defn save-views
  [views]
  (let [clean (into {}
                    (for [[name view] views]
                      [name (record-to-map view)]))]
    (write-monstr-data clean "views.clj")))

(defn load-views
  "Returns nil if there is no view file."
  []
  (when (monstr-file-exists? "views.clj")
    (let [raw (read-monstr-data "views.clj")]
      (into {}
            (for [[name map] raw]
              [name (domain/map->View map)])))))
