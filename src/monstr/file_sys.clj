(ns monstr.file-sys
  (:require [clojure.java.io :as io])
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
