(ns nuestr.metadata
  (:require [nuestr.cache :as nuestr-cache]
            [nuestr.nip05 :as nip05]
            [nuestr.nip19 :as nip19]
            [nuestr.store :as store]
            [nuestr.util :as util]
            [clojure.tools.logging :as log])
  (:import (nuestr.domain ParsedMetadata)))

(def ^:private cache-spec
  "initialCapacity=500,maximumSize=1000")

(defn create-cache
  [db]
  (nuestr-cache/build-loading cache-spec
    (fn [k]
      (get (store/load-metadata db [k]) k ::missing))))

(defn update!
  [cache pubkey ^ParsedMetadata parsed-metadata]
  (nuestr-cache/put! cache pubkey parsed-metadata))

(defn get*
  ^ParsedMetadata [cache pubkey]
  (let [rv (nuestr-cache/get* cache pubkey)]
    (when-not (identical? rv ::missing)
      rv)))

(defonce cache (create-cache store/db))

(defn npub-format-short [pubkey]
  (if (empty? pubkey)
    "?"
    (util/format-string-short (nip19/encode "npub" pubkey))))

(defn metadata-name [pubkey]
  (if-let [metadata (and pubkey (get* cache pubkey))]
    (let [name (:name metadata)]
      (if (empty? name)
        (npub-format-short pubkey)
        name))
    (npub-format-short pubkey)))
  
(defn user-short-name [pubkey]
  (if-let [user (nip05/find-by-pubkey pubkey)]
    (let [name (nip05/pretty-name user)]
      (if (empty? name)
        (metadata-name pubkey)
        name))
    (metadata-name pubkey)))
