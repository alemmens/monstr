(ns nuestr.metadata
  (:require [nuestr.cache :as nuestr-cache]
            [nuestr.store :as store]
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
