(ns nuestr.channels
  (:require [nuestr.cache :as nuestr-cache]
            [nuestr.store :as store]
            [clojure.tools.logging :as log])
  (:import (nuestr.domain Channel)))

(def ^:private cache-spec
  "initialCapacity=500,maximumSize=1000")

(defn create-cache
  [db]
  (nuestr-cache/build-loading cache-spec
    (fn [k]
      (get (store/load-metadata db [k]) k ::missing))))

(defonce cache (create-cache store/db))

(defn update!
  [id ^Channel channel]
  (nuestr-cache/put! cache id channel))

(defn get*
  ^Channel [id]
  (let [rv (nuestr-cache/get* cache id)]
    (when-not (identical? rv ::missing)
      rv)))


