(ns monstr.cache
  (:import (com.google.common.cache CacheBuilder Cache CacheLoader LoadingCache)))

(defn build
  ^Cache [^String spec]
  (.build
    (CacheBuilder/from spec)))

(defn build-loading
  ^LoadingCache [^String spec load-fn]
  (.build
    (CacheBuilder/from spec)
    ^CacheLoader (proxy [CacheLoader] []
                   (load [k]
                     (load-fn k)))))

(defn cache-contains?
  [^Cache cache key]
  (some? (.getIfPresent cache key)))

(defn put!
  ([^Cache cache key]
   (put! cache key ::existential))
  ([^Cache cache key val]
   (.put cache key val)))

(defn get-if-present
  [^Cache cache key]
  (.getIfPresent cache key))

(defn get*
  [^LoadingCache cache key]
  (.get cache key))

