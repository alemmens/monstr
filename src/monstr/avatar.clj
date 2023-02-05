(ns monstr.avatar
  (:require [monstr.util :as util]
            [monstr.cache :as cache])
  (:import (javafx.scene.image Image)))

(def color
  (memoize
    (fn [^String public-key]
      (util/rand-hex-color (.hashCode public-key)))))

(defonce image-cache
  (cache/build-loading "initialCapacity=500,maximumSize=1000"
    (fn [[picture-url avatar-dim]]
      (Image. picture-url ^double avatar-dim ^double avatar-dim true true true))))
