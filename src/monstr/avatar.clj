(ns monstr.avatar
  (:require [monstr.util :as util]
            [monstr.cache :as cache]
            [clojure.tools.logging :as log]
            [clojure.string :as str])
  (:import (javafx.scene.image Image)))

(def color
  (memoize
    (fn [^String public-key]
      (util/rand-hex-color (.hashCode public-key)))))

(defonce image-cache
  (cache/build-loading "initialCapacity=500,maximumSize=1000"
                       (fn [[picture-url avatar-dim]]
                         (when (string? picture-url)
                           (let [url (if (str/starts-with? picture-url "<iframe src=")
                                       ;; For some reason we have lots of picture urls that look like
                                       ;; <iframe src="https://giphy.com/embed/xUNda1y700or5uNM7m" width="480"
                                       ;; height="360" frameBorder="0" class="giphy-embed" allowFullScreen></iframe>
                                       ;; We'll just extract the url from that.
                                       (second (re-find #"\"(http[^\"]+)" picture-url))
                                       picture-url)]
                             (when-not (str/blank? url)
                               #_(log/debugf "Image cache for picture url %s" picture-url)
                               (Image. url ^double avatar-dim ^double avatar-dim true true true)))))))
