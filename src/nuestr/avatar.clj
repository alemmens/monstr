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
  (cache/build-loading
   "initialCapacity=500,maximumSize=1000"
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
           (try (Image. url ^double avatar-dim ^double avatar-dim true true true)
                (catch Exception e
                  (log/debugf "Image cache exception for %s: %s"
                              url
                              (.getMessage e))))))))))

(defn avatar [{:keys [width picture-url]}]
  (if-let [image (when-not (str/blank? picture-url)
                   (try (cache/get* image-cache [picture-url width])
                        (catch Exception e
                          (log/debugf "Can't find %s in image cache: %s"
                                      picture-url
                                      (.getMessage e)))))]
    {:fx/type :image-view
     :image image}
    {:fx/type :label
     :min-width width
     :min-height width
     :max-width width
     :max-height width
     :text ""}))
