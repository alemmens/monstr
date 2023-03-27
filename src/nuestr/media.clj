(ns nuestr.media
  (:require [nuestr.util :as util]
            [nuestr.cache :as cache]
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
   (fn [[picture-url width height]]
     (when (string? picture-url)
       (let [url (if (str/starts-with? picture-url "<iframe src=")
                   ;; For some reason we have lots of picture urls that look like
                   ;; <iframe src="https://giphy.com/embed/xUNda1y700or5uNM7m" width="480"
                   ;; height="360" frameBorder="0" class="giphy-embed" allowFullScreen></iframe>
                   ;; We'll just extract the url from that.
                   (second (re-find #"\"(http[^\"]+)" picture-url))
                   picture-url)]
         (when-not (str/blank? url)
           (try (Image. url ^double width ^double height true true true)
                (catch Exception e
                  (log/debugf "Image cache exception for %s: %s"
                              url
                              (.getMessage e))))))))))

(defn show-picture [{:keys [url width height]}]
  (if-let [image (when-not (str/blank? url)
                   (try (cache/get* image-cache [url width (or height width)])
                        (catch Exception e
                          (log/debugf "Can't find %s in image cache: %s"
                                      url
                                      (.getMessage e)))))]
    {:fx/type :image-view
     :image image}
    {:fx/type :label
     :min-width width
     :min-height height
     :max-width width
     :max-height height
     :text url}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Avatars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn avatar [{:keys [width picture-url]}]
  (show-picture {:url picture-url
                 :width width
                 :height width}))

(def avatar-dim 40)

(defn avatar-or-empty-space
  [picture-url avatar-color pubkey-for-avatar]
  (if (str/blank? picture-url)
    {:fx/type :label
     :min-width avatar-dim
     :min-height avatar-dim
     :max-width avatar-dim
     :max-height avatar-dim
     :style {:-fx-background-color avatar-color}
     :style-class "ndesk-timeline-item-photo"
     :text pubkey-for-avatar}
    {:fx/type avatar
     :width avatar-dim
     :picture-url picture-url}))

