(ns nuestr.status-bar
  (:require
   [cljfx.api :as fx]
   [clojure.tools.logging :as log]
   [nuestr.domain :as domain]
   [nuestr.util :as util]
   [nuestr.view-relays :as view-relays])
  (:import (javafx.scene.canvas Canvas)
           (javafx.scene.paint Color)
           (java.time Instant)))

(defn- relay-dot
  [{:keys [connected-info] {:keys [url read? write?] :as _relay} :relay}]
  {:fx/type :label
   :style {:-fx-padding [0 2]}
   :tooltip
   {:fx/type :tooltip
    :text (format "%s%s" url
            (cond
              (and read? write?) ""
              read? " (read-only)"
              write? " (write-only)"
              :else " (disabled)"))}
   :graphic
   (let [dim 12]
     {:fx/type :canvas
      :width dim
      :height dim
      :draw
      (fn [^Canvas canvas]
        (doto (.getGraphicsContext2D canvas)
          (.setFill (if (get connected-info url) Color/LIGHTGREEN Color/LIGHTGREY))
          (.fillOval 0 0 dim dim)))})})

(defn- relay-dots
  [{:keys [relays connected-info]}]
  {:fx/type :h-box
   :padding 5
   ; :style {:-fx-padding [0 5 0 0]}
   :children
   (mapv #(hash-map :fx/type relay-dot
                    :relay %
                    :connected-info connected-info) relays)})

(defn status-relays
  [{:keys [show-relays? relays refresh-relays-ts connected-info]}]
  {:fx/type fx/ext-let-refs
   :refs {:dialog {:fx/type view-relays/dialog
                   :show-relays? show-relays?
                   :relays relays
                   :refresh-relays-ts refresh-relays-ts}}
   :desc {:fx/type :h-box
          :style-class ["ndesk-status-relays"]
          :alignment :center
          :padding 5
          :children [{:fx/type :text :text "Relays: "}
                     (if (nil? relays)
                       {:fx/type :text :text "..."}
                       {:fx/type relay-dots
                        :relays (filter #(or (:write? %) (:read? %))
                                        relays)
                        :connected-info connected-info})]
          :cursor :hand
          :on-mouse-clicked {:event/type :show-relays}}})

(defn pane [{:keys [show-relays? relays refresh-relays-ts connected-info status-message]}]
  {:fx/type :border-pane
   :style-class "ndesk-status-bar"
   :left {:fx/type :h-box
          :padding 5
          :children [{:fx/type :label
                      :text (or status-message "")}]}
   :right {:fx/type status-relays
           :show-relays? show-relays?
           :relays relays
           :refresh-relays-ts refresh-relays-ts
           :connected-info connected-info}})

(defn message! [message]
  (log/debug message)
  (swap! domain/*state assoc
         :status-message message
         :status-message-timestamp (util/now-epoch-second)
         ))
