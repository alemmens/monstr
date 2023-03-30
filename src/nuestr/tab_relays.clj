(ns nuestr.tab-relays
  (:require
   [cljfx.api :as fx]
   [clojure.tools.logging :as log]
   [clojure.string :as str]
   [nuestr.domain :as domain]
   [nuestr.file-sys :as file-sys]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.timeline :as timeline]
   [nuestr.util :as util])
  (:import (javafx.geometry Insets)
           (javafx.scene.layout VBox HBox Priority)))

(defn- update-relay! [r property value]
  (let [new-relay (assoc r property value)]
    (swap! domain/*state assoc
           :relays (conj (remove #{r} (:relays @domain/*state))
                         new-relay))
    (store/update-relay! store/db new-relay)))

(def url-width 300)
(def checkbox-width 80)

(defn- relay-row [r]
  {:fx/type :h-box
   :spacing 20
   :children [{:fx/type :label
               :min-width url-width
               :max-width url-width
               :padding 5
               :text (:url r)}
              {:fx/type :check-box
               :min-width checkbox-width
               :max-width checkbox-width
               :padding 5
               :selected (:read? r)
               :on-selected-changed
               (fn [e] (update-relay! r :read? e))}
              {:fx/type :check-box
               :min-width checkbox-width
               :max-width checkbox-width
               :padding 5               
               :selected (:write? r)
               :on-selected-changed
               (fn [e] (update-relay! r :write? e))}]})

(defn- header [{:keys [relays relays-sort-by]}]
  (let [up-arrow (char 0x25b2)]
    {:fx/type :h-box
     :spacing 20
     :children [{:fx/type :hyperlink
                 :min-width url-width
                 :max-width url-width
                 :disable (= relays-sort-by :url)
                 :style-class ["hyperlink"]
                 :text (str "Relay "
                            (if (= relays-sort-by :url) up-arrow ""))
                 :on-action (fn [_]
                              (swap! domain/*state assoc
                                    :relays-sort-by :url))}
                {:fx/type  :hyperlink
                 :min-width checkbox-width
                 :max-width checkbox-width
                 :disable (= relays-sort-by :read?)
                 :style-class ["hyperlink"]
                 :text (str "Read "
                            (if (= relays-sort-by :read?) up-arrow ""))
                 :on-action (fn [_]
                              (swap! domain/*state assoc
                                    :relays-sort-by :read?))}
                {:fx/type :hyperlink
                 :min-width checkbox-width
                 :max-width checkbox-width
                 :disable (= relays-sort-by :write?)
                 :style-class ["hyperlink"]
                 :text (str "Write "
                            (if (= relays-sort-by :write?) up-arrow ""))
                 :on-action (fn [_]
                              (swap! domain/*state assoc
                                    :relays-sort-by :write?))}]}))

(defn- compare-for-read [r1 r2]
  (cond (= (:read? r1) (:read? r2)) (if (= (:write? r1) (:write? r2))
                                      (compare (:url r1) (:url r2))
                                      (if (:write? r1) -1 1))
        (:read? r1) -1
        :else 1))

(defn- compare-for-write [r1 r2]
  (cond (= (:write? r1) (:write? r2)) (if (= (:read? r1) (:read? r2))
                                        (compare (:url r1) (:url r2))
                                        (if (:read? r1) -1 1))
        (:write? r1) -1
        :else 1))

(defn- compare-for-url [r1 r2]
  (compare (:url r1) (:url r2)))
    
(defn- sort-relays [relays relays-sort-by]
  (sort (case relays-sort-by
          :read? compare-for-read
          :write? compare-for-write
          compare-for-url)
        relays))


(defn relays
  [{:keys [relays relays-sort-by]}]
  #_(log/debugf "Relays tab sort-by %s with %d relays"
                relays-sort-by
                (count (:relays @domain/*state)))
  {:fx/type :scroll-pane
   :padding 20
   :hbar-policy :as-needed
   :vbar-policy :as-needed
   :content {:fx/type :v-box
             :padding 5
             :children (cons {:fx/type header
                              :relays relays
                              :relays-sort-by relays-sort-by}
                             (map relay-row (sort-relays relays relays-sort-by)))}})



  


