(ns nuestr.tab-relays
  (:require
   [cljfx.api :as fx]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.string :as str]
   [nuestr.domain :as domain]
   [nuestr.file-sys :as file-sys]
   [nuestr.relay-conn :as relay-conn]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.timeline :as timeline]
   [nuestr.util :as util])
  (:import (javafx.geometry Insets)
           (javafx.scene.control DialogEvent Dialog Button TextArea)
           (javafx.scene.layout VBox HBox Priority)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update-relay! [r property value]
  (let [new-relay (assoc r property value)]
    (status-bar/message! (format "Changed the %s property of %s to %s."
                                 property (:url r) value))
    (swap! domain/*state assoc
           :relays (util/update-in-sequence r new-relay (:relays @domain/*state)))
    (store/update-relay! store/db new-relay)))

(def url-width 300)
(def checkbox-width 80)

(defn- relay-row [r connected-info]
  {:fx/type :h-box
   :spacing 7
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
               :on-selected-changed (fn [e] (update-relay! r :read? e))}
              {:fx/type :check-box
               :min-width checkbox-width
               :max-width checkbox-width
               :padding 5               
               :selected (:write? r)
               :on-selected-changed (fn [e] (update-relay! r :write? e))}
              {:fx/type :v-box
               :padding 5
               :children [{:fx/type status-bar/relay-dot
                           :relay r
                           :connected-info connected-info}]}]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compare-urls [r1 r2]
  (compare (util/relay-url-short r1) (util/relay-url-short r2)))

(defn- compare-for-read [r1 r2]
  (cond (= (:read? r1) (:read? r2)) (if (= (:write? r1) (:write? r2))
                                      (compare-urls (:url r1) (:url r2))
                                      (if (:write? r1) -1 1))
        (:read? r1) -1
        :else 1))

(defn- compare-for-write [r1 r2]
  (cond (= (:write? r1) (:write? r2)) (if (= (:read? r1) (:read? r2))
                                        (compare-urls (:url r1) (:url r2))
                                        (if (:read? r1) -1 1))
        (:write? r1) -1
        :else 1))

(defn- compare-for-url [r1 r2]
  (compare-urls (:url r1) (:url r2)))

(defn compare-for-status [connected-info r1 r2]
  (let [status1 (get connected-info (:url r1))
        status2 (get connected-info (:url r2))]
    (cond (= status1 status2) (compare-for-read r1 r2)
          status1 -1
          :else 1)))
    
(defn sort-relays [relays sort-by connected-info]
  (sort (case sort-by
          :read? compare-for-read
          :write? compare-for-write
          :status (partial compare-for-status connected-info)
          compare-for-url)
        relays))

(defn maybe-add-relay!
  "Returns the new Relay if successful, otherwise nil."
  [url]
  (when (and (relay-conn/is-relay-url? url)
             ;; We don't want 'numerical' urls like 'wss://123.456.789'.
             (not (util/numerical-relay-url? url)))
    (when-not (domain/find-relay url)
      (let [r (domain/->Relay url false false true)]
        (status-bar/message! (format "Adding recommended relay %s" url))
        ;; Add the recommended relay.
        (store/insert-relay! store/db r)
        (swap! domain/*state assoc
               :relays (sort-relays (conj (:relays @domain/*state) r)
                                    (:relays-sorted-by @domain/*state)
                                    (:connected-info @domain/*state)))
        ;; And try to get server recommendations from the new relay.
        (relay-conn/add-meta-subscription! url)
        ;; Return the new relay
        r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Header
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- header [{:keys [relays relays-sorted-by connected-info]}]
  (let [up-arrow (char 0x25b2)]
    {:fx/type :h-box
     :style-class "header"
     :spacing 7
     :children [{:fx/type :hyperlink
                 :min-width url-width
                 :max-width url-width
                 :style-class ["hyperlink"]
                 :text (str "Relay "
                            (if (= relays-sorted-by :url) up-arrow ""))
                 :on-action (fn [_]
                              (swap! domain/*state assoc
                                     :relays (sort-relays relays :url connected-info)
                                     :relays-sorted-by :url))}
                {:fx/type  :hyperlink
                 :min-width checkbox-width
                 :max-width checkbox-width
                 :style-class ["hyperlink"]
                 :text (str "Read "
                            (if (= relays-sorted-by :read?)
                              up-arrow
                              ""))
                 :on-action (fn [_]
                              (swap! domain/*state assoc
                                     :relays (sort-relays relays :read? connected-info)
                                     :relays-sorted-by :read?))}
                {:fx/type :hyperlink
                 :min-width checkbox-width
                 :max-width checkbox-width
                 :style-class ["hyperlink"]
                 :text (str "Write "
                            (if (= relays-sorted-by :write?) up-arrow ""))
                 :on-action (fn [_]
                              (swap! domain/*state assoc
                                     :relays (sort-relays relays :write? connected-info)
                                     :relays-sorted-by :write?))}
                {:fx/type :hyperlink
                 :min-width checkbox-width
                 :max-width checkbox-width
                 :style-class ["hyperlink"]
                 :text (str "Status "
                            (if (= relays-sorted-by :status) up-arrow ""))
                 :on-action (fn [_]
                              (swap! domain/*state assoc
                                     :relays (sort-relays relays :status connected-info)
                                     :relays-sorted-by :status))}]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relay input field (for searching)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-relay-button [{:keys [relays relay-search-text]}]
  {:fx/type :button
   :disable (boolean (or (not (relay-conn/can-be-used-for-relay-url? relay-search-text))
                         (domain/find-relay relay-search-text)))
   :on-mouse-pressed (fn [_]
                       (if (maybe-add-relay! relay-search-text)
                         (status-bar/message! (format "Added new relay %s" relay-search-text))
                         (status-bar/message! (format "Can't add relay %s" relay-search-text))))
   :text "Add Relay"})

(defn- search-field [{:keys [relays relay-search-text]}]
  {:fx/type :v-box
   :children [{:fx/type :label
               :text (format "Search (in %d relays): " (count relays))}
              {:fx/type :h-box
               :spacing 10
               :children [{:fx/type :text-field
                           :max-width url-width
                           :min-width url-width
                           :padding 5
                           :text (or relay-search-text "")
                           :style-class ["text-input"]
                           :on-text-changed  (fn [new-text]
                                               (swap! domain/*state assoc
                                                      :relay-search-text new-text))}
                          {:fx/type add-relay-button
                           :relays relays
                           :relay-search-text relay-search-text}]}
              {:fx/type :h-box :padding 10}]})

(defn filter-relays [relays relay-search-text]
  (filter #(str/includes? (:url %) relay-search-text)
          relays))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn relays
  [{:keys [relays relays-sorted-by relay-search-text connected-info]}]
  #_(log/debugf "Relays tab sort-by %s with %d relays"
                relays-sort-by
                (count (:relays @domain/*state)))
  {:fx/type :scroll-pane
   :padding 15
   :hbar-policy :as-needed
   :vbar-policy :as-needed
   :content {:fx/type :v-box
             :padding 5
             :children (concat [{:fx/type search-field
                                 :relays relays
                                 :relay-search-text relay-search-text}
                                {:fx/type header
                                 :relays relays
                                 :relays-sorted-by relays-sorted-by
                                 :connected-info connected-info}]
                               (map #(relay-row % connected-info)
                                    (filter-relays relays relay-search-text)))}})



  


