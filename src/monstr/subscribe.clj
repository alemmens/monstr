(ns monstr.subscribe
  (:require
   [clojure.tools.logging :as log]
   [monstr.domain :as domain]
   [monstr.relay-conn :as relay-conn]
   [monstr.status-bar :as status-bar]
   [monstr.util :as util])
  (:import (java.time Instant)))


;; TODO ultimately may need to partition whale-of-pubkeys.
(defn whale-of-pubkeys*
  [pubkeys contact-lists]
  (let [contact-pubkeys (mapcat #(map :public-key (:parsed-contacts %)) (vals contact-lists))]
    (set (concat pubkeys contact-pubkeys))))

(defn overwrite-subscriptions!
  
  ([identities contact-lists column since]
   ;; SINCE is a Java Instant.  
   ;; TODO note: since here is a stop-gap protection .. really we would like to track a
   ;; durable "watermark" for stable subscriptions
   (let [use-since (.getEpochSecond since)
         pubkeys (mapv :public-key identities)]
     (when-not (empty? pubkeys)
       (let [filters [{:kinds [0 1 2 3]
                       :since use-since
                       :authors (if (domain/follows-all? column)
                                  []
                                  (whale-of-pubkeys* pubkeys contact-lists))
                       :limit 1000}
                      {:kinds [1 4] :#p pubkeys :since use-since}
                      {:kinds [4] :since use-since :authors pubkeys}]]
         (swap! domain/*state assoc :last-refresh (Instant/now))
         (log/debugf "Subscribing all for '%s' with filters %s"
                     (:name (:view column))
                     filters)
         (relay-conn/subscribe-all! (format "flat:%s" (:id column))
                                    filters)
         (log/info "overwrote subscriptions")))))
  
  ([identities contact-lists column]
   (let [last-refresh (:last-refresh @domain/*state)
         since (or last-refresh (util/days-ago 1))]
     (overwrite-subscriptions! identities contact-lists column since))))

(defn refresh! []
  (status-bar/message! "Refreshing subscriptions.")
  (doseq [c (:all-columns @domain/*state)]
    (overwrite-subscriptions! (:identities @domain/*state)
                              (:contact-lists @domain/*state)
                              c)))

