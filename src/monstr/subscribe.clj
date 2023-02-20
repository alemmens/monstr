(ns monstr.subscribe
  (:require
   [monstr.domain :as domain]
   [monstr.relay-conn :as relay-conn]
   [clojure.tools.logging :as log]
   [monstr.util :as util])
  (:import (java.time Instant)))


;; todo ultimately may need to partition whale-of-pubkeys
(defn whale-of-pubkeys*
  [pubkeys contact-lists]
  (let [contact-pubkeys (mapcat #(map :public-key (:parsed-contacts %)) (vals contact-lists))]
    (set (concat pubkeys contact-pubkeys))))

(defn overwrite-subscriptions!
  ([identities contact-lists since]
   ;; SINCE is a Java Instant.  
   ;; todo note: since here is a stop-gap protection .. really we would like to track a
   ;; durable "watermark" for stable subscriptions
   (let [use-since (.getEpochSecond since)
         pubkeys (mapv :public-key identities)]
     (when-not (empty? pubkeys)
       (let [filters (filterv
                      some?
                      [(domain/->subscription-filter
                        nil [0 1 2 3] nil nil use-since nil (whale-of-pubkeys* pubkeys contact-lists))
                       (domain/->subscription-filter
                        nil [1 4] nil pubkeys use-since nil nil)
                       (domain/->subscription-filter
                        nil [4] nil nil use-since nil pubkeys)])]
         (swap! domain/*state assoc :last-refresh (Instant/now))
         (relay-conn/subscribe-all! "primary" filters)
         (log/info "overwrote subscriptions")))))
  ([identities contact-lists]
   (let [last-refresh (:last-refresh @domain/*state)
         since (or last-refresh (util/days-ago 2))]
     (overwrite-subscriptions! identities contact-lists since))))
  
(defn refresh! []
  (log/debugf "Refreshing...")
  (overwrite-subscriptions! (:identities @domain/*state)
                            (:contact-lists @domain/*state)))

