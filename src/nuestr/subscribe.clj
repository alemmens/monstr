(ns nuestr.subscribe
  (:require
   [clojure.tools.logging :as log]
   [nuestr.domain :as domain]
   [nuestr.relay-conn :as relay-conn]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.util :as util])
  (:import (java.time Instant)))


;; TODO ultimately may need to partition whale-of-pubkeys.
(defn whale-of-pubkeys*
  [pubkeys contact-lists]
  (let [contact-pubkeys (mapcat #(map :public-key (:parsed-contacts %))
                                (vals contact-lists))]
    (set (concat pubkeys contact-pubkeys))))


(defn relevant-pubkeys-for-view
  "Returns a sequence with pubkeys that must be used for subscribing to relays or loading
  from the database. If the result is nil, that means we should not filter on event
  authors at all ('follow all')."
  [view]
  (case (:follow view)
    :all nil
    :use-identity (let [account-keys [(:public-key (domain/active-identity))]
                        contact-lists (store/load-contact-lists store/db account-keys)]
                    (log/debugf "Computing whale for %d contact lists" (count contact-lists))
                    (whale-of-pubkeys* account-keys contact-lists))
    :use-list (:follow-set view)))

(defn filters-for-view [view since]
  ;; 0: set_metadata
  ;; 1: text note
  ;; 2: recommend server
  ;; 3: contact list
  ;; 4: direct message
  ;; 40: channel create
  ;; 41: channel metadata
  ;; 42: channel message
  ;; 43: hide message
  ;; 44: mute user
  (let [account-pubkeys (map :public-key (:identities @domain/*state))]
    [{:kinds [0 1 3]
      :since since
      :authors (relevant-pubkeys-for-view view)
      :limit 1000}
     {:kinds [2]
      :limit 1000}
     {:kinds [1 4]
      :#p account-pubkeys
      :since since}
     {:kinds [4]
      :since since
      :authors account-pubkeys}
     {:kinds [40 41]
      :limit 1000}
     
     #_
     {:kinds [42] ; TODO: channel messages
      :since since
      :limit 5000}
     ]))
  
(defn overwrite-subscriptions!
  ([column since]
   ;; SINCE is a Java Instant.  
   ;; TODO: track a durable "watermark" for stable subscriptions.
   (when-not (empty? (:identities @domain/*state))
     (let [view (:view column)
           filters (filters-for-view view (.getEpochSecond since))]
       #_(swap! domain/*state assoc :last-refresh (Instant/now))
       (log/debugf "Subscribing all for '%s'" (:name view))
       (relay-conn/subscribe-all! (format "flat:%s" (:id column))
                                  filters)
       (log/info "overwrote subscriptions"))))
  
  ([column]
   (let [last-refresh (:last-refresh @domain/*state)
         since (or last-refresh (util/days-ago 1))]
     (overwrite-subscriptions! column since))))

(defn refresh! []
  (status-bar/message! "Refreshing subscriptions.")
  (doseq [c (:all-columns @domain/*state)]
    (overwrite-subscriptions! c)))

