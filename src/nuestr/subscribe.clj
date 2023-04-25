(ns nuestr.subscribe
  (:require
   [clojure.tools.logging :as log]
   [manifold.stream :as s]
   [nuestr.domain :as domain]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.util :as util])
  (:import (java.time Instant)
           (java.util UUID)))


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
    [{:kinds [0 2 3]
      :since since
      :authors (relevant-pubkeys-for-view view)
      :limit 1000}
     {:kinds [1]
      :since since
      :authors (relevant-pubkeys-for-view view)
      :limit 200}
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

(defn meta-subscription
  "Returns a map from subscription id to filters."
  [pubkeys]
  #_(status-bar/debug! (format "Subscribing to meta for %s" pubkeys))
  (let [filter (if pubkeys
                 {:kinds [0 2] :authors [pubkeys]}
                 {:kinds [2] :limit 3000})
        subscription-id (format "meta:%s" (.toString (UUID/randomUUID)))]
    {subscription-id [filter]}))
