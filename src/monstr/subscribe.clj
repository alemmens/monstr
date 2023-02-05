(ns monstr.subscribe
  (:require
   [monstr.domain :as domain]
   [monstr.relay-conn :as relay-conn]
   [clojure.tools.logging :as log]
   [monstr.util :as util]))

;; todo ultimately may need to partition whale-of-pubkeys
(defn whale-of-pubkeys*
  [pubkeys contact-lists]
  (let [contact-pubkeys (mapcat #(map :public-key (:parsed-contacts %)) (vals contact-lists))]
    (set (concat pubkeys contact-pubkeys))))

(defn overwrite-subscriptions!
  [identities contact-lists]
  ;; todo note: since here is a stop-gap protection .. really we would like to track a durable "watermark" for stable subscriptions
  (let [use-since (-> (util/days-ago 45) .getEpochSecond)
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
        (relay-conn/subscribe-all! "primary" filters)
        (log/info "overwrote subscriptions")))))
