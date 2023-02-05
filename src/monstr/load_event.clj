(ns monstr.load-event
  (:require
    [clojure.tools.logging :as log]
    [monstr.domain :as domain]
    [monstr.relay-conn :as relay-conn]
    [monstr.store :as store]
    [monstr.timeline :as timeline]
    [monstr.util :as util])
  (:import (java.util UUID)
           (java.util.concurrent ScheduledExecutorService)))

(def max-load-event-time-millis 10000)

(defonce active-load-event-info (atom {}))

(defn async-load-event!
  [*state db ^ScheduledExecutorService executor event-id]
  (log/info "loading event" {:event-id event-id})
  (if-let [event-from-store (store/load-event db event-id)]
    (timeline/dispatch-text-note! *state event-from-store)
    (let [uuid (.toString (UUID/randomUUID))
          subscription-id (format "load-event:%s" uuid)]
      (swap! active-load-event-info
        (fn [{:keys [subscription-id]}]
          (when subscription-id
            (relay-conn/unsubscribe-all! subscription-id))
          {:subscription-id subscription-id}))
      (relay-conn/subscribe-all!
        subscription-id
        [(domain/->subscription-filter
           [event-id] [1] nil nil nil nil nil)])
      (util/schedule! executor
        #(relay-conn/unsubscribe-all! subscription-id)
        max-load-event-time-millis))))
