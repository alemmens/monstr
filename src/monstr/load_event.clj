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

(defn- load-from-store [db event-id]
  (when-let [event (store/load-event db event-id)]
    (assoc event :relays (store/load-relays-for-event db event-id))))

(defn async-load-event!
  "Load the event with the given id from either the database or from the relays."
  [*state db ^ScheduledExecutorService executor event-id]
  (log/info "loading event" {:event-id event-id})
  (if-let [event-from-store (load-from-store db event-id)]
    (do (log/debugf "Found event in store: %s" event-from-store)
        (timeline/dispatch-text-note! *state true event-from-store))
    ;; Create a unique subscription id to load the event, subscribe to
    ;; all relays in the hope that we find the event and then unsubscribe
    ;; 10 seconds later.
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
