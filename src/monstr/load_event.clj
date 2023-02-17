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

(defn- load-from-store [db event-id]
  (when-let [event (store/load-event db event-id)]
    (assoc event :relays (store/load-relays-for-event db event-id))))

(defn async-load-event!
  "Load the event with the given id from either the database or from the relays."
  [*state db ^ScheduledExecutorService executor event-id]
  (if-let [event-from-store (load-from-store db event-id)]
    (timeline/dispatch-text-note! *state true event-from-store)
    ;; Create a unique subscription id to load the event and subscribe to all relays in
    ;; the hope that we find the event.  We'll unsubscribe automatically when we get an
    ;; EOSE event.
    (let [uuid (.toString (UUID/randomUUID))
          subscription-id (format "load-event:%s" uuid)]
      (relay-conn/subscribe-all! subscription-id
                                 [(domain/->subscription-filter
                                   [event-id] [1] nil nil nil nil nil)]))))
