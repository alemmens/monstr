(ns nuestr.relay-conn
  (:require
   [aleph.http :as http]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [manifold.time :as t]
   [nuestr.domain :as domain]
   [nuestr.json :as json*]
   [nuestr.status-bar :as status-bar]
   [nuestr.subscribe :as subscribe]
   [nuestr.util :as util]))

;; TODO: adjust "since" on re/connect ... "watermark" w/ Nminute lag???? or author watermark in db???

;; TODO: how to do client-side "fulfillment"?

;; TODO: test relay connection failures, losses etc


(defn- websocket-client* [relay-url]
  (http/websocket-client relay-url
                         {:insecure? true
                          :max-frame-payload 196608
                          :max-frame-size 3145728
                          :heartbeats {:send-after-idle 5000}}))

(defn is-relay-url? [string]
  ;; TODO: This is a bit too simple.
  (re-matches #"wss?://[^ ]+$" string))


;; --

(def ^:private connect-timeout-secs 20)
(def ^:private send-timeout-secs 10)

(defrecord ReadConnection
  [relay-url
   deferred-conn
   subscriptions ;; id -> [filters]
   sink-stream
   num-successive-failures
   destroyed?])

(declare connect!*)

(defn- retry-delay-ms [num-successive-failures]
  (case num-successive-failures
    0 1000
    1 5000
    2 10000
    3 20000
    4 60000
    300000))

(defn connection-has-subscription?
  "Returns true if a ReadConnection has a subscription for the given `filters`."
  [read-connection filters]
  (some #{filters} (vals (:subscriptions read-connection))))
  

(defn- send-subscribe-req!* [conn-vol subscriptions-snapshot new-raw-conn]
  (locking conn-vol
    (let [{:keys [relay-url]} @conn-vol]
      (doseq [[id filters] subscriptions-snapshot]
        #_(log/debugf "subscribing %s to %s" id relay-url)
        (s/put! new-raw-conn
          (json*/write-str* (vec (concat ["REQ" id] filters))))
        (status-bar/message! (format "subscribed %s on %s" id relay-url))))))

(defn on-failure [conn-vol err]
  (locking conn-vol
    (let [{:keys [relay-url num-successive-failures destroyed?]} @conn-vol]
      (when-not destroyed?
        ;; Note: we should never see a deferred-conn here that isn't yet realized; so any
        ;; listeners of prior deferred connection should have had their chance to fire.
        (vswap! conn-vol assoc :deferred-conn (d/deferred))
        ;; could be a connection failure or abrupt closure
        (let [delay-ms (if (= err :connection-closed)
                         600000 ; try again in 10 minutes
                         (retry-delay-ms num-successive-failures))]
          (status-bar/message! (format "connection failure '%s'; reconnecting in %d ms; %s"
                                       relay-url delay-ms
                                       (pr-str (take 1 (str/split-lines (str err))))))
          (vswap! conn-vol update :num-successive-failures inc)          
          (t/in delay-ms #(connect!* conn-vol)))))))

(defn connect!*
  [conn-vol]
  (locking conn-vol
    (let [{:keys [relay-url deferred-conn sink-stream destroyed?]
           subscriptions-snapshot :subscriptions} @conn-vol]
      (when-not destroyed?
        (log/debugf "connect attempt %s" relay-url)
        ;; We contrive here for our deferred-conn to for-sure get an error or success.
        (-> (websocket-client* relay-url)
            ;; Timeout without a timeout-val produces an d/error! that is handled
            ;; by the d/catch below.
            (d/timeout! (* connect-timeout-secs 1000))
            (d/chain
             (util/wrap-exc-fn
              (fn [raw-conn]
                (locking conn-vol
                  (log/debugf "connected %s" relay-url)
                  (vswap! conn-vol assoc :num-successive-failures 0)
                  (d/success! deferred-conn raw-conn)
                  ;; :downstream? false means when raw-conn closes the sink-stream will not.
                  (s/connect raw-conn sink-stream {:downstream? false})
                  (s/on-closed raw-conn
                               #(on-failure conn-vol :connection-closed))
                  (send-subscribe-req!* conn-vol subscriptions-snapshot raw-conn))
                :unused)))
            (d/catch (fn [err] (on-failure conn-vol err))))))))

(defn connect! [relay-url sink-stream]
  (doto (volatile! (->ReadConnection relay-url (d/deferred) {} sink-stream 0 false))
    connect!*))

(defn- connected?* [deferred-conn]
  (and (d/realized? deferred-conn)
       (s/stream? @deferred-conn)))

(defn connected? [conn-vol]
  (locking conn-vol
    (let [{:keys [deferred-conn]} @conn-vol]
      (connected?* deferred-conn))))

(defn destroy! [conn-vol]
  (log/debugf "destroying %s" (:relay-url @conn-vol))
  (vswap! conn-vol assoc :destroyed? true)
  (locking conn-vol
    (-> @conn-vol
        :sink-stream
        s/close!)
    (-> @conn-vol
      :deferred-conn
      (d/chain s/close!)
      (d/error! :destroyed))))

(defn subscribe! [conn-vol id filters]
  {:pre [(vector? filters) (every? map? filters)]}
  (status-bar/message! (format "Subscribing to events from %s" (:relay-url @conn-vol)))
  (locking conn-vol
    (vswap! conn-vol update :subscriptions
            assoc id filters)
    (let [{:keys [relay-url deferred-conn]} @conn-vol]
      (d/chain deferred-conn
        (util/wrap-exc-fn ::subscribe!
                          (fn [raw-conn]
                            (log/debugf "subscribing %s %s %s"
                                        id relay-url (type raw-conn))
                            (s/put! @deferred-conn
                                    (json*/write-str*
                                     (vec (concat ["REQ" id] filters))))))))))

(defn unsubscribe! [conn-vol id]
  (locking conn-vol
    (vswap! conn-vol update :subscriptions dissoc id)
    (let [{:keys [deferred-conn]} @conn-vol]
      (when (d/realized? deferred-conn)
        (log/debugf "Closing subscription with id %s (%d left)"
                    id
                    (count (:subscriptions @conn-vol)))
        (status-bar/message! "")
        (s/put! @deferred-conn
                (json*/write-str* ["CLOSE" id]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Registry/pool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The idea with writes is that we'll re-use ReadConnection if there is one, otherwise
;; we'll create a write connection on-demand for the async write and give it a timeout to
;; close unless a new write arrives writes will be retried w/ backoff? and then failure
;; otherwise reported upstream/to user??

(defrecord Registry
  [sink-stream
   write-connections-vol ;; relay-url -> deferred-conn
   read-connections-vol ;; relay-url -> opaque read conn; ie volatile<ReadConnection>
   subscriptions ;; vol<id -> [filters]>
   ])

(defonce conn-registry (->Registry (s/stream) (volatile! {}) (volatile! {}) (volatile! {})))

(defn sink-stream
  "Return back a stream will get *all* events for every subscribed read relay."
  []
  (:sink-stream conn-registry))

(defn maybe-add-subscriptions!
  "SUBSCRIPTIONS is a map from subscription id to filters."
  [relay-url subscription-map]
  (status-bar/message! (format "Maybe adding subscriptions (%s) for %s"
                               (vals subscription-map)
                               relay-url))
  (when-not (every? (fn [filters]
                      (when-let [connection-vol (get @(:read-connections-vol conn-registry) relay-url)]
                        (connection-has-subscription? @connection-vol filters)))
                    (vals subscription-map))
    ;; New read connection.
    (status-bar/message! (format "Adding read connection for %s" relay-url))
    (let [;; Arbitrarily use a 100-buffer stream for now; see also s/throttle.
          connection-sink-stream (s/stream 100)
          read-connection-vol (connect! relay-url connection-sink-stream)]
      (doseq [[id filters] subscription-map]
        (subscribe! read-connection-vol id filters))
      ;; :downstream? false means when conn-sink-stream closes global conn-registry stream will not.
      (let [{registry-sink-stream :sink-stream} conn-registry]
        (s/connect-via connection-sink-stream
                       #(s/put! registry-sink-stream [relay-url %])
                       registry-sink-stream
                       {:downstream? false}))
      ;; Add the relay-url to the registry.
      (vswap! (:read-connections-vol conn-registry) assoc
              relay-url read-connection-vol))))

(defn update-relays!
  "Change the set of relays and/or their read or write status."
  [relays]
  (let [read-url? (into #{} (comp (filter :read?) (map :url)) relays)
        write-url? (into #{} (comp (filter :write?) (map :url)) relays)]
    (log/debugf "Updating %d relays" (count relays))
    (locking conn-registry
      ;; Close all removed write connections AND write connections that we'll promote to
      ;; read connections.
      (doseq [[relay-url deferred-conn] @(:write-connections-vol conn-registry)]
        (when (or (not (write-url? relay-url)) (read-url? relay-url))
          (d/chain deferred-conn
            (util/wrap-exc-fn ::update-relays!
                              (fn [raw-conn]
                                (log/debugf "closing write conn %s" relay-url)
                                (s/close! raw-conn))))
          (vswap! (:write-connections-vol conn-registry) dissoc relay-url)))
      ;; Close all removed read connections.
      (doseq [[relay-url read-conn-vol] @(:read-connections-vol conn-registry)]
        (when-not (read-url? relay-url)
          (log/debugf "closing read conn %s" relay-url)
          (destroy! read-conn-vol)
          (vswap! (:read-connections-vol conn-registry) dissoc relay-url)))
      ;; Add all reader newbies -- note that writes will be on-demand per upstream sends.
      (doseq [{:keys [url read? write? meta?]} relays]
        (when read?
          (maybe-add-subscriptions! url @(:subscriptions conn-registry)))
        (when meta?
          (maybe-add-subscriptions! url (subscribe/recommend-server-subscription)))))))

(defn add-recommend-server-subscription! [relay-url]
  (log/debugf "Adding recommend-server subscription for %s" relay-url)
  (locking conn-registry
    (maybe-add-subscriptions! relay-url (subscribe/recommend-server-subscription))))

(defn subscribe-all!
  "Establish subscription to all relays marked for read. (If `update-relays!` is used
  to add new relays, outstanding subscriptions will get automatically and transparently
  added to these new relays.)"
  [id filters]
  {:pre [(vector? filters) (every? map? filters)]}
  (let [filters' (mapv util/compact filters)]
    (locking conn-registry
      (vswap! (:subscriptions conn-registry) assoc id filters')
      (doseq [[relay-url read-conn-vol] @(:read-connections-vol conn-registry)]
        (when (:read? (domain/find-relay relay-url))
          (subscribe! read-conn-vol id filters'))))))

(defn unsubscribe-all!
  "Kill a subscription by id that was previously established via `subscribe-all!`."
  [id]
  (locking conn-registry
    (vswap! (:subscriptions conn-registry) dissoc id)
    (doseq [[relay-url read-conn-vol] @(:read-connections-vol conn-registry)]
      (when (:read? (domain/find-relay relay-url))     
        (unsubscribe! read-conn-vol id)))))

(defn- connect-for-write!*
  [conn-registry relay-url]
  (locking conn-registry
    (let [deferred-conn (d/deferred)
          {:keys [write-connections-vol]} conn-registry
          _ (vswap! write-connections-vol assoc relay-url deferred-conn)]
      (->
        (websocket-client* relay-url)
        ;; Timeout without a timeout-val produces an d/error! that is handled
        ;; by the d/catch below.
        (d/timeout! (* connect-timeout-secs 1000))
        (d/chain
          (util/wrap-exc-fn
            (fn [raw-conn]
              (d/success! deferred-conn raw-conn)
              ;; If the connection is closed, remove the relay from the write connections.
              (s/on-closed raw-conn
                           (fn []
                             (locking conn-registry
                               (vswap! write-connections-vol dissoc relay-url))))
              :unused)))
        (d/catch (fn [err] (d/error! deferred-conn err)))))))

(defn send!*
  "Send an event to a relay, either using an existing persistent connection or creating
  a connection on demand. (We keep persistent connections to relays that are marked
  as read, but we don't for write-only relays so these will get opened and closed on
  demand.)"
  [event-obj to-relay-url]
  (let [deferred-result (-> (d/deferred)
                          (d/timeout! (* send-timeout-secs 1000)))]
    (locking conn-registry
      (let [read-connections @(:read-connections-vol conn-registry)]
        ;; If we have a read-connection we'll use it; if not, we will
        ;; attempt to reuse a write-conn (or create one on demand); we
        ;; do not yet discard write conns neither immediately or after
        ;; some time duration - something we may wish to do in the future
        ;; just to not keep infrequently used write connections open.
        (let [use-deferred-conn
              (if-let [read-conn-vol (get read-connections to-relay-url)]
                (:deferred-conn @read-conn-vol)
                ;; Otherwise get or create a deferred conn for writing.
                (or
                  (get @(:write-connections-vol conn-registry) to-relay-url)
                  (connect-for-write!* conn-registry to-relay-url)))]
          (d/chain use-deferred-conn
            (fn [raw-conn]
              (->
                (s/put! raw-conn
                  (json*/write-str* ["EVENT" event-obj]))
                (d/chain
                  (fn [_]
                    (log/info "succeeded with" to-relay-url)
                    (d/success! deferred-result :success!)))
                (d/catch
                  (fn [err]
                    (d/error! deferred-result err)))))))))
    deferred-result))

(defn connected-info
  "Return a map where the keys are a relay-url and the value is true iff
  the relay is sucessfully connected. (This is polled frequently by a scheduled
  recurring job and used to populate the green dots in the status bar.)"
  []
  (locking conn-registry
    (into {}
      (concat
        (map
          (fn [[relay-url read-conn-vol]]
            [relay-url (connected? read-conn-vol)])
          @(:read-connections-vol conn-registry))
        (map
          (fn [[relay-url deferred-conn]]
            [relay-url (connected?* deferred-conn)])
          @(:write-connections-vol conn-registry))))))


(defn overwrite-subscriptions!
  ([column since]
   ;; SINCE is a Java Instant.  
   ;; TODO: track a durable "watermark" for stable subscriptions.
   (when-not (empty? (:identities @domain/*state))
     (let [view (:view column)
           filters (subscribe/filters-for-view view (.getEpochSecond since))]
       #_(swap! domain/*state assoc :last-refresh (Instant/now))
       (log/debugf "Subscribing all for '%s'" (:name view))
       (subscribe-all! (format "flat:%s" (:id column))
                       filters)
       (log/info "overwrote subscriptions"))))
  
  ([column]
   (let [last-refresh (:last-refresh @domain/*state)
         since (or last-refresh (util/days-ago 7))]
     (overwrite-subscriptions! column since))))

(defn refresh! []
  (status-bar/message! "Refreshing subscriptions.")
  (doseq [c (:all-columns @domain/*state)]
    (overwrite-subscriptions! c)))

