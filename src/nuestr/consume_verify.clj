(ns monstr.consume-verify
  (:require
    [clojure.tools.logging :as log]
    [monstr.cache :as cache]
    [monstr.store :as store]
    [monstr.x.crypt :as crypt]
    [monstr.json :as json]
    [next.jdbc :as jdbc])
  (:import (com.google.common.cache Cache)
           (java.nio.charset StandardCharsets)))

;; todo batch db writes
;; todo consume-event/calc- exception handling
;; todo fully test

(defn- calc-event-id
  [{:keys [pubkey created_at kind tags content]}]
  (-> [0 pubkey created_at kind tags content]
    json/write-str*
    (.getBytes StandardCharsets/UTF_8)
    crypt/sha-256
    crypt/hex-encode))

(defn- verify
  [public-key message signature]
  (crypt/verify
    (crypt/hex-decode public-key)
    (crypt/hex-decode message)
    (crypt/hex-decode signature)))

(defn- store-event!
  [db {:keys [id pubkey created_at kind tags content] :as _e} raw-event-tuple]
  ;; use a tx, for now; don't want to answer queries with events
  ;; that don't fully exist. could denormalize or some other strat
  ;; to avoid tx if needed
  (jdbc/with-transaction [tx db]
    (if-let [rowid (store/insert-event! tx id pubkey created_at kind content raw-event-tuple)]
      (do
        (doseq [[tag-kind arg0] tags]
          (condp = tag-kind
            "e" (store/insert-e-tag! tx id arg0)
            "p" (store/insert-p-tag! tx id arg0)
            :no-op))
        rowid))))

(defn verify-maybe-persist-event! [db ^Cache cache relay-url
                                   {:keys [pubkey id sig] :as event-obj}
                                   raw-event-tuple
                                   on-new-event
                                   on-new-relay-seen
                                   on-event-stored-but-not-cached]
  (let [relay-id-key [relay-url id]
        id-key [:id id]]
    (when-not (cache/cache-contains? cache relay-id-key)
      (if (store/contains-event-from-relay? db relay-url id)
        ;; -- then; we've seen this event from the relay but it's not in cache
        (do
          ;; consider: conditional puts (eg exclude old events)
          (cache/put! cache relay-id-key)
          (when-not (cache/cache-contains? cache id-key)
            (when-let [sig-from-db (store/event-signature-by-id db id)]
              (cache/put! cache [:id id] sig-from-db)))
          (on-event-stored-but-not-cached event-obj))
        ;; -- else (event not yet seen from relay - cache nor db)
        (let [calculated-event-id (calc-event-id event-obj)]
          (if (not= id calculated-event-id)
            (do
              (log/debug "bad id" id relay-url)) ;; consider: something else?
            ;; -- else (we have a good id)
            (if-let [sig-from-cache (cache/get-if-present cache id-key)]
              ;; -- then sig in cache
              (if (not= sig sig-from-cache)
                (do
                  (log/debug "bad sig" id relay-url)) ;; consider: something else?
                ;; -- else (we have good sig for a message we've seen, just not yet from this relay)
                (do
                  (store/contains-event-from-relay! db relay-url id) ;; consider: accumulating batching db writes -- these can be best-effort
                  ;; consider: conditional puts (eg exclude old events)
                  (cache/put! cache relay-id-key)
                  (on-new-relay-seen event-obj)))
              ;; -- else sig not in cache
              (if-let [sig-from-db (store/event-signature-by-id db id)]
                ;; -- then sig in db
                (if (not= sig sig-from-db)
                  (do
                    (log/debug "bad sig" id relay-url)) ;; consider: something else?
                  ;; -- good sig (per db)
                  (do
                    (store/contains-event-from-relay! db relay-url id) ;; consider: accumulating batching db writes -- these can be best-effort
                    ;; consider: conditional puts (eg exclude old events)
                    (cache/put! cache relay-id-key)
                    (on-new-relay-seen event-obj)))
                ;; -- else (id not in db/never seen message)
                (if (verify pubkey id sig)
                  ;; then -- verified
                  (do
                    (store-event! db event-obj raw-event-tuple)
                    (store/contains-event-from-relay! db relay-url id)
                    (store/event-signature! db id sig)
                    (cache/put! cache relay-id-key)
                    (cache/put! cache [:id id] sig)
                    (on-new-event event-obj))
                  ;; else -- failed to verify
                  (do
                    (log/debug "bad sig" id relay-url)) ;; consider: something else? consider: cache bad ids from relays too?
                  )))))))))
