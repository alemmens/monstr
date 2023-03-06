(ns nuestr.publish
  (:require
    [nuestr.x.crypt :as crypt]
    [nuestr.json :as json]
    [nuestr.util :as util]
    [manifold.deferred :as d]
    [nuestr.relay-conn :as relay-conn]
    [clojure.tools.logging :as log])
  (:import (java.nio.charset StandardCharsets)
           (java.security SecureRandom)))

(defonce ^SecureRandom secure-random (SecureRandom.))

(defn ->event-id
  [pubkey created_at kind tags content]
  (-> [0 pubkey created_at kind tags content]
    json/write-str*
    (.getBytes StandardCharsets/UTF_8)
    crypt/sha-256
    crypt/hex-encode))

(defn ->event
  [pubkey created_at kind tags content secret-key]
  (let [event-id (->event-id pubkey created_at kind tags content)
        aux-bytes (byte-array 32)
        _ (.nextBytes secure-random aux-bytes)
        sig (->
              (crypt/sign
                (crypt/hex-decode secret-key)
                (crypt/hex-decode event-id)
                aux-bytes)
              crypt/hex-encode)]
    {:id event-id
     :pubkey pubkey
     :created_at created_at
     :kind kind
     :tags tags
     :content content
     :sig sig}))

(defn- reply-context->tags
  [{:keys [root-event-id event-id] :as reply-context}]
  (cond
    (nil? reply-context) []
    (= root-event-id event-id) [["e" root-event-id]]
    ;; root event id always *first*
    :else [["e" root-event-id] ["e" event-id]]))

(defn publish-note!
  [pubkey secret-key content relays reply-context]
  (let [deferred-result (d/deferred)
        timestamp (util/now-epoch-second)
        tags (reply-context->tags reply-context)
        event-obj (->event pubkey timestamp 1 tags content secret-key)
        deferreds (map
                    #(relay-conn/send!* event-obj (:url %))
                    (filter :write? relays))]
    (if (empty? deferreds)
      (d/error! deferred-result :no-write-relays)
      (let [err-counter (atom (count deferreds))]
        (doseq [deferred deferreds]
          (-> deferred
            (d/chain
              (fn [_]
                ;; todo -
                ;; (1) for now - any success that we see will cause us to
                ;;   return success to upstream; we'll want to change this
                ;;   to use db as a queue to ensure message eventually is sent
                ;;   to all write-relays (or popped if write relays change?)
                ;; (2) for just write-relays we are not reporting any errors
                ;;     notices back to client user
                (d/success! deferred-result :first-success)))
            (d/catch
              (fn [_]
                (when-not (pos? (swap! err-counter dec))
                  (d/error! deferred-result :all-write-relays-failed))
                ))))))
    deferred-result))
