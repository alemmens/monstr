(ns nuestr.util
  (:require [clojure.tools.logging :as log]
            [nuestr.metadata :as metadata])
  (:import (java.util Random Date)
           (java.text SimpleDateFormat)
           (java.util.function BiFunction)
           (java.util.concurrent ScheduledExecutorService TimeUnit)
           (java.time ZonedDateTime Instant)
           (java.awt Desktop)
           (java.net URI)
           (javafx.scene.input Clipboard ClipboardContent)))

(defn rand-hex-color
  [seed]
  (let [random (Random. seed)]
    (apply (partial str "#")
      (map
        #(if (> % 9) (char (+ (int \a) (- % 10))) %)
        (repeatedly 6 #(.nextInt random 16))))))

(defn wrap-exc-fn
  (^Runnable [f]
   (wrap-exc-fn nil f))
  (^Runnable [context f]
   (fn [& args]
     (try
       (apply f args)
       (catch Throwable t
         (log/error t (or context "<no-context>")))))))

(defn compact
  [m]
  (into {}
    (filter (fn [[_ v]] (and (some? v) (or (not (coll? v)) (not (empty? v))))))
    m))

(defn dissoc-in
  "dissoc-in is a mixture of assoc-in and dissoc. It lets us remove an entry from a nested
  associative structure using a path of keys."
  [m [k & knext :as ks]]
  (cond
    (and knext
      (contains?
        (get-in m (butlast ks))
        (last ks))) (update-in m (butlast ks) dissoc (last ks))
    (not knext) (dissoc m k)
    :else m))

(defn format-pubkey-short
  [pubkey]
  (str (subs pubkey 0 3) "..." (subs pubkey (- (count pubkey) 4))))

(defn name-for-pubkey [pubkey metadata-cache]
  (or (when metadata-cache
        (:name (metadata/get* metadata-cache pubkey)))
      (format-pubkey-short pubkey)))

(defn format-event-id-short
  [event-id]
  (str (subs event-id 0 8) "..." (subs event-id (- (count event-id) 8))))

(defn format-timestamp
  [^long epoch-seconds]
  (let [sdt (SimpleDateFormat. "yyyy LLL dd h:mm a")]
    (.format sdt (Date. (long (* 1000 epoch-seconds))))))

(defn days-ago
  ^Instant [n]
  (-> (ZonedDateTime/now)
    (.minusDays n)
    .toInstant))

(defn now-epoch-second ^long []
  (-> (Instant/now) .getEpochSecond))

(defn concatv
  [& colls]
  (vec (apply concat colls)))

(defn schedule!
  ([^ScheduledExecutorService executor ^Runnable f ^long delay]
   (schedule! executor f delay nil))
  ([^ScheduledExecutorService executor ^Runnable f ^long delay context]
   (.schedule executor (wrap-exc-fn context f) delay TimeUnit/MILLISECONDS)))

(defn schedule-with-fixed-delay!
  [^ScheduledExecutorService executor ^Runnable f ^long initial-delay ^long delay]
  (.scheduleWithFixedDelay executor (wrap-exc-fn f) initial-delay delay TimeUnit/MILLISECONDS))

(defn submit!
  [^ScheduledExecutorService executor ^Runnable f]
  (.submit executor (wrap-exc-fn f)))

(defn open-url!
  [^String url]
  (try
    (.browse (Desktop/getDesktop) (URI. url))
    (catch Exception e
      (log/error 'open-url! (type e) (ex-message e)))))

(defn put-clipboard! [^String content]
  (-> (Clipboard/getSystemClipboard)
    (.setContent
      (doto (ClipboardContent.)
        (.putString content)))))
