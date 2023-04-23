(ns nuestr.util
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
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

(defn relay-url-short [url]
  (cond (str/starts-with? url "wss://") (subs url 6)
        (str/starts-with? url "ws://")  (subs url 5)
        :else url))
  
(defn numerical-relay-url?
  "Returns true for relay urls like 'wss://123.456.789:4012'."
  [url]
  (every? #(re-matches #"\d+" %)
          (str/split (relay-url-short url) #"(\.|:)")))

(defn format-string-short [string]
  (if (> (count string) 15)
    (str (subs string 0 10)
         "..."
         (subs string (- (count string) 5)))
    string))

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

(defn insert-at-index [i elt sequence]
  (concat (take i sequence)
          (list elt)
          (drop i sequence)))

(defn position
  "Returns the position (index) of the first occurrence of `elt` in `sequence`
  (or nil if `elt` does not occur in `sequence`)."
  [elt sequence]
  (first (keep-indexed #(when (= %2 elt) %1) sequence)))

(defn update-in-sequence [old-element new-element sequence]
  (if-let [pos (position old-element sequence)]
    (insert-at-index pos new-element
                     (remove #{old-element} sequence))
    sequence))

(defn group-on
  "Returns a vector with two elements: [1] all elements of `coll` for which
  `pred` returns true, [2] all other elements of `coll`."
  [pred coll]
  (loop [yes '()
         no '()
         elts coll]
    (if (empty? elts)
      [yes no]
      (let [elt (first elts)]
        (if (pred elt)
          (recur (cons elt yes)
                 no
                 (rest elts))
          (recur yes
                 (cons elt no)
                 (rest elts)))))))

         
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
