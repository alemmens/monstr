;; @see https://stackoverflow.com/questions/24254000/how-to-force-anti-aliasing-in-javafx-fonts
;; @see https://docs.oracle.com/javafx/2/api/javafx/scene/text/FontSmoothingType.html
(System/setProperty "prism.lcdtext" "false")
(System/setProperty "io.netty.noUnsafe" "true") ;; ...needed w/ latest netty?
;(System/setProperty "cljfx.style.mode" "true") ;; todo
(ns monstr.app
  (:require
   [cljfx.api :as fx]
   [monstr.consume :as consume]
   [monstr.hydrate :as hydrate]
   [monstr.metadata :as metadata]
   [monstr.relay-conn :as relay-conn]
   [monstr.store :as store]
   [monstr.view :as view]
   [monstr.event :as ev]
   [clojure.tools.logging :as log]
   [monstr.util :as util]
   [monstr.domain :as domain])
  (:import
    (java.util.concurrent ThreadFactory Executors ScheduledExecutorService TimeUnit))
  (:gen-class))

(defonce ^ScheduledExecutorService daemon-scheduled-executor
  (let [factory (reify ThreadFactory
                  (newThread [_ runnable]
                    (let [thread-name "nostr-desk-scheduled-executor-thread"]
                      (doto (Thread. runnable thread-name)
                        (.setDaemon true)))))]
    (Executors/newSingleThreadScheduledExecutor factory)))


(defn init-homes!
  "Create home timelines three relays and views for all relays."
  []
  (let [urls (domain/relay-urls @domain/*state)]
    (swap! domain/*state assoc
           :relay-timelines (doall (take 3 urls))
           :views (into {}
                        (map (fn [url]
                               [url (domain/->View url #{url} #{} #{})])
                             urls)))))
  
(defn- load-relays!
  []
  (let [relays (store/load-relays store/db)]
    (swap! domain/*state assoc
           :relays relays
           :refresh-relays-ts (System/currentTimeMillis))
    (log/debugf "Loaded %d relays." (count relays))))

(defn- update-relays! []
  (relay-conn/update-relays! (:relays @domain/*state)))

(defn- load-identities!
  []
  (let [identities (store/load-identities store/db)]
    (log/debugf "Loaded %d identities." (count identities))
    (hydrate/hydrate! domain/*state store/db daemon-scheduled-executor identities)))

(defn- update-connected-info!
  []
  (let [connected-info (relay-conn/connected-info)]
    (swap! domain/*state assoc :connected-info connected-info)))

(defn fg-effect [f dispatch!]
  (fx/on-fx-thread
    (f domain/*state store/db dispatch!)))

(defn bg-effect [f dispatch!]
  (.submit daemon-scheduled-executor
    ^Runnable
    (fn []
      (try
        (f domain/*state store/db daemon-scheduled-executor #(fx/on-fx-thread (dispatch! %)))
        (catch Throwable t
          (log/error t "on bg"))))))

(defonce map-event-handler
  (-> ev/handle
      (fx/wrap-effects {:fg fg-effect :bg bg-effect})))

(defonce renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type view/stage :metadata-cache metadata/cache)
    :opts {:fx.opt/map-event-handler map-event-handler}))

(defn -main
  [& _]
  (load-relays!)
  (init-homes!)
  (fx/mount-renderer domain/*state renderer)
  (consume/start! store/db domain/*state metadata/cache daemon-scheduled-executor)
  (util/schedule! daemon-scheduled-executor load-identities! 1000)
  (util/schedule! daemon-scheduled-executor update-relays! 3000)
  (util/schedule-with-fixed-delay!
    daemon-scheduled-executor update-connected-info! 4000 10000)
  ;; CONSIDER shutdown hooks, graceful executor shutdown etc
  )

