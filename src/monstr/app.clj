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
   [monstr.view-home-new :as view-home-new]
   [monstr.event :as ev]
   [clojure.tools.logging :as log]
   [monstr.util :as util]
   [monstr.domain :as domain])
  (:import
    (java.util.concurrent ThreadFactory Executors ScheduledExecutorService TimeUnit))
  (:gen-class))

(defonce metadata-cache (metadata/create-cache store/db))

(defonce ^ScheduledExecutorService daemon-scheduled-executor
  (let [factory (reify ThreadFactory
                  (newThread [_ runnable]
                    (let [thread-name "nostr-desk-scheduled-executor-thread"]
                      (doto (Thread. runnable thread-name)
                        (.setDaemon true)))))]
    (Executors/newSingleThreadScheduledExecutor factory)))

(defonce *state
  (atom
    (domain/initial-state)))

(defn make-home []
  (view-home-new/create-list-view *state store/db metadata-cache daemon-scheduled-executor))

(defn init-homes!
  "Create home timelines for the first 3 relays."
  []
  (swap! *state assoc
         :homes (into {}
                      (map #(vector #{%} (make-home))
                           (take 3 (domain/relay-urls @*state))))))
  
(defn- load-relays!
  []
  (let [relays (store/load-relays store/db)]
    (swap! *state assoc :relays relays :refresh-relays-ts (System/currentTimeMillis))
    (log/debugf "Loaded %d relays." (count relays))))

(defn- update-relays! []
  (relay-conn/update-relays! (:relays @*state)))

(defn- load-identities!
  []
  (let [identities (store/load-identities store/db)]
    (log/debugf "Loaded %d identities." (count identities))
    (hydrate/hydrate! *state store/db daemon-scheduled-executor identities)))

(defn- update-connected-info!
  []
  (let [connected-info (relay-conn/connected-info)]
    (swap! *state assoc :connected-info connected-info)))

(defn fg-effect [f dispatch!]
  (fx/on-fx-thread
    (f *state store/db dispatch!)))

(defn bg-effect [f dispatch!]
  (.submit daemon-scheduled-executor
    ^Runnable
    (fn []
      (try
        (f *state store/db daemon-scheduled-executor #(fx/on-fx-thread (dispatch! %)))
        (catch Throwable t
          (log/error t "on bg"))))))

(defonce map-event-handler
  (-> ev/handle
      (fx/wrap-effects {:fg fg-effect :bg bg-effect})))

(defonce renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type view/stage :metadata-cache metadata-cache)
    :opts {:fx.opt/map-event-handler map-event-handler}))

(defn -main
  [& _]
  (load-relays!)
  (init-homes!)
  (fx/mount-renderer *state renderer)
  (consume/start! store/db *state metadata-cache daemon-scheduled-executor)
  (util/schedule! daemon-scheduled-executor load-identities! 1000)
  (util/schedule! daemon-scheduled-executor update-relays! 3000)
  (util/schedule-with-fixed-delay!
    daemon-scheduled-executor update-connected-info! 4000 10000)
  ;; CONSIDER shutdown hooks, graceful executor shutdown etc
  )

