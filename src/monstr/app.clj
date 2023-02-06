;; @see https://stackoverflow.com/questions/24254000/how-to-force-anti-aliasing-in-javafx-fonts
;; @see https://docs.oracle.com/javafx/2/api/javafx/scene/text/FontSmoothingType.html
(System/setProperty "prism.lcdtext" "false")
(System/setProperty "io.netty.noUnsafe" "true") ;; ...needed w/ latest netty?
;(System/setProperty "cljfx.style.mode" "true") ;; todo
(ns monstr.app
  (:require
   [cljfx.api :as fx]
   [monstr.consume :as consume]
   [monstr.file-sys :as file-sys]
   [monstr.hydrate :as hydrate]
   [monstr.metadata :as metadata]
   [monstr.relay-conn :as relay-conn]
   [monstr.store :as store]
   [monstr.view :as view]
   [monstr.view-home :as view-home]
   [monstr.view-home-new :as view-home-new]
   [monstr.event :as ev]
   [clojure.tools.logging :as log]
   [monstr.util :as util]
   [monstr.domain :as domain])
  (:import
    (java.util.concurrent ThreadFactory Executors ScheduledExecutorService TimeUnit))
  (:gen-class))

(defonce db (store/init! (file-sys/db-path)))

(defonce metadata-cache (metadata/create-cache db))

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

(defonce home-ux
  (view-home/create-list-view *state db metadata-cache daemon-scheduled-executor))

(defonce home-ux-new
  (view-home-new/create-list-view *state db metadata-cache daemon-scheduled-executor))

(swap! *state assoc :home-ux home-ux)
(swap! *state assoc :home-ux-new home-ux-new)

(defn- load-relays!
  []
  (let [relays (store/load-relays db)]
    (swap! *state assoc :relays relays :refresh-relays-ts (System/currentTimeMillis))
    (log/debugf "Loaded %d relays." (count relays))
    (relay-conn/update-relays! relays)))

(defn- load-identities!
  []
  (let [identities (store/load-identities db)]
    (log/debugf "Loaded %d identities." (count identities))
    (hydrate/hydrate! *state db daemon-scheduled-executor identities)))

(defn- update-connected-info!
  []
  (let [connected-info (relay-conn/connected-info)]
    (swap! *state assoc :connected-info connected-info)))

(defn fg-effect [f dispatch!]
  (fx/on-fx-thread
    (f *state db dispatch!)))

(defn bg-effect [f dispatch!]
  (.submit daemon-scheduled-executor
    ^Runnable
    (fn []
      (try
        (f *state db daemon-scheduled-executor #(fx/on-fx-thread (dispatch! %)))
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
  (fx/mount-renderer *state renderer)
  (consume/start! db *state metadata-cache daemon-scheduled-executor)
  (util/schedule! daemon-scheduled-executor load-identities! 1000)
  (util/schedule! daemon-scheduled-executor load-relays! 3000)
  (util/schedule-with-fixed-delay!
    daemon-scheduled-executor update-connected-info! 4000 10000)
  ;; CONSIDER shutdown hooks, graceful executor shutdown etc
  )

