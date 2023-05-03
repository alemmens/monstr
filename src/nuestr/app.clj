;; @see https://stackoverflow.com/questions/24254000/how-to-force-anti-aliasing-in-javafx-fonts
;; @see https://docs.oracle.com/javafx/2/api/javafx/scene/text/FontSmoothingType.html
(System/setProperty "prism.lcdtext" "false")
(System/setProperty "io.netty.noUnsafe" "true") ;; ...needed w/ latest netty?
#_(System/setProperty "cljfx.style.mode" "true") ; todo

(ns nuestr.app
  (:require
   [cljfx.api :as fx]
   [clojure.tools.logging :as log]
   [nuestr.channels :as channels]
   [nuestr.consume :as consume]
   [nuestr.domain :as domain]
   [nuestr.event :as ev]   
   [nuestr.file-sys :as file-sys]
   [nuestr.hydrate :as hydrate]
   [nuestr.metadata :as metadata]
   [nuestr.relay-conn :as relay-conn]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.tab-relays :as tab-relays]
   [nuestr.timeline :as timeline]
   [nuestr.view :as view]
   [nuestr.util :as util])
  (:gen-class))


(defn init-homes!
  "Create columns for all views (if there are no views yet, create a
  few)."
  []
  (let [urls (domain/relay-urls @domain/*state)
        views (or (file-sys/load-views)
                  (into {}
                        (map (fn [url] [url (domain/make-view url #{url} {})])
                             (take 3 urls))))
        identities (store/load-identities store/db)
        all-columns (map #(hydrate/new-column % identities)
                         (vals views))]
    (swap! domain/*state assoc
           :views views
           :all-columns all-columns
           :identities identities
           :active-key (or (file-sys/load-active-key)
                           (:public-key (first identities)))
           :visible-column-ids (or (file-sys/load-visible-columns)
                                   (map :id (take 3 all-columns))))))

(defn- load-relays!
  []
  (let [db-relays (seq (store/load-relays store/db))
        relays (or db-relays
                   (file-sys/load-relay-defaults))]
    ;; Add relays to the database if we don't have any yet.
    (when-not db-relays
      (store/replace-relays! store/db relays))
    ;;
    (swap! domain/*state assoc
           :relays (tab-relays/sort-relays relays :read? nil)
           :relays-sorted-by :read?
           :refresh-relays-ts (System/currentTimeMillis))
    (status-bar/message! (format "Loaded %d relays." (count relays)))))

(defn- update-relays! []
  (relay-conn/update-relays! (:relays @domain/*state)))

(defn- load-identities! []
  (let [identities (store/load-identities store/db)]
    (log/debugf "Loaded %d identities." (count identities))
    (hydrate/hydrate! domain/*state store/db domain/daemon-scheduled-executor identities)
    (timeline/maybe-add-open-profile-state! (:active-key @domain/*state))))

(defn- load-channels! []
  (let [channels (store/load-channels store/db)]
    (log/debugf "Loaded %d channels." (count channels))
    (dorun (map #(channels/update! (:id %) %)
                channels))))

(defn- update-connected-info! []
  (let [connected-info (relay-conn/connected-info)]
    (swap! domain/*state assoc :connected-info connected-info)))

(defn fg-effect [f dispatch!]
  (fx/on-fx-thread
    (f domain/*state store/db dispatch!)))

(defn bg-effect [f dispatch!]
  (.submit domain/daemon-scheduled-executor
    ^Runnable
    (fn []
      (try
        (f domain/*state store/db domain/daemon-scheduled-executor #(fx/on-fx-thread (dispatch! %)))
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
  (load-channels!)
  (log/debugf "Loaded %d identities" (count (:identities @domain/*state)))
  (fx/mount-renderer domain/*state renderer)
  (consume/start! store/db domain/*state metadata/cache domain/daemon-scheduled-executor)
  (util/schedule! domain/daemon-scheduled-executor load-identities! 1000)
  (util/schedule! domain/daemon-scheduled-executor update-relays! 3000)
  (util/schedule-with-fixed-delay!
   domain/daemon-scheduled-executor
   relay-conn/update-server-recommendations! 4000 60000)
  (util/schedule-with-fixed-delay!
   domain/daemon-scheduled-executor update-connected-info! 5000 20000)  
  ;; CONSIDER shutdown hooks, graceful executor shutdown etc
  )

