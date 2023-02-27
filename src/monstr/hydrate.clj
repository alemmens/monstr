(ns monstr.hydrate
  (:require
   [cljfx.api :as fx]   
   [monstr.timeline :as timeline]
   [monstr.domain :as domain]
   [monstr.util :as util]
   [monstr.store :as store]
   [monstr.metadata :as metadata]
   [monstr.status-bar :as status-bar]
   [monstr.subscribe :as subscribe]
   [monstr.view-home :as view-home]
   [clojure.tools.logging :as log])
  (:import (java.util.concurrent ScheduledExecutorService)
           (java.util UUID)))

(defn new-column
  [view]
  (log/debugf "New column for %s" (pr-str view))
  (let [id (.toString (UUID/randomUUID))
        column (domain/->Column id
                                view
                                (timeline/new-timeline (:relay-urls view) false)
                                (timeline/new-timeline (:relay-urls view) true)
                                nil nil false nil)]
    (assoc column
           :flat-listview (view-home/create-list-view id domain/*state store/db
                                                      metadata/cache
                                                      domain/daemon-scheduled-executor)
           :thread-listview (view-home/create-thread-view id domain/*state store/db
                                                          metadata/cache
                                                          domain/daemon-scheduled-executor))))


(defn- hydrate-contact-lists!
  [new-identities]
  (let [contact-lists (store/load-contact-lists store/db new-identities)]
    (status-bar/message! "Loading contact lists")
    (swap! domain/*state update :contact-lists merge contact-lists)
    contact-lists))

(defn dispatch-text-notes
  [*state relay-url events]
  ;; TODO consider transduce iterate over timeline-data and throttling
  ;;      dispatches via yielding of bg thread; this way we'd move
  ;;      on to subscriptions, allowing new stuff to come in sooner
  ;;      as we backfill
  (status-bar/message! (format "Dispatching %d text notes from %s" (count events) relay-url))
  (doseq [event-obj events]
    (timeline/dispatch-text-note! *state
                                  false
                                  (assoc event-obj :relays (list relay-url))
                                  false
                                  false)))

(defn hydrate!*
  ;; The first of new-identities will become the active identity.
  [*state db new-identities]
  ;; TODO: consider transduce iterate over timeline-data and throttling dispatches via
  ;; yielding of bg thread; this way we'd move on to subscriptions, allowing new stuff to
  ;; come in sooner as we backfill.
  (log/debugf "Hydrating with %d new identities" (count new-identities))
  (let [new-public-keys (mapv :public-key new-identities)
        identity-metadata (store/load-metadata db new-public-keys)
        relay-urls (domain/relay-urls @*state)]
    (swap! *state
      (fn [curr-state]
        (-> curr-state
            (update :identities into new-identities)
            (update :identity-metadata merge identity-metadata))))
    (when-let [first-identity-key (first new-public-keys)]
      (log/debugf "Hydrating with first identity key %s" first-identity-key)
      (timeline/update-active-timelines! *state first-identity-key))    
    (let [contact-lists (hydrate-contact-lists! new-identities)
          closure-public-keys (subscribe/whale-of-pubkeys* new-public-keys contact-lists)]
      ;; TODO: also limit timeline events to something, some cardinality?
      ;; TODO: also load watermarks and include in new subscriptions.
      (doseq [r relay-urls]
        (let [events (store/load-relay-events db r closure-public-keys)]
          (status-bar/message! (format "Loaded %d events for %s from database"
                                       (count events)
                                       r))
          (dispatch-text-notes *state r events))))
    ;; NOTE: use *all* identities to update subscriptions.
    (subscribe/refresh!)))

#_
(defn dehydrate!*
  [*state _db ^ScheduledExecutorService _executor dead-identities]
  (let [dead-public-keys-set (into #{} (map :public-key) dead-identities)]
    (let [{new-active-key :active-key
           new-identities :identities
           new-contact-lists :contact-lists}
          (swap! *state
            (fn [{curr-active-key :active-key :as curr-state}]
              (let [{remaining-identities :identities :as curr-state'}
                    (update curr-state :identities
                            #(remove (comp dead-public-keys-set :public-key) %))
                    curr-active-key-is-still-alive?
                    (some #(= (:public-key %) curr-active-key) remaining-identities)]
                (cond-> curr-state'
                  (not curr-active-key-is-still-alive?)
                  ;; NOTE: could result in nil new active-key:
                  (assoc :active-key (:public-key (first remaining-identities)))
                  true
                  (update :identity-metadata #(apply dissoc % dead-public-keys-set))
                  true
                  ;; TO DO: FIX THIS!
                  (update :identity->columns #(apply dissoc % dead-public-keys-set))
                  true
                  (update :contact-lists #(apply dissoc % dead-public-keys-set))))))]
      (timeline/update-active-timelines! *state new-active-key)
      ;; TODO note: this means we are resubscribing -- def should optimize w/ some kind of
      ;; watermark strategy.
      (subscribe/overwrite-subscriptions! new-identities new-contact-lists))))

(defn hydrate! [*state db executor new-identities]
  (util/submit! executor
                #(hydrate!* *state db new-identities)))

#_
(defn dehydrate! [*state db executor dead-identities]
  (util/submit! executor
                #(dehydrate!* *state db dead-identities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hydrating per column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- hydrate-column! [column]
  (log/debugf "Hydrating column '%s' with view '%s' and relays %s"
              (:id column)
              (:name (:view column))
              (:relay-urls (:view column)))
  (let [identities (:identities @domain/*state)
        new-public-keys (mapv :public-key identities)
        contact-lists (hydrate-contact-lists! identities)
        closure-public-keys (subscribe/whale-of-pubkeys* new-public-keys contact-lists)]
    (doseq [r (:relay-urls (:view column))]
      (let [events (store/load-relay-events store/db r closure-public-keys)]
        (status-bar/message! (format "Loaded %d events for %s from database"
                                     (count events)
                                     r))
        ;; TODO: Optimize by only dispatching to the given column.
        (dispatch-text-notes domain/*state r events)))
    (subscribe/overwrite-subscriptions! identities contact-lists column)))

(defn add-column-for-view! [view]
  (let [column (new-column view)]
    (swap! domain/*state assoc
           :all-columns (conj (:all-columns @domain/*state) column))
    (timeline/update-column-timelines! column)))

(defn refresh-column! [column]
  (fx/run-later
   (timeline/clear-column! column false)
   (hydrate-column! column)))

