(ns monstr.hydrate
  (:require
   [monstr.timeline :as timeline]
   [monstr.domain :as domain]
   [monstr.util :as util]
   [monstr.store :as store]
   [monstr.metadata :as metadata]
   [monstr.subscribe :as subscribe]
   [monstr.view-home :as view-home]
   [clojure.tools.logging :as log])
  (:import (java.util.concurrent ScheduledExecutorService)
           (java.util UUID)))

(defn new-column
  [view executor]
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
                                                      executor)
           :thread-listview (view-home/create-thread-view id domain/*state store/db
                                                          metadata/cache
                                                          executor))))

(defn- hydrate-contact-lists!
  [*state db new-identities]
  (let [contact-lists (store/load-contact-lists db new-identities)]
    (swap! *state update :contact-lists merge contact-lists)
    contact-lists))

(defn dispatch-text-notes
  [*state relay-url timeline-data]
  ;; TODO consider transduce iterate over timeline-data and throttling
  ;;      dispatches via yielding of bg thread; this way we'd move
  ;;      on to subscriptions, allowing new stuff to come in sooner
  ;;      as we backfill
  (log/debugf "Dispatching %d text notes from %s" (count timeline-data) relay-url)
  (doseq [event-obj timeline-data]
    (timeline/dispatch-text-note! *state
                                  false
                                  (assoc event-obj :relays (list relay-url))
                                  false)))

(defn hydrate!*
  ;; The first of new-identities will become the active identity.
  [*state db ^ScheduledExecutorService executor new-identities]
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
    (let [contact-lists (hydrate-contact-lists! *state db new-identities)
          closure-public-keys (subscribe/whale-of-pubkeys* new-public-keys contact-lists)
          ;; TODO: also limit timeline events to something, some cardinality?
          ;; TODO: also load watermarks and include in new subscriptions.
          relay-events (map (fn [r]
                              [r
                               (store/load-relay-events db r closure-public-keys)])
                            relay-urls)]
      (log/debugf "Loaded %s relay events" (doall (map (comp count second) relay-events)))
      (when-let [first-identity-key (first new-public-keys)]
        (log/debugf "Hydrating with first identity key %s" first-identity-key)
        (timeline/update-active-timelines! *state first-identity-key))
      (doseq [[relay-url events] relay-events]
        (dispatch-text-notes *state relay-url events)))
    ;; NOTE: use *all* identities to update subscriptions.
    (let [{:keys [identities contact-lists]} @*state]
      (subscribe/overwrite-subscriptions! identities contact-lists))))

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
                  (update :identity->columns #(apply dissoc % dead-public-keys-set))
                  true
                  (update :contact-lists #(apply dissoc % dead-public-keys-set))))))]
      (timeline/update-active-timelines! *state new-active-key)
      ;; TODO note: this means we are resubscribing -- def should optimize w/ some kind of
      ;; watermark strategy.
      (subscribe/overwrite-subscriptions! new-identities new-contact-lists))))

(defn hydrate!
  [*state db ^ScheduledExecutorService executor new-identities]
  (util/submit! executor
    #(hydrate!* *state db ^ScheduledExecutorService executor new-identities)))

(defn dehydrate!
  [*state db ^ScheduledExecutorService executor dead-identities]
  (util/submit! executor
    #(dehydrate!* *state db ^ScheduledExecutorService executor dead-identities)))
