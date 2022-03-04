(ns me.untethr.nostr.hydrate
  (:require [me.untethr.nostr.util :as util]
            [me.untethr.nostr.store :as store]
            [me.untethr.nostr.timeline :as timeline]
            [me.untethr.nostr.subscribe :as subscribe]
            [clojure.tools.logging :as log])
  (:import (java.util.concurrent ScheduledExecutorService)))

(defn- hydrate-contact-lists!
  [*state db new-identities]
  (let [contact-lists (store/load-contact-lists db new-identities)]
    (swap! *state update :contact-lists merge contact-lists)
    contact-lists))

(defn hydrate!*
  ;; note: first of new-identities will become the active identity
  [*state db ^ScheduledExecutorService executor new-identities]
  (let [new-public-keys (mapv :public-key new-identities)
        identity-metadata (store/load-metadata db new-public-keys)
        identity-timeline (into {}
                            (map #(vector % (timeline/new-timeline))) new-public-keys)]
    (swap! *state
      (fn [curr-state]
        (-> curr-state
          (update :identities into new-identities)
          (update :identity-metadata merge identity-metadata)
          (update :identity-timeline merge identity-timeline))))
    (let [contact-lists (hydrate-contact-lists! *state db new-identities)
          closure-public-keys (subscribe/whale-of-pubkeys* new-public-keys contact-lists)
          ;; todo also limit timeline events to something, some cardinality?
          ;; todo also load watermarks and include in new subscriptions
          timeline-data (store/load-timeline-events db closure-public-keys)]
      (when-let [first-identity-key (first new-public-keys)]
        (timeline/update-active-timeline! *state first-identity-key))
      ;; todo consider transduce iterate over timeline-data and throttling
      ;;      dispatches via yielding of bg thread; this way we'd move
      ;;      on to subscriptions, allowing new stuff to come in sooner
      ;;      as we backfill
      (doseq [event-obj timeline-data]
        (timeline/dispatch-text-note! *state event-obj)))
    ;; note: use *all* identities to update subscriptions
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
                  ;; note: could result in nil new active-key:
                  (assoc :active-key (:public-key (first remaining-identities)))
                  true
                  (update :identity-metadata #(apply dissoc % dead-public-keys-set))
                  true
                  (update :identity-timeline #(apply dissoc % dead-public-keys-set))
                  true
                  (update :contact-lists #(apply dissoc % dead-public-keys-set))))))]
      (timeline/update-active-timeline! *state new-active-key)
      ;; todo note: this means we are resubscribing -- def should optimize w/ some kind of watermark strat.
      (subscribe/overwrite-subscriptions! new-identities new-contact-lists))))

(defn hydrate!
  [*state db ^ScheduledExecutorService executor new-identities]
  (util/submit! executor
    #(hydrate!* *state db ^ScheduledExecutorService executor new-identities)))

(defn dehydrate!
  [*state db ^ScheduledExecutorService executor dead-identities]
  (util/submit! executor
    #(dehydrate!* *state db ^ScheduledExecutorService executor dead-identities)))
