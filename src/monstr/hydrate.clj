(ns monstr.hydrate
  (:require
   [monstr.timeline :as timeline]
   [monstr.domain :as domain]
   [monstr.util :as util]
   [monstr.store :as store]
   [monstr.metadata :as metadata]
   [monstr.subscribe :as subscribe]
   [monstr.view-home-new :as view-home]
   [clojure.tools.logging :as log])
  (:import (java.util.concurrent ScheduledExecutorService)
           (java.util UUID)))

(defn- new-column
  [*state db executor metadata-cache relay-urls]
  (let [id (.toString (UUID/randomUUID))
        column (domain/->Column id
                                (domain/->View (first relay-urls) relay-urls)
                                (timeline/new-timeline relay-urls false)
                                (timeline/new-timeline relay-urls true)
                                nil nil false nil)]
    (assoc column
           :flat-listview (view-home/create-list-view id *state db metadata-cache executor)
           :thread-listview (view-home/create-thread-view id *state db metadata-cache executor))))

(defn- new-columns
  "Create a new Column for each relay-url."
  [*state db executor metadata-cache relay-urls]
  (log/debugf "Creating new columns for %s" relay-urls)
  (map #(new-column *state db executor metadata-cache #{%})
       relay-urls))

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
  (log/debugf "Dispatching %d text notes for %s" (count timeline-data) relay-url)
  (doseq [event-obj timeline-data]
    (timeline/dispatch-text-note! *state false
                                  (assoc event-obj :relays (list relay-url)))))

(defn hydrate!*
  ;; note: first of new-identities will become the active identity
  [*state db ^ScheduledExecutorService executor new-identities]
  (log/debugf "Hydrating with %d new identities" (count new-identities))
  (let [new-public-keys (mapv :public-key new-identities)
        identity-metadata (store/load-metadata db new-public-keys)
        relay-urls (domain/relay-urls @*state)
        identity->columns (into {}
                                (map #(vector % (new-columns *state db executor
                                                             metadata/cache
                                                             relay-urls))
                                     new-public-keys))]
    (swap! *state
      (fn [curr-state]
        (-> curr-state
            (update :identities into new-identities)
            (update :identity-metadata merge identity-metadata)
            (update :identity->columns merge identity->columns))))
    (let [contact-lists (hydrate-contact-lists! *state db new-identities)
          closure-public-keys (subscribe/whale-of-pubkeys* new-public-keys contact-lists)
          ;; TODO: also limit timeline events to something, some cardinality?
          ;; TODO: also load watermarks and include in new subscriptions.
          timelines-data (map #(store/load-relay-timeline-events db % closure-public-keys)
                              relay-urls)]
      (log/debugf "Loaded %s timeline events" (doall (map count timelines-data)))
      (when-let [first-identity-key (first new-public-keys)]
        (log/debugf "Hydrating with first identity key %s" first-identity-key)
        (timeline/update-active-timelines! *state first-identity-key))
      ;; TODO: consider transduce iterate over timeline-data and throttling
      ;;      dispatches via yielding of bg thread; this way we'd move
      ;;      on to subscriptions, allowing new stuff to come in sooner
      ;;      as we backfill
      (doseq [[relay-url timeline-data] (map list relay-urls timelines-data)]
        (dispatch-text-notes *state relay-url timeline-data)))
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
      ;; TODO note: this means we are resubscribing -- def should optimize w/ some kind of watermark strat.
      (subscribe/overwrite-subscriptions! new-identities new-contact-lists))))

(defn hydrate!
  [*state db ^ScheduledExecutorService executor new-identities]
  (util/submit! executor
    #(hydrate!* *state db ^ScheduledExecutorService executor new-identities)))

(defn dehydrate!
  [*state db ^ScheduledExecutorService executor dead-identities]
  (util/submit! executor
    #(dehydrate!* *state db ^ScheduledExecutorService executor dead-identities)))
