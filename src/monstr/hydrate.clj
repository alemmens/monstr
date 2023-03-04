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


(defn new-timeline-pair [column-id]
  (let [flat-timeline (timeline/new-timeline)
        thread-timeline (timeline/new-timeline)
        flat-listview (view-home/create-list-view column-id
                                                  domain/*state store/db
                                                  metadata/cache
                                                  domain/daemon-scheduled-executor)                     
        thread-listview (view-home/create-thread-view column-id
                                                      domain/*state store/db
                                                      metadata/cache
                                                      domain/daemon-scheduled-executor)]
    (domain/->TimelinePair flat-timeline thread-timeline
                           flat-listview thread-listview)))
(defn- new-timelines-map
  [column-id pubkeys]
  (log/debugf "New timelines map with pubkeys=%s" (pr-str pubkeys))
  (into {}
        (map (fn [pubkey] [pubkey (new-timeline-pair column-id)])
             pubkeys)))

(defn new-column
  [view identities]
  (log/debugf "New column for %s" (pr-str view))
  (let [id (.toString (UUID/randomUUID))
        column (domain/->Column id view nil false nil)]
    (assoc column
           :identity->timeline-pair (new-timelines-map id (map :public-key identities)))))

(defn- hydrate-contact-lists!
  [new-identities]
  (let [contact-lists (store/load-contact-lists store/db (mapv :public-key new-identities))]
    (status-bar/message! (format "Loaded %d contact lists for %s"
                                 (count contact-lists)
                                 new-identities))
    (swap! domain/*state update :contact-lists merge contact-lists)
    contact-lists))

(defn dispatch-text-notes
  "If COLUMN is a string, the notes are only dispatched to the specified column.
  Otherwise they are dispatched to all columns."
  [*state relay-url events column]
  ;; TODO consider transduce iterate over timeline-data and throttling
  ;;      dispatches via yielding of bg thread; this way we'd move
  ;;      on to subscriptions, allowing new stuff to come in sooner
  ;;      as we backfill
  (status-bar/message! (format "Dispatching %d text notes from %s to column %s"
                               (count events) relay-url (:id column)))
  (doseq [event-obj events]
    (timeline/dispatch-text-note! *state
                                  (if (string? column)
                                    (:id column)
                                    false)
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
    (swap! *state assoc
           :identities (distinct (concat (:identities @*state) new-identities)))
    (swap! *state update
           :identity-metadata merge identity-metadata)
    (when-let [first-identity-key (first new-public-keys)]
      (log/debugf "Hydrating with first identity key %s" first-identity-key)
      (timeline/update-active-timelines! *state first-identity-key))    
    (let [contact-lists (hydrate-contact-lists! new-identities)
          ;; TODO: Make sure that user follow lists for all views are also in
          ;; this 'closure' list of public keys!
          closure-public-keys (subscribe/whale-of-pubkeys* new-public-keys contact-lists)]
      ;; TODO: also limit timeline events to something, some cardinality?
      ;; TODO: also load watermarks and include in new subscriptions.
      (doseq [r relay-urls]
        (let [events (store/load-relay-events db r closure-public-keys)]
          (status-bar/message! (format "Loaded %d events for %s from database"
                                       (count events)
                                       r))
          (fx/run-later (dispatch-text-notes *state r events false)))))
    ;; NOTE: use *all* identities to update subscriptions.
    (swap! domain/*state assoc :last-refresh false)
    (fx/run-later (subscribe/refresh!))))

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
  (let [view (:view column)
        relevant-pubkeys (subscribe/relevant-pubkeys-for-view view)]
    (doseq [r (:relay-urls view)]
      (let [events (store/load-relay-events store/db r relevant-pubkeys)]
        (status-bar/message! (format "Loaded %d events for %s from database"
                                     (count events)
                                     r))
        (dispatch-text-notes domain/*state r events column)))
    ;; TODO: pass relevant-pubkeys here so we don't have to recompute it
    ;; in overwrite-subscriptions!
    (subscribe/overwrite-subscriptions! column)))

(defn add-column-for-view! [view]
  (let [column (new-column view (:identities @domain/*state))]
    (swap! domain/*state assoc
           :all-columns (conj (:all-columns @domain/*state) column))
    (log/debugf "Added column %s for view %s"
                (:id column)
                (:name view))
    (timeline/update-column-timelines! column)))

(defn refresh-column! [column]
  (fx/run-later
   (log/debugf "Refreshing column %s with view %s"
               (:id column)
               (:name (:view column)))
   (timeline/clear-column! column false)
   (hydrate-column! column)))

