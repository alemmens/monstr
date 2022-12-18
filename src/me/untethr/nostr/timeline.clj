(ns me.untethr.nostr.timeline
  (:require
    [cljfx.api :as fx]
    [clojure.set :as set]
    [clojure.tools.logging :as log]
    [me.untethr.nostr.domain :as domain]
    [me.untethr.nostr.parse :as parse]
    [me.untethr.nostr.timeline-support :as timeline-support]
    [me.untethr.nostr.util :as util]
    [me.untethr.nostr.util-java :as util-java])
  (:import (javafx.collections FXCollections ObservableList)
           (java.util HashMap HashSet Set)
           (me.untethr.nostr.domain UITextNote UITextNoteWrapper)
           (javafx.scene.control ListView)
           (javafx.collections.transformation FilteredList)))

(defrecord Timeline
  ;; these field values are only ever mutated on fx thread
  [^ObservableList adapted-list
   ^ObservableList observable-list ;; contains UITextNoteWrapper
   ^HashMap author-pubkey->item-id-set
   ^HashMap item-id->index
   ^HashSet item-ids
   timeline-epoch-vol])

(defn new-timeline
  []
  ;; NOTE: we're querying and subscribing to all of time but for now, for ux
  ;; experience, we filter underlying data by n days
  ;; todo we'll really wish to query/subscribe at an epoch and only update it on scroll etc.
  (let [init-timeline-epoch (-> (util/days-ago 20) .getEpochSecond)
        timeline-epoch-vol (volatile! init-timeline-epoch)
        observable-list (FXCollections/observableArrayList)
        filtered-list (FilteredList. observable-list
                        (util-java/->Predicate #(> (:max-timestamp %) init-timeline-epoch)))
        adapted-list (.sorted
                       filtered-list
                       ;; latest wrapper entries first:
                       (comparator #(< (:max-timestamp %2) (:max-timestamp %1))))]
    (->Timeline
      adapted-list
      observable-list
      (HashMap.)
      (HashMap.)
      (HashSet.)
      timeline-epoch-vol)))

(defn accept-text-note?
  [*state identity-pubkey {:keys [pubkey] :as event-obj}]
  (let [{:keys [contact-lists]} @*state
        {:keys [parsed-contacts] :as _contact-list} (get contact-lists identity-pubkey)
        ;; consider: optimization--not having to create contact set each note
        contact-keys-set (into #{} (map :public-key) parsed-contacts)
        ptag-keys-set (set (parse/parse-ptag-keys* event-obj))]
    (or
      ;; identity's own note
      (= pubkey identity-pubkey)
      ;; the text-note's pubkey matches an identity's contact
      (contact-keys-set pubkey)
      ;; the text-note's ptags reference identity itself
      (ptag-keys-set identity-pubkey)
      ;; the text-note's ptags references one of identities contacts
      (not-empty (set/intersection contact-keys-set ptag-keys-set)))))

(defn dispatch-metadata-update!
  [*state {:keys [pubkey] :as _event-obj}]
  (fx/run-later
    (let [{:keys [identity-timeline]} @*state]
      (doseq [[_identity-pubkey timeline] identity-timeline]
        (let [{:keys [^ObservableList observable-list
                      ^HashMap author-pubkey->item-id-set
                      ^HashMap item-id->index]} timeline]
          (doseq [item-id (seq (.get author-pubkey->item-id-set pubkey))]
            (when-let [item-idx (.get item-id->index item-id)]
              (let [curr-wrapper (.get observable-list item-idx)]
                ;; todo why doesn't this refresh timeline immediately?
                (.set observable-list item-idx
                  (assoc curr-wrapper :touch-ts (System/currentTimeMillis)))))))))))

(defn dispatch-text-note!
  [*state {:keys [id pubkey created_at content] :as event-obj}]
  {:pre [(some? pubkey)]}
  ;; CONSIDER if is this too much usage of on-fx-thread - do we need to batch/debounce
  (fx/run-later
    (let [{:keys [identity-timeline] :as _state-snap} @*state]
      (doseq [[identity-pubkey timeline] identity-timeline]
        (let [{:keys [^ObservableList observable-list
                      ^HashMap author-pubkey->item-id-set
                      ^HashMap item-id->index
                      ^HashSet item-ids]} timeline]
          (when-not (.contains item-ids id)
            (when (or
                    ;; our item-id->index map will have a key for any id that
                    ;; has been referenced by any other accepted text note.
                    ;; so we also want to accept those "missing" notes:
                    (.containsKey item-id->index id)
                    (accept-text-note? *state identity-pubkey event-obj))
              (.add item-ids id)
              (.merge author-pubkey->item-id-set pubkey (HashSet. [id])
                (util-java/->BiFunction (fn [^HashSet acc id] (doto acc (.addAll ^Set id)))))
              (let [etag-ids (parse/parse-etag-ids* event-obj) ;; order matters
                    id-closure (cons id etag-ids)
                    existing-idx (first (keep #(.get item-id->index %) id-closure))]
                (if (some? existing-idx)
                  (let [curr-wrapper (.get observable-list existing-idx)
                        new-wrapper (timeline-support/contribute!
                                      curr-wrapper event-obj etag-ids)]
                    (doseq [x id-closure]
                      (.put item-id->index x existing-idx))
                    (.set observable-list existing-idx new-wrapper))
                  (let [init-idx (.size observable-list)
                        init-wrapper (timeline-support/init! event-obj etag-ids)]
                    (doseq [x id-closure]
                      (.put item-id->index x init-idx))
                    (.add observable-list init-wrapper)))))))))))

(defn toggle!
  [*state id]
  (let [{:keys [active-key identity-timeline]} @*state
        ^Timeline active-timeline (get identity-timeline active-key)
        {:keys [^ObservableList observable-list
                ^HashMap item-id->index]} active-timeline]
    (fx/run-later
      (try
        (when-let [item-index (.get item-id->index id)]
          (when-let [^UITextNoteWrapper wrapper (.get observable-list item-index)]
            (let [new-wrapper (update wrapper :expanded? #(not %))]
              (.set observable-list item-index new-wrapper))))
        (catch Exception e
          (log/error 'toggle! e))))))

(defn update-active-timeline!
  [*state public-key] ;; note public-key may be nil!
  (fx/run-later
    (swap! *state
      (fn [{:keys [^ListView home-ux identity-timeline] :as curr-state}]
        (.setItems home-ux
          ^ObservableList (or
                            (:adapted-list (get identity-timeline public-key))
                            (FXCollections/emptyObservableList)))
        (assoc curr-state :active-key public-key)))))
