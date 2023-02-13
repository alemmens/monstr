(ns monstr.event
  (:require
    [cljfx.api :as fx]
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [clojure.string :as str]
    [monstr.modal :as modal]
    [monstr.relay-conn :as relay-conn]
    [monstr.store :as store]
    [monstr.timeline-new :as timeline-new]
    [monstr.x.crypt :as crypt]
    [monstr.hydrate :as hydrate]
    [monstr.publish :as publish]
    [monstr.domain :as domain]
    [monstr.util :as util]
    [monstr.util-domain :as util-domain]
    [manifold.deferred :as d])
  (:import (javafx.scene.control DialogEvent Dialog Button TextArea)
           (javafx.event Event ActionEvent)))

(defn relay-defaults
  []
  (read-string (slurp (io/resource "monstr/relay-defaults.edn"))))

(defn click-keycard
  [{:keys [public-key]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (log/debugf "Click keycard for public key %s" public-key)
      (timeline-new/update-active-timelines! *state public-key))]])

(defn show-new-identity-effect
  [show-new-identity?]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (swap! *state
        (fn [curr-state]
          (cond-> (assoc curr-state :show-new-identity? show-new-identity?)
            (not show-new-identity?) (assoc :new-identity-error "")))))]])

(defn delete-keycard
  [{:keys [identity_]}]
  (when (modal/blocking-yes-no-alert "" "Are you sure?")
    [[:bg
      (fn [*state db executor _dispatch!]
        (store/delete-identity! db (:public-key identity_))
        (let [{curr-identities :identities} @*state]
          (when (some #(= (:public-key identity_) (:public-key %)) curr-identities)
            (hydrate/dehydrate! *state db executor [identity_]))))]]))

(defn maybe-contribute-secret-key*
  [*state public-key maybe-private-key]
  ;; this handles a special case where a new identity matches an existing
  ;; one but we're now getting its private-key.
  (when (some? maybe-private-key)
    (swap! *state
      (fn [{:keys [identities] :as curr-state}]
        (assoc curr-state :identities
                          (mapv (fn [identity-]
                                  (if (and
                                        (= public-key (:public-key identity-))
                                        (nil? (:secret-key identity-)))
                                    (assoc identity- :secret-key maybe-private-key)
                                    identity-)) identities))))))

(defn add-identity-and-close-dialog-effect
  [public-key maybe-private-key]
  (util/concatv
    (show-new-identity-effect false)
    [[:bg
      (fn [*state db executor _dispatch!]
        (store/insert-identity! db public-key maybe-private-key) ;; idempotent
        (maybe-contribute-secret-key* *state public-key maybe-private-key)
        (let [{curr-identities :identities} @*state]
          ;; don't hydrate identity if it's already hydrated
          (when-not (some #(= public-key (:public-key %)) curr-identities)
            (hydrate/hydrate! *state db executor
              [(domain/->Identity public-key maybe-private-key)]))))]]))

(defn new-identity-close-request
  [{^DialogEvent dialog-event :fx/event}]
  (let [^Dialog dialog (.getSource dialog-event)
        dialog-result (.getResult dialog)]
    (condp = dialog-result
      :cancel
      (show-new-identity-effect false)
      (let [{:keys [val public?]} dialog-result]
        (cond
          (not= (count val) 64)
          (do
            (.consume dialog-event) ;; prevents dialog closure
            [[:bg (fn [*state _db _exec _dispatch!]
                    (swap! *state assoc
                      :new-identity-error "Key must be 64 characters"))]])
          public?
          (add-identity-and-close-dialog-effect val nil)
          :else
          (if-let [corresponding-pubkey
                   (some-> val
                     crypt/hex-decode
                     crypt/generate-pubkey
                     crypt/hex-encode)]
            (add-identity-and-close-dialog-effect corresponding-pubkey val)
            (do
              (.consume dialog-event) ;; prevents dialog closure
              [[:bg (fn [*state _db _exec _dispatch!]
                      (swap! *state assoc
                        :new-identity-error "Bad private key"))]])))))))

(defn replace-relays-effect
  [new-relays show-relays?]
  [[:bg
    (fn [*state db _exec _dispatch!]
      (swap! *state
        assoc
        :relays (store/replace-relays! db new-relays)
        :show-relays? show-relays?
        :refresh-relays-ts (System/currentTimeMillis))
      (relay-conn/update-relays! new-relays))]])

(defn show-relays-effect
  [show-relays?]
  [[:fg
    (fn [*state db _]
      (swap! *state assoc
             :show-relays? show-relays?
             ;; this is especially necessary when user is close-cancelling; they
             ;; may have mutated some text fields; and this forces a complete
             ;; re-render of the text fields, wiping out their mutations.
             :refresh-relays-ts (System/currentTimeMillis)))]])

(defn relays-close-request
  [{^DialogEvent dialog-event :fx/event}]
  (let [dialog-result (.getResult ^Dialog (.getSource dialog-event))]
    (condp = dialog-result
      :restore
      (do
        (.consume dialog-event) ;; prevents dialog closure
        ;; modal answers nil if 'no' which upstream will interpret as no-op
        (when (modal/blocking-yes-no-alert "" "Are you sure?")
          (replace-relays-effect (relay-defaults) true)))
      :cancel
      (show-relays-effect false)
      (replace-relays-effect dialog-result false))))

(defn publish-effect
  [content reply? success-callback]
  ;; not for now effect is empty/no-op if content is blank
  (when-not (str/blank? content)
    [[:bg
      (fn [*state _db _exec dispatch!]
        (let [{:keys [active-key identities relays active-reply-context]} @*state
              ;; safety: if reply?=false we should not have an active-reply-context anyway:
              use-active-reply-context (when reply? active-reply-context)]
          (when-let [secret-key (util-domain/->secret-key* active-key identities)]
            (-> (publish/publish-note! active-key secret-key content relays use-active-reply-context)
              (d/chain
                (fn [_]
                  (success-callback *state)))
              (d/catch
                (fn [e]
                  (log/error e "error sending")))))))]]))

(defn publish!
  [{^ActionEvent event :fx/event}]
  (let [^Button target (.getTarget event)
        ^TextArea found (.lookup (.getScene target) ".ndesk-publish-box")
        content (.getText found)]
    ;; the idea here is that we only clear our publish text box if we're
    ;; successful - otherwise text remains (we still do need better ux feedback
    ;; in this case tho)
    (publish-effect content false (fn [& _] (fx/run-later (.clear found))))))

(defn hide-reply-box* [*state]
  (swap! *state dissoc :active-reply-context))

(defn hide-reply-box-effect []
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (hide-reply-box* *state))]])

(defn reply!
  [{^DialogEvent dialog-event :fx/event}]
  (let [dialog-result (.getResult ^Dialog (.getSource dialog-event))]
    (condp = dialog-result
      :cancel
      (hide-reply-box-effect)
      (let [{:keys [content success-callback]} dialog-result]
        ;; okay -- we consume dialog event -- this means from this point
        ;; on we are fully in control of hiding the box -- we want this control
        ;; b/c we'll close on success but not on publish failure
        (.consume dialog-event) ;; !
        (publish-effect content true
          (fn [*state]
            ;; only on success we will close the reply box -- otherwise
            ;; for now assume error and frustrated user will have to cancel
            ;; see note on publish box handling, too - we need better failure
            ;; feedback/handling (eg, enqueue in db and have outgoing queue
            ;; ux -- kinda like scuttleb!!!)
            (success-callback)
            (hide-reply-box* *state)))))))

(defn click-contact-card
  [{:keys [contact-pubkey]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (swap! *state
        (fn [{:keys [active-key] :as curr-state}]
          (assoc-in curr-state [:identity-active-contact active-key] contact-pubkey))))]])

(defn click-reply-button
  [event-obj]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (log/debugf "Reply button action")
      (swap! *state assoc :active-reply-context
             (domain/->UIReplyContext (:id event-obj) (:id event-obj))))]])

(defn remove-relay-timeline!
  [{:keys [relay-url]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (log/debugf "Removing relay timeline %s, active key = %s" relay-url (:active-key @*state))
      (swap! *state assoc
             :relay-timelines (remove #{relay-url} (:relay-timelines @*state))))]])

(defn add-relay-timeline!
  [{:keys [relay-url]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (log/debugf "Adding relay timeline %s" relay-url)
      (swap! *state assoc
             :relay-timelines (conj (:relay-timelines @*state) relay-url)))]])

(defn handle
  [{:event/keys [type] :as event}]
  (log/debugf "Handling event of type %s: %s" type event)
  (case type
    :click-keycard (click-keycard event)
    :show-new-identity (show-new-identity-effect true)
    :new-identity-close-request (new-identity-close-request event)
    :delete-keycard (delete-keycard event)
    :show-relays (show-relays-effect true)
    :relays-close-request (relays-close-request event)
    :publish! (publish! event)
    :reply-close-request (reply! event)
    :click-contact-card (click-contact-card event)
    :click-reply-button (click-reply-button event)
    :remove-relay-timeline (remove-relay-timeline! event)
    :add-relay-timeline (add-relay-timeline! event)
    (log/error "no matching clause" type)))
