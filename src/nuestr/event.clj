(ns nuestr.event
  (:require
    [cljfx.api :as fx]
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]
    [clojure.string :as str]
    [nuestr.domain :as domain]
    [nuestr.file-sys :as file-sys]
    [nuestr.hydrate :as hydrate]    
    [nuestr.modal :as modal]
    [nuestr.publish :as publish]
    [nuestr.relay-conn :as relay-conn]
    [nuestr.status-bar :as status-bar]
    [nuestr.store :as store]
    [nuestr.timeline :as timeline]
    [nuestr.util :as util]
    [nuestr.util-domain :as util-domain]
    [nuestr.x.crypt :as crypt]
    [manifold.deferred :as d])
  (:import (javafx.scene.control DialogEvent Dialog Button TextArea)
           (javafx.event Event ActionEvent)))

(defn click-keycard
  [{:keys [public-key]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (log/debugf "Click keycard for public key %s" public-key)
      (timeline/update-active-timelines! *state public-key))]])

(defn click-contact-card
  [{:keys [contact-pubkey]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (swap! *state
        (fn [{:keys [active-key] :as curr-state}]
          (assoc-in curr-state [:identity-active-contact active-key] contact-pubkey))))]])

(defn show-new-identity-effect
  [show-new-identity?]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (swap! *state
        (fn [curr-state]
          (cond-> (assoc curr-state :show-new-identity? show-new-identity?)
            (not show-new-identity?) (assoc :new-identity-error "")))))]])

(defn delete-account
  [{:keys [identity]}]
  (when (modal/blocking-yes-no-alert "" "Are you sure?")
    [[:bg
      (fn [*state db executor _dispatch!]
        (store/delete-identity! db (:public-key identity))
        (let [{curr-identities :identities} @*state]
          (when (some #(= (:public-key identity) (:public-key %))
                      curr-identities)
            (hydrate/dehydrate! *state db executor [identity]))))]]))

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

(defn add-timeline-pair-to-column [pubkey column]
  (update-in column [:identity->timeline-pair]
             #(conj %[pubkey (hydrate/new-timeline-pair (:id column))])))

(defn add-identity-and-close-dialog-effect
  [public-key maybe-private-key]
  (util/concatv
    (show-new-identity-effect false)
    [[:bg
      (fn [*state db executor _dispatch!]
        (store/insert-identity! db public-key maybe-private-key) ;; idempotent
        (maybe-contribute-secret-key* *state public-key maybe-private-key)
        (let [{curr-identities :identities} @*state]
          ;; Don't hydrate identity if it's already hydrated.
          (when-not (some #(= public-key (:public-key %)) curr-identities)
            ;; Add a new timeline pair to all columns for the new public key.
            (swap! *state assoc
                   :all-columns (map #(add-timeline-pair-to-column public-key %)
                                     (:all-columns @*state)))
            ;; Hydrate the new identity.
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
            (.consume dialog-event) ; prevents dialog closure
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
              (.consume dialog-event) ; prevents dialog closure
              [[:bg (fn [*state _db _exec _dispatch!]
                      (swap! *state assoc
                        :new-identity-error "Bad private key"))]])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
             ;; This is especially necessary when user is close-cancelling; they
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
          (replace-relays-effect (file-sys/load-relay-defaults) true)))
      :cancel
      (show-relays-effect false)
      (replace-relays-effect dialog-result false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ;; The idea here is that we only clear our publish text box if we're successful -
    ;; otherwise text remains (we still do need better ux feedback in this case tho).
    (publish-effect content false (fn [& _] (fx/run-later (.clear found))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Replies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn click-reply-button
  [event-obj]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (log/debugf "Reply button action")
      (swap! *state assoc :active-reply-context
             (domain/->UIReplyContext (:id event-obj) (:id event-obj))))]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding/removing columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-visible-column!
  [{:keys [column-id]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (log/debugf "Removing column %s, active key = %s" column-id (:active-key @*state))
      (swap! *state assoc
             :visible-column-ids (remove #{column-id} (:visible-column-ids @*state)))
      (file-sys/save-visible-columns))]])

(defn- show-add-column-effect
  [show?]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (swap! *state assoc :show-add-column-dialog? show?))]])
  
(defn- add-column-close-request
  [{^DialogEvent dialog-event :fx/event}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      ;; Add a new column if the user has selected a column id.
      (when-let [view-name (.getResult (.getSource dialog-event))]
        (when-let [new-column-id (:id (domain/find-column-with-view-name view-name))]
          (log/debugf "Adding visible column with id %s for view %s."
                      new-column-id view-name)
          (swap! *state update
                 :visible-column-ids #(conj % new-column-id))))
      (file-sys/save-visible-columns)
      (swap! *state assoc :show-add-column-dialog? false))]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Managing views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_
(defn confirmation-alert
  ;; TODO: FINISH THIS.
  [_]
  {:fx/type :dialog
   :showing true
   :on-hidden (fn [^DialogEvent e]
                (condp = (.getButtonData ^ButtonType (.getResult ^Dialog (.getSource e)))
                  ButtonBar$ButtonData/NO (reset! *state :select-action)
                  ButtonBar$ButtonData/YES (reset! *state :confirmed)))
   :dialog-pane {:fx/type :dialog-pane
                 :header-text "Confirm"
                 :content-text "This view is used by one or more columns. These columns
will be removed when the view is deleted. Continue?"
                 :expandable-content {:fx/type :label
                                      :text "This action can't be undone."}
                 :button-types [:no :yes]}})

(defn save-view
  [{:keys [temp-view]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (let [temp-view (:temp-view @*state)
            old-name (:selected-view @*state)
            new-name (:name temp-view)]
        (log/debugf "Saving view with new name %s and old name %s and relays %s"
                    new-name
                    old-name
                    (:relay-urls temp-view))
        (swap! *state assoc-in
               [:views new-name]
               temp-view)
        (swap! *state assoc :temp-view-changed? false)
        ;; Update the column that uses this view.    
        (let [old-column (domain/find-column-with-view-name old-name)
              new-column (assoc old-column :view temp-view)
              column-id (:id old-column)]
          (swap! *state assoc
                 :all-columns (conj (remove #(= (:id %) column-id)
                                            (:all-columns @*state))
                                    new-column)))
        ;; Update views if necessary.
        (when-not (= old-name new-name)
          (swap! *state assoc
                 :views (dissoc (:views @*state) old-name)
                 :selected-view new-name))
        ;; Save view and refresh the column timelines.
        (file-sys/save-views (:views @*state))
        (status-bar/message! (format "Saved view '%s'" new-name))
        (let [column (domain/find-column-with-view-name new-name)]
          (assert column)
          (hydrate/refresh-column! column))))]])

(defn delete-view [event]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (let [views (:views @*state)
            selected-view (:selected-view @*state)
            using-columns (domain/columns-using-view selected-view)]
        (log/debugf "Deleting view %s" selected-view)
        ;; TODO: Check if the view is in use. If so, ask for confirmation first.
        (when (> (count views) 1)
          ;; Move listview focus to the first view in the list.
          (let [new-view-name (first (sort (keys views)))
                new-view (domain/find-view new-view-name)
                new-columns (remove #(domain/column-uses-view? % selected-view)
                                    (:all-columns @*state))]
            (swap! *state assoc
                   :selected-view new-view-name
                   :temp-view new-view
                   :temp-view-changed? false
                   :views (dissoc views selected-view)
                   ;; Remove all columns (both visible and hidden) that use the deleted view.               
                   :all-columns new-columns
                   :visible-column-ids (map :id
                                            (keep domain/find-column-by-id
                                                  (:visible-column-ids @*state)))))
          ;; Remember that this view is now gone.
          (file-sys/save-views (:views @domain/*state))
          ;; Show status.
          (status-bar/message! (format "Deleted view '%s'" selected-view)))))]])

(defn- new-view-name
  ([] (if (domain/find-view "New view")
        (new-view-name 2)
        "New view"))
  ([n] (if (domain/find-view (format "New view %d" n))
         (new-view-name (inc n))
         (format "New view %d" n))))

(defn- add-view [event]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (let [new-name (new-view-name)
            new-view (domain/make-view new-name
                                       #{(first (sort (domain/relay-urls @*state)))}
                                       #{})]
        (swap! *state assoc-in
               [:views new-name]
               new-view)
        (swap! *state assoc
               :selected-view (:name new-view)
               :temp-view new-view
               :temp-view-changed? true)
        (status-bar/message! (format "Added view '%s'" new-name))
        (hydrate/add-column-for-view! new-view)))]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Managing profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- save-following-views
  [{:keys [views pubkey]}]
  [[:bg
    (fn [*state _db _exec _dispatch!]
      (let [profile-state (get (:open-profile-states @*state) pubkey)
            following-views (:following-views profile-state)]
        #_(log/debugf "Saving following views for %s, changed=%s"
                      pubkey (:following-views-changed profile-state))
        (doseq [v (keys views)]
          ;; Update view's follow set.          
          (let [follow-set (:follow-set (domain/find-view v))
                new-follow-set (if (following-views v)
                                 (conj follow-set pubkey)
                                 (disj follow-set pubkey))]
            (domain/update-view! v :follow-set new-follow-set)))
        ;; Update the columns that use the views.
        (let [new-columns (map #(assoc % :view (domain/find-view (:name (:view %))))
                               (:all-columns @domain/*state))]
          (swap! *state assoc :all-columns new-columns))
        (doseq [v (:following-views-changed profile-state)]
          ;; Refresh column if its view has changed.
          (let [column (domain/find-column-with-view-name v)]
            (assert column)
            (hydrate/refresh-column! column))))
      ;; Save views.
      (file-sys/save-views (:views @*state))
      (swap! *state assoc-in
             [:open-profile-states pubkey :following-views-changed]
             #{}))]])
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle
  [{:event/keys [type] :as event}]
  (log/debugf "Handling %s" type)
  (case type
    :click-keycard (click-keycard event)
    :show-new-identity (show-new-identity-effect true)
    :new-identity-close-request (new-identity-close-request event)
    :delete-account (delete-account event)
    :show-relays (show-relays-effect true)
    :relays-close-request (relays-close-request event)
    :publish! (publish! event)
    :reply-close-request (reply! event)
    :click-contact-card (click-contact-card event)
    :click-reply-button (click-reply-button event)
    ;; Add/remove visible columns.
    :remove-visible-column (remove-visible-column! event)
    :show-add-column-dialog (show-add-column-effect true)
    :add-column-close-request (add-column-close-request event)
    :save-view (save-view event)
    :delete-view (delete-view event)
    :add-view (add-view event)
    ;; Profile tab
    :save-following-views (save-following-views event)
    (log/error "no matching clause" type)))
