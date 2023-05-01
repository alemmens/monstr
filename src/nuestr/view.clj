(ns nuestr.view
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [cljfx.ext.tab-pane :as fx.ext.tab-pane]   
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [nuestr.cache :as cache]
   [nuestr.domain :as domain]
   [nuestr.event :as event]
   [nuestr.media :as media]
   [nuestr.metadata :as metadata]
   [nuestr.modal :as modal]
   [nuestr.nip19 :as nip19]
   [nuestr.relay-conn :as relay-conn]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.style :as style :refer [BORDER|]]
   [nuestr.subscribe :as subscribe]
   [nuestr.tab-profile :as tab-profile]
   [nuestr.tab-relays :as tab-relays]
   [nuestr.tab-views :as tab-views]
   [nuestr.timeline :as timeline]   
   [nuestr.util :as util]
   [nuestr.util-domain :as util-domain]
   [nuestr.util-java :as util-java]
   [nuestr.util-fx :as util-fx]
   [nuestr.view-common :as view-common]
   [nuestr.view-new-identity :as view-new-identity]
   [nuestr.view-reply :as view-reply])
  (:import (javafx.geometry Pos)
           (javafx.scene.layout VBox Priority)
           (javafx.scene.control TextFormatter$Change TextArea)
           (javafx.beans.property ReadOnlyProperty)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contacts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn contact-card
  [{:keys [active? parsed-contact parsed-metadata]}]
  (let [{:keys [public-key main-relay-url petname]} parsed-contact
        {:keys [name about picture-url nip05-id created-at]} parsed-metadata
        pubkey-short (util/format-pubkey-short public-key)
        avatar-color (media/color public-key)
        avatar-dim 50.0]
    {:fx/type :v-box
     :style (if active? {:-fx-border-color avatar-color} {})
     :style-class (cond-> ["ndesk-contact-card"] active? (conj "ndesk-contact-card-active"))
     :on-mouse-clicked {:event/type :click-contact-card :contact-pubkey public-key}
     :children
     [{:fx/type :h-box
       :children
       [(if (str/blank? picture-url)
          {:fx/type :label
           :min-width avatar-dim
           :min-height avatar-dim
           :max-width avatar-dim
           :max-height avatar-dim
           :style {:-fx-background-color avatar-color}
           :style-class "ndesk-contact-photo"
           :text (subs public-key 0 3)}
          {:fx/type media/avatar
           :picture-url picture-url
           :width avatar-dim})
        {:fx/type :v-box
         :children
         [{:fx/type :label
           :style {:-fx-padding [0 5]}
           :text (or petname name)}
          {:fx/type :label
           :style-class ["label" "ndesk-contact-pubkey"]
           :style {:-fx-padding [0 5]}
           :text public-key}
          {:fx/type :label
           :text about}]}]}
      {:fx/type :label
       :text main-relay-url}]}))

(defn contacts [{:keys [active-contact-list active-contact-pubkey metadata-cache]}]
  ;; just a note, active-contact-pubkey may refer to a contact that we no longer
  ;; have - this is fine - we just need to handle this case - user will have to
  ;; click on another actual contact in this case
  (log/debugf "Showing %d contacts for active key %s"
              (count active-contact-list)
              active-contact-pubkey)
  (let [{:keys [parsed-contacts]} active-contact-list]
    {:fx/type :v-box
     :children
     [{:fx/type :h-box
       :style {:-fx-padding 5}
       :cursor :hand
       :on-mouse-clicked {:event/type :show-new-contact}
       :children
       [{:fx/type :label
         :h-box/hgrow :always
         :max-width Integer/MAX_VALUE
         :style-class ["label" "ndesk-add-contact"]
         :alignment :center
         :text "add new contact"}]}
      {:fx/type :scroll-pane
       :style-class ["scroll-pane" "chronos-scroll-pane" "ndesk-contact-list"]
       :fit-to-width true
       :hbar-policy :never
       :vbar-policy :always
       :content
       {:fx/type :v-box
        :children
        (vec
          (sort-by
            (comp str/trim #(or % "zz") :name :parsed-metadata)
            (map #(let [contact-pubkey (:public-key %)]
                    (hash-map
                     :fx/type contact-card
                     :fx/key contact-pubkey
                     :active? (= contact-pubkey active-contact-pubkey)
                     :parsed-contact %
                     :parsed-metadata (metadata/get* metadata-cache contact-pubkey)
                     :metadata-cache metadata-cache))
                 parsed-contacts)))}}]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main panes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn messages [_]
  {:fx/type :label
   :text "messages"})

(defn search [_]
  {:fx/type :label
   :text "search"})

(defn publish-box
  [{:keys [can-publish?]}]
  {:fx/type :text-area
   :disable (not can-publish?)
   :style-class ["text-area" "ndesk-publish-box"] ;; used for .lookup
   :prompt-text "What's on your mind?"
   :wrap-text true
   :text-formatter {:fx/type :text-formatter
                    :value-converter :default
                    :filter view-common/text-formatter-filter*}})

(defn main-pane
  [{:keys [listview]}]
  {:fx/type :v-box
   :children (if (nil? listview)
               []
               [{:fx/type fx/ext-instance-factory
                 :create #(doto listview
                            (VBox/setVgrow Priority/ALWAYS))}])})

(defn refresh-button [last-refresh]
  {:fx/type :v-box
   :padding 5
   :children [{:fx/type :h-box :v-box/vgrow :always}
              {:fx/type :button
               :padding 5
               :tooltip {:fx/type :tooltip
                         :style-class "nuestr-tooltip"
                         :text "refresh all subscriptions"}
               :style-class ["button" "nuestr-refresh-button"]
               :on-mouse-pressed (fn [_]
                                   (doseq [c (:all-columns @domain/*state)]
                                     (timeline/grow-timeline! (:id c) nil))
                                   (doseq [s (:open-profile-states @domain/*state)]
                                     (timeline/grow-timeline! nil (:pubkey s)))
                                   (relay-conn/refresh!))
               :text (str (char 0x21bb)) ; clockwise open-circle arrow
               }
              {:fx/type :h-box :v-box/vgrow :always}]})
              

(defn hidden-columns [all-column-ids visible-column-ids]
  "Returns a list of the ids of those columns that are not visible."
  (sort (seq (set/difference (set all-column-ids)
                             visible-column-ids))))

(defn add-column-button
  [{:keys [text all-column-ids visible-column-ids]}]
  {:fx/type :button
   :disable (not (seq (hidden-columns all-column-ids visible-column-ids)))
   :padding 5
   :on-mouse-pressed {:event/type :show-add-column-dialog}
   :text text})

(defn find-column [relay-urls columns]
  (log/debugf "Looking for %s in %d columns" relay-urls (count columns))
  (first (filter #(domain/column-matches-relay-urls? % relay-urls)
                 columns)))

(defn thread-list [listview]
  {:fx/type :v-box
   :children (if (nil? listview)
               []
               [{:fx/type fx/ext-instance-factory
                 :create #(doto listview
                            (VBox/setVgrow Priority/ALWAYS))}])})

(defn tab*
  [{:keys [label content closable pubkey]}]
  {:fx/type :tab
   :id (or pubkey (str (rand-int 1000000)))
   :closable closable
   :on-closed (fn [_]
                (log/debugf "Closing tab %s with pubkey %s" label pubkey)
                (swap! domain/*state update-in
                       [:open-profile-states] dissoc pubkey))
   :text label
   :content content})

(defn whats-on-your-mind [{:keys [can-publish?]}]
  {:fx/type :h-box
   :alignment :center
   :children [{:fx/type publish-box
               :can-publish? can-publish?
               :h-box/margin 5
               :min-width 300
               :max-width 600}
              {:fx/type :label
               :h-box/margin 5
               :graphic {:fx/type :button
                         ;; NOTE: :on-action and :on-mouse-clicked don't seem to work
                         ;; when the publish text-area has focus but :on-mouse-pressed
                         ;; does.
                         :on-mouse-pressed {:event/type :publish!}
                         :disable (not can-publish?)
                         :style-class ["button" "ndesk-publish-button"]
                         :text "Publish"}}]})


(defn column-header [{:keys [column show-thread? listview timeline visible-column-ids all-columns]}]
  (let [column-id (:id column)
        name (:name (:view column))]
    {:fx/type :h-box
     :alignment :center
     :spacing 10
     :style-class "relay-timeline-label"
     :children (remove nil?
                       [{:fx/type :h-box :h-box/hgrow :always}
                        (when show-thread?
                          (timeline/back-from-thread-button column nil))
                        #_
                        (when-not show-thread?
                          (let [new (domain/nr-new-notes timeline)]
                            {:fx/type :button
                             :padding 5
                             :text (format " %s new notes " new)}))                        
                        {:fx/type :label
                         :text (if show-thread?
                                 (format "thread: %s" name)
                                 name)
                         :padding 5}
                        {:fx/type :button
                         :padding 5
                         :on-mouse-pressed {:event/type :remove-visible-column
                                            :column-id column-id}
                         :text "x"}
                        {:fx/type :h-box :h-box/hgrow :always}
                        (when (= column-id (last visible-column-ids))
                          {:fx/type add-column-button
                           :text "+"
                           :visible-column-ids visible-column-ids
                           :all-column-ids (map :id all-columns)})])}))

(defn column-pane [{:keys [column show-thread? listview timeline all-columns visible-column-ids]}]
  {:fx/type :v-box
   :children [{:fx/type column-header
               :column column
               :show-thread? show-thread?
               :listview listview
               :timeline timeline
               :all-columns all-columns
               :visible-column-ids visible-column-ids}
              {:fx/type main-pane
               :listview listview}]})
  
(defn main-panes
  [{:keys [views visible-column-ids all-columns
           relays show-add-column-dialog? can-publish?
           active-key active-reply-context active-contact-pubkey
           metadata-cache]}]
  #_(log/debugf "Main panes with active key='%s', %d columns=%s"
              active-key
              (count visible-column-ids)
              (pr-str visible-column-ids))
  {:fx/type :v-box
   :children
   [;; The "what's on your mind?" box.
    {:fx/type whats-on-your-mind
     :can-publish? can-publish?}
    ;; Columns
    {:fx/type :h-box
     :spacing 5
     :children
     (if (seq visible-column-ids)
       (map (fn [column-id]
              (let [column (domain/find-column-by-id column-id)
                    name (:name (:view column))
                    show-thread? (:show-thread? column)
                    pair (get (:identity->timeline-pair column) active-key)
                    listview (if show-thread?
                               (:thread-listview pair)
                               (:flat-listview pair))
                    timeline (if show-thread?
                               (:thread-timeline pair)
                               (:flat-timeline pair))]
                (if (nil? pair)
                  (log/debugf "No pair found for active key %s and column %s" active-key column-id)
                  (log/debugf "Creating pane for column %s with view %s (show-thread=%s pair=%s listview=%s)"
                              column-id (:name (:view column))
                              show-thread?
                              pair listview))
                {:fx/type column-pane
                 :column column
                 :show-thread? show-thread?
                 :listview listview
                 :timeline timeline
                 :all-columns all-columns
                 :visible-column-ids visible-column-ids}))
            visible-column-ids)
       ;; If there are no columns, just show an "Add column" button (centered).
       [{:fx/type :h-box :h-box/hgrow :always}
        (add-column-button "Add column")
        {:fx/type :h-box :h-box/hgrow :always}])}]})


(defn new-column-dialog
  [{:keys [views visible-column-ids show-add-column-dialog?]}]
  #_(log/debugf "New column dialog with all-columns=%s visible=%s"
                (pr-str (map (comp :name :view) all-columns)) (pr-str visible-column-ids))
  (let [column-ids (hidden-columns (domain/all-column-ids) visible-column-ids)
        view-names (sort (remove nil?
                                 (map (comp :name :view domain/find-column-by-id)
                                      column-ids)))]
    #_(log/debugf "New column dialog, view names = %s, first = %s"
                  (pr-str view-names) (first view-names))
    (if (seq view-names)
      {:fx/type :choice-dialog
       :selected-item (first view-names)
       :title "New column"
       :showing show-add-column-dialog?
       :header-text "Add a column"
       :on-close-request {:event/type :add-column-close-request}
       :items view-names}
      {:fx/type :label :text ""})))

(defn tab-pane
  [{:keys [visible-column-ids all-columns
           open-profile-states
           views selected-view temp-view temp-view-changed?
           relays relays-sorted-by relay-search-text connected-info
           show-add-column-dialog? new-timeline
           can-publish? active-reply-context active-contact-list
           active-key active-contact-pubkey identities
           identity-metadata metadata-cache
           ]}]
  #_(log/debugf "Tab pane with columns=%s and show=%s"
              (pr-str visible-column-ids)
              show-add-column-dialog?)
  {:fx/type fx/ext-let-refs
   :refs {:dialog {:fx/type view-reply/dialog
                   :active-reply-context active-reply-context}
          :timeline-dialog {:fx/type new-column-dialog
                            :views views
                            :visible-column-ids visible-column-ids
                            :new-timeline new-timeline
                            :show-add-column-dialog? show-add-column-dialog?}}
   :desc {:fx/type :tab-pane
          :side :top
          :id "nuestr-tabs"
          ; :style-class "nuestr-tab-pane"
          :tabs (for [[label content closable pubkey]
                      (concat [["Home" {:fx/type main-panes
                                        :views views
                                        :visible-column-ids visible-column-ids
                                        :all-columns all-columns
                                        :can-publish? can-publish?
                                        :show-add-column-dialog? show-add-column-dialog?
                                        :active-key active-key
                                        :active-reply-context active-reply-context
                                        :active-contact-list active-contact-list
                                        :active-contact-pubkey active-contact-pubkey
                                        :metadata-cache metadata-cache
                                        }
                                false]
                               ["Views" {:fx/type tab-views/show-tab
                                         :views views
                                         :selected-view selected-view
                                         :temp-view temp-view
                                         :temp-view-changed? temp-view-changed?
                                         :metadata-cache metadata-cache
                                         }
                                false]
                               ;; Contacts (i.e. follows) for the active identity.
                               ["Contacts" {:fx/type contacts
                                            :active-contact-list active-contact-list
                                            :active-contact-pubkey active-contact-pubkey
                                            :metadata-cache metadata-cache}
                                false]
                               ["Relays" {:fx/type tab-relays/relays
                                          :relays relays
                                          :relays-sorted-by relays-sorted-by
                                          :relay-search-text relay-search-text
                                          :connected-info connected-info}
                                false]]
                              ;; Profile tabs.
                              (map (fn [pubkey]
                                     (let [metadata (metadata/get* metadata-cache pubkey)]
                                       [(format "Profile: %s" (or (:name metadata)
                                                                  (util/format-pubkey-short pubkey)))
                                        {:fx/type tab-profile/profile
                                         :pubkey pubkey
                                         :views views
                                         :open-profile-states open-profile-states
                                         :identities identities
                                         :identity-metadata identity-metadata
                                         :metadata metadata}
                                        (not= pubkey active-key) ; not closable for active account
                                        pubkey
                                        ]))
                                   (keys open-profile-states)))]
                  {:fx/type tab*
                   :label label
                   :pubkey pubkey
                   :closable closable
                   :content content})}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accounts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn keycard-create-new
  [{:keys [show-new-identity? new-identity-error views]}]
  {:fx/type fx/ext-let-refs
   :refs {:dialog {:fx/type view-new-identity/dialog
                   :views views
                   :show-new-identity? show-new-identity?
                   :new-identity-error new-identity-error}}
   :desc {:fx/type :button
          :padding 5
          :on-mouse-clicked {:event/type :show-new-identity}
          :text "Add account"}})

(defn identity-selector
  [{:keys [identities views active-key identity-metadata show-new-identity?]}]
  {:fx/type :h-box
   :spacing 5
   :padding 14
   :children (let [combo-box (when active-key
                               {:fx/type :combo-box
                                :value (or (:name (get identity-metadata active-key))
                                           active-key)
                                :pref-width 150
                                :style-class "identity-selector"
                                :on-value-changed (fn [k]
                                                    (swap! domain/*state assoc :active-key k)
                                                    (timeline/update-active-timelines! domain/*state k))
                                :button-cell (fn [k] {:text (:name (get identity-metadata k))})
                                :cell-factory {:fx/cell-type :list-cell
                                               :describe (fn [k] {:text (:name (get identity-metadata k))})}
                                :items (sort-by #(:name (get identity-metadata %))
                                                (keys identity-metadata))})
                   add-new-button {:fx/type :v-box
                                   :padding 4
                                   :children [{:fx/type keycard-create-new
                                               :show-new-identity? show-new-identity?}]}]
               (if active-key
                 [combo-box add-new-button]
                 [add-new-button]))})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-search-text [string]
  (if (str/starts-with? string "npub")
    (second (nip19/decode string))
    (when (seq (store/load-metadata store/db [string]))
      string)))
       
(defn search-pane [{:keys [search-text]}]
  {:fx/type :h-box
   :spacing 5
   :padding 18
   :children [{:fx/type :text-field
               :max-width 500
               :min-width 400
               :padding 5
               :prompt-text "npub or raw pubkey"
               :text (or search-text "")
               :style-class ["text-input"]
               :on-text-changed  (fn [new-text]
                                   (swap! domain/*state assoc
                                          :search-text new-text))}
              {:fx/type :button
               :padding 5
               :text "Search" ; (str (char 0x2315))}
               :on-mouse-pressed (fn [e]
                                   (if-let [pubkey (parse-search-text (:search-text @domain/*state))]
                                     (timeline/open-profile e pubkey)
                                     (modal/info-popup "Search string not found.")))}]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn root [{:keys [visible-column-ids all-columns
                    views selected-view temp-view temp-view-changed?
                    active-key identities identity-metadata
                    relays relays-sorted-by relay-search-text
                    refresh-relays-ts connected-info
                    show-add-column-dialog? new-timeline
                    show-new-identity? new-identity-error active-reply-context contact-lists
                    identity-active-contact metadata-cache
                    last-refresh
                    search-text
                    status-message status-message-timestamp
                    debug-message
                    open-profile-states
                    ]}]
  #_(log/debugf "Root with column ids=%s and %d contact-lists"
              (pr-str visible-column-ids)
              (count contact-lists))
  {:fx/type :border-pane
   :top {:fx/type :h-box
         :spacing 10
         :children [{:fx/type identity-selector
                     :identities identities
                     :active-key active-key
                     :show-new-identity? show-new-identity?
                     :identity-metadata identity-metadata}
                    {:fx/type search-pane
                     :search-text search-text}
                    {:fx/type :label
                     :text (or debug-message "")}
                    {:fx/type :h-box :h-box/hgrow :always}
                    (refresh-button last-refresh)]}
   :center {:fx/type tab-pane
            :visible-column-ids visible-column-ids
            :all-columns all-columns
            :views views
            :selected-view selected-view
            :temp-view temp-view
            :temp-view-changed? temp-view-changed?
            :relays relays
            :relays-sorted-by relays-sorted-by
            :relay-search-text relay-search-text
            :connected-info connected-info
            :show-add-column-dialog? show-add-column-dialog?
            :new-timeline new-timeline
            :can-publish? (util-domain/can-publish? active-key identities)
            :active-key active-key
            :active-reply-context active-reply-context
            :active-contact-list (get contact-lists active-key)
            :active-contact-pubkey (get identity-active-contact active-key)
            :identities identities
            :identity-metadata identity-metadata
            :metadata-cache metadata-cache
            :open-profile-states open-profile-states
            }
   :bottom {:fx/type status-bar/pane
            :status-message (if (> (- (util/now-epoch-second)
                                      (or status-message-timestamp
                                          (util/now-epoch-second)))
                                   30)
                              "" ; clear status message after 30 seconds
                              status-message)
            :relays relays
            :refresh-relays-ts refresh-relays-ts
            :connected-info connected-info}})

(defn stage [{:keys [visible-column-ids all-columns
                     active-key identities identity-metadata
                     relays relays-sorted-by relay-search-text
                     refresh-relays-ts connected-info
                     show-add-column-dialog? new-timeline
                     show-new-identity? new-identity-error active-reply-context
                     contact-lists identity-active-contact metadata-cache
                     last-refresh views selected-view temp-view temp-view-changed?
                     search-text
                     status-message status-message-timestamp
                     debug-message
                     open-profile-states
                     ]}]
  #_(log/debugf "Stage with %d identities and active key %s" (count identities) active-key)
  {:fx/type :stage
   :showing true
   :title "Nuestr"
   :width 1272
   :height 800
   :scene
   {:fx/type :scene
    :stylesheets (style/css)
    :root {:fx/type root
           :show-new-identity? show-new-identity?
           :new-identity-error new-identity-error
           :active-reply-context active-reply-context
           :active-key active-key
           :identities identities
           :identity-metadata identity-metadata
           :contact-lists contact-lists
           :identity-active-contact identity-active-contact
           :relays relays
           :relays-sorted-by relays-sorted-by
           :relay-search-text relay-search-text
           :last-refresh last-refresh
           :refresh-relays-ts refresh-relays-ts
           :connected-info connected-info
           :visible-column-ids visible-column-ids
           :all-columns all-columns
           :views views
           :selected-view (or selected-view (:name (first (vals views))))
           :temp-view (or temp-view (first (vals views)))
           :temp-view-changed? temp-view-changed?
           :show-add-column-dialog? show-add-column-dialog?
           :new-timeline new-timeline
           :metadata-cache metadata-cache
           :search-text search-text
           :status-message status-message
           :status-message-timestamp status-message-timestamp
           :debug-message debug-message
           :open-profile-states open-profile-states
           }}})
