(ns monstr.view
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [monstr.avatar :as avatar]
   [monstr.cache :as cache]
   [monstr.domain :as domain]
   [monstr.event :as ev]
   [monstr.metadata :as metadata]   
   [monstr.status-bar :as status-bar]
   [monstr.style :as style :refer [BORDER|]]
   [monstr.subscribe :as subscribe]
   [monstr.tab-views :as tab-views]
   [monstr.tab-profile :as tab-profile]
   [monstr.timeline :as timeline]   
   [monstr.util :as util]
   [monstr.util-domain :as util-domain]
   [monstr.util-java :as util-java]
   [monstr.util-fx :as util-fx]
   [monstr.view-common :as view-common]
   [monstr.view-new-identity :as view-new-identity]
   [monstr.view-reply :as view-reply])
  (:import (javafx.geometry Pos)
           (javafx.scene.layout VBox Priority)
           (javafx.scene.control TextFormatter$Change TextArea)
           (javafx.beans.property ReadOnlyProperty)))


(defn keycard-create-new
  [{:keys [show-new-identity? new-identity-error views]}]
  {:fx/type fx/ext-let-refs
   :refs {:dialog {:fx/type view-new-identity/dialog
                   :views views
                   :show-new-identity? show-new-identity?
                   :new-identity-error new-identity-error}}
   :desc {:fx/type :h-box
          :cursor :hand
          :style-class ["ndesk-keycard"]
          :on-mouse-clicked {:event/type :show-new-identity}
          :children
          [{:fx/type :label
            :text "Add account"}]}})

(defn contact-card
  [{:keys [active? parsed-contact parsed-metadata]}]
  (let [{:keys [public-key main-relay-url petname]} parsed-contact
        {:keys [name about picture-url nip05-id created-at]} parsed-metadata
        pubkey-short (util/format-pubkey-short public-key)
        avatar-color (avatar/color public-key)
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
          {:fx/type avatar/avatar
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

(defn add-column-button
  [text]
  {:fx/type :button
   :padding 5
   :on-mouse-pressed {:event/type :show-add-column-dialog}
   :text text})

(defn back-from-thread-button
  [column]
  {:fx/type :button
   :padding 5
   :on-mouse-pressed (fn [e] (timeline/unshow-column-thread! domain/*state column))
   :text (str " " (char 0x2190) " ") ; left arrow
   })

(defn refresh-button
  [last-refresh]
  {:fx/type :v-box
   :padding 5
   :children [{:fx/type :h-box :v-box/vgrow :always}
              {:fx/type :button
               :padding 5
               :tooltip {:fx/type :tooltip
                         :style-class "monstr-tooltip"
                         :text "refresh all subscriptions"}
               :style-class ["button" "monstr-refresh-button"]
               :on-mouse-pressed (fn [_] (subscribe/refresh!))
               :text (str (char 0x21bb)) ; clockwise open-circle arrow
               }
              {:fx/type :h-box :v-box/vgrow :always}]})
              

(defn hidden-columns [all-column-ids visible-column-ids]
  "Returns a list of the ids of those columns that are not visible."
  (sort (seq (set/difference (set all-column-ids)
                             visible-column-ids))))

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
  "PUBKEY is only given for closable profile tabs."
  [{:keys [label content closable pubkey]}]
  {:fx/type :tab
   :closable closable
   :on-closed (fn [_]
                (log/debugf "Closing tab %s with pubkey %s" label pubkey)
                (swap! domain/*state update-in
                       [:open-profile-states] dissoc pubkey))
   :text label
   :content content})

(defn main-panes
  [{:keys [views visible-column-ids all-columns
           relays show-add-column-dialog? can-publish?
           active-key active-reply-context active-contact-pubkey
           metadata-cache]}]
  (log/debugf "Main panes with active key='%s', %d columns=%s and views %s"
              active-key
              (count visible-column-ids)
              (pr-str visible-column-ids)
              (pr-str (map (comp :view domain/find-column-by-id) visible-column-ids)))
  {:fx/type :v-box
   :children
   [;; The "what's on your mind?" box.
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
                           :text "Publish"}}]}
    ;; Columns
    {:fx/type :h-box
     :children
     (if (seq visible-column-ids)
       (map (fn [column-id]
              (let [column (domain/find-column-by-id column-id)
                    name (:name (:view column))
                    show-thread? (:show-thread? column)
                    pair (get (:identity->timeline-pair column) active-key)
                    listview (if show-thread?
                               (:thread-listview pair)
                               (:flat-listview pair))]
                (if (nil? pair)
                  (log/debugf "No pair found for active key %s and column %s" active-key column-id)
                  (log/debugf "Creating pane for column %s with view %s (show-thread=%s pair=%s listview=%s)"
                              column-id (pr-str (:view column))
                              show-thread?
                              pair listview))
                {:fx/type :v-box
                 :h-box/margin 5
                 :h-box/hgrow :always
                 :children [{:fx/type :h-box
                             :alignment :center
                             :style-class "relay-timeline-label"
                             :children (remove nil?
                                               [{:fx/type :h-box :h-box/hgrow :always}
                                                (when show-thread?
                                                  (back-from-thread-button column))
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
                                                (when (and (= column-id (last visible-column-ids))
                                                           (not (nil?
                                                                 (hidden-columns (domain/all-column-ids)
                                                                                 visible-column-ids))))
                                                  (add-column-button "+"))])}
                            {:fx/type main-pane
                             :listview listview}]}))
            visible-column-ids)
       ;; If there are no relay timelines, just show an "Add timeline" button (centered).
       [{:fx/type :h-box :h-box/hgrow :always}
        (add-column-button "Add timeline")
        {:fx/type :h-box :h-box/hgrow :always}])}]})




(defn new-column-dialog
  [{:keys [views visible-column-ids new-timeline show-add-column-dialog?]}]
  #_(log/debugf "New column dialog with all-columns=%s visible=%s"
                (pr-str (map (comp :name :view) all-columns)) (pr-str visible-column-ids))
  (let [column-ids (hidden-columns (domain/all-column-ids) visible-column-ids)
        view-names (map (comp :name :view domain/find-column-by-id)
                        column-ids)]
    {:fx/type :choice-dialog
     :selected-item (first view-names)
     :title "New column"
     :showing show-add-column-dialog?
     :header-text "Add a column"
     :on-close-request {:event/type :add-column-close-request}
     :items view-names}))

(defn tab-pane
  [{:keys [visible-column-ids all-columns
           open-profile-states
           views selected-view temp-view temp-view-changed?
           relays show-add-column-dialog? new-timeline
           can-publish? active-reply-context active-contact-list
           active-key active-contact-pubkey identities
           identity-metadata metadata-cache
           ]}]
  (log/debugf "Tab pane with columns=%s and show=%s"
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
                                         }
                                false]
                               ;; Contacts (i.e. follows) for the active identity.
                               ["Contacts" {:fx/type contacts
                                            :active-contact-list active-contact-list
                                            :active-contact-pubkey active-contact-pubkey
                                            :metadata-cache metadata-cache}
                                false]
                               ;; Profile tab for the active identity.
                               ["Profile" {:fx/type tab-profile/profile
                                           :pubkey active-key
                                           :identities identities
                                           :metadata (get identity-metadata active-key)}
                                false]]
                              ;; Profile tabs for authors.
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
                                        true
                                        pubkey
                                        ]))
                                   (keys open-profile-states)))]
                  {:fx/type tab*
                   :label label
                   :pubkey pubkey
                   :closable closable
                   :content content})}})

(defn keycards
  [{:keys [active-key views identities identity-metadata show-new-identity? new-identity-error]}]
  {:fx/type :v-box
   :style-class "ndesk-lhs-pane"
   :children (vec
               (concat
                 (map
                   #(hash-map
                      :fx/type tab-profile/keycard
                      :fx/key (:public-key %)
                      :active? (= active-key (:public-key %))
                      :identity_ %
                      :this-identity-metadata (get identity-metadata (:public-key %)))
                   identities)
                 [{:fx/type keycard-create-new
                   :views views
                   :show-new-identity? show-new-identity?
                   :new-identity-error new-identity-error}]))})

(defn identity-selector
  [{:keys [identities views active-key identity-metadata show-new-identity?]}]
  {:fx/type :h-box
   ; :alignment :center
   :padding 10
   :children (let [label {:fx/type :label
                          :alignment :center
                          :padding 10
                          :text "Account: "}
                   combo-box (when active-key
                               {:fx/type :combo-box
                                :value active-key
                                :style-class "identity-selector"
                                :on-value-changed (fn [k]
                                                    (swap! domain/*state assoc :active-key k)
                                                    (timeline/update-active-timelines! domain/*state k))
                                :button-cell (fn [k] {:text (:name (get identity-metadata k))})
                                :cell-factory {:fx/cell-type :list-cell
                                               :describe (fn [k] {:text (:name (get identity-metadata k))})}
                                :items (sort-by #(:name (get identity-metadata %))
                                                (keys identity-metadata))})
                   add-new-button {:fx/type keycard-create-new
                                   :padding 5
                                   :show-new-identity? show-new-identity?}]
               (if active-key
                 [label combo-box add-new-button]
                 [label add-new-button]))})
              

(defn root [{:keys [visible-column-ids all-columns
                    views selected-view temp-view temp-view-changed?
                    show-relays? active-key identities identity-metadata
                    relays refresh-relays-ts connected-info
                    show-add-column-dialog? new-timeline
                    show-new-identity? new-identity-error active-reply-context contact-lists
                    identity-active-contact metadata-cache
                    last-refresh
                    status-message status-message-timestamp
                    open-profile-states
                    ]}]
  (log/debugf "Root with column ids=%s" (pr-str visible-column-ids))
  {:fx/type :border-pane
   :top {:fx/type :h-box
         :children [{:fx/type identity-selector
                     :identities identities
                     :active-key active-key
                     :show-new-identity? show-new-identity?
                     :identity-metadata identity-metadata}
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
                                   5)
                              "" ; clear status message after 5 seconds
                              status-message)
            :show-relays? show-relays?
            :relays relays
            :refresh-relays-ts refresh-relays-ts
            :connected-info connected-info}})

(defn stage [{:keys [visible-column-ids all-columns
                     show-relays? active-key identities identity-metadata
                     relays refresh-relays-ts connected-info
                     show-add-column-dialog? new-timeline
                     show-new-identity? new-identity-error active-reply-context
                     contact-lists identity-active-contact metadata-cache
                     last-refresh views selected-view temp-view temp-view-changed?
                     status-message status-message-timestamp
                     open-profile-states
                     ]}]
  (log/debugf "Stage with %d identities and active key %s" (count identities) active-key)
  {:fx/type :stage
   :showing true
   :title "Nuestr"
   :width 1272
   :height 800
   :scene
   {:fx/type :scene
    :stylesheets (style/css)
    :root {:fx/type root
           :show-relays? show-relays?
           :show-new-identity? show-new-identity?
           :new-identity-error new-identity-error
           :active-reply-context active-reply-context
           :active-key active-key
           :identities identities
           :identity-metadata identity-metadata
           :contact-lists contact-lists
           :identity-active-contact identity-active-contact
           :relays relays
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
           :status-message status-message
           :status-message-timestamp status-message-timestamp
           :open-profile-states open-profile-states
           }}})
