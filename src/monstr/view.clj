(ns monstr.view
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [clojure.tools.logging :as log]
   [monstr.avatar :as avatar]
   [monstr.cache :as cache]
   [monstr.domain :as domain]
   [monstr.event :as ev]
   [monstr.style :as style :refer [BORDER|]]
   [monstr.subscribe :as subscribe]
   [monstr.tab-views :as tab-views]
   [monstr.util :as util]
   [monstr.util-domain :as util-domain]
   [monstr.view-new-identity :as view-new-identity]
   [monstr.view-relays :as view-relays]
   [monstr.util-java :as util-java]
   [monstr.util-fx :as util-fx]
   [monstr.view-common :as view-common]
   [monstr.view-reply :as view-reply]
   [monstr.timeline :as timeline]
   [clojure.string :as str]
   [clojure.set :as set]
   [monstr.metadata :as metadata])
  (:import (javafx.scene.canvas Canvas)
           (javafx.scene.paint Color)
           (javafx.geometry Pos)
           (javafx.scene.layout VBox Priority)
           (javafx.scene.control TextFormatter$Change TextArea)
           (javafx.beans.property ReadOnlyProperty)))

(defn avatar [{:keys [width picture-url]}]
  (when-not (str/blank? picture-url)
    {:fx/type :image-view
     :image (cache/get* avatar/image-cache [picture-url width])}))

(defn keycard
  [{:keys [active? profile?]
    {:keys [public-key] :as identity_} :identity_
    ;; possibly nil:
    {:keys [name about picture-url nip05-id]} :this-identity-metadata}]
  (let [avatar-dim 75.0
        avatar-color (avatar/color public-key)]
    {:fx/type :h-box
     :style (cond-> {}
              active? (assoc :-fx-border-color avatar-color))
     :style-class ["ndesk-keycard"
                   (when active?
                     "ndesk-keycard-active")]
     :on-mouse-clicked {:event/type :click-keycard :public-key public-key}
     :children
     [(if (str/blank? picture-url)
        {:fx/type :label
         :min-width avatar-dim
         :min-height avatar-dim
         :max-width avatar-dim
         :max-height avatar-dim
         :style {:-fx-background-color avatar-color}
         :style-class "ndesk-keycard-photo"
         :text (subs public-key 0 3)}
        {:fx/type avatar
         :picture-url picture-url
         :width avatar-dim})
      {:fx/type :v-box
       :h-box/hgrow :always
       :children
       [{:fx/type :border-pane
         :max-width Integer/MAX_VALUE
         :left {:fx/type :h-box
                :children
                (cond-> []
                  name (conj {:fx/type :label
                              :alignment :top-left
                              :style-class ["label" "ndesk-keycard-name"]
                              :text name})
                  true (conj {:fx/type :label
                              :alignment :top-left
                              :style-class ["label" "ndesk-keycard-pubkey"]
                              :text (if profile?
                                      ;; In the profile tab we have enough room to
                                      ;; show the whole public key.
                                      public-key
                                      (util/format-pubkey-short public-key))}))}
         :right {:fx/type :hyperlink :text "X"
                 :on-action {:event/type :delete-keycard :identity_ identity_}}}
        {:fx/type :label
         :style-class "ndesk-keycard-about"
         :text (or about "")}]}]}))

(defn keycard-create-new
  [{:keys [show-new-identity? new-identity-error]}]
  {:fx/type fx/ext-let-refs
   :refs {:dialog {:fx/type view-new-identity/dialog
                   :show-new-identity? show-new-identity?
                   :new-identity-error new-identity-error}}
   :desc {:fx/type :h-box
          :cursor :hand
          :style-class ["ndesk-keycard"]
          :on-mouse-clicked {:event/type :show-new-identity}
          :children
          [{:fx/type :label
            :text "add identity"}]}})

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
          {:fx/type avatar
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

(defn add-timeline-button
  [text]
  {:fx/type :button
   :padding 5
   :on-mouse-pressed {:event/type :show-add-timeline-dialog}
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
              

(defn hidden-columns [all-columns visible-column-ids]
  "Returns a list of the ids of those columns that are not visible."
  (sort (seq (set/difference (set (map :id all-columns))
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
  [{:keys [label content]}]
  {:fx/type :tab
   :closable false
   :text label
   :content content})

(defn main-panes
  [{:keys [all-columns visible-column-ids
           relays show-add-timeline-dialog? can-publish? active-reply-context active-contact-pubkey metadata-cache]}]
  (log/debugf "Main panes with %d columns=%s and views %s"
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
              ;; TODO: For now we assume that the relay-url set contains only one
              ;; element. We probably want to move towards arbitrary-sized relay-url sets
              ;; instead.
              (let [column (domain/find-column-by-id column-id)
                    name (:name (:view column))
                    show-thread? (:show-thread? column)
                    listview (if show-thread?
                               (:thread-listview column)
                               (:flat-listview column))]
                (if (nil? column)
                  (log/debugf "No column found for %s" column-id)
                  (log/debugf "Creating pane for column %s with view %s (show-thread=%s listview=%s)"
                              column-id (pr-str (:view column))
                              show-thread? listview))
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
                                                                 (hidden-columns all-columns
                                                                                 visible-column-ids))))
                                                  (add-timeline-button "+"))])}
                            {:fx/type main-pane
                             :listview listview}]}))
            visible-column-ids)
       ;; If there are no relay timelines, just show an "Add timeline" button (centered).
       [{:fx/type :h-box :h-box/hgrow :always}
        (add-timeline-button "Add timeline")
        {:fx/type :h-box :h-box/hgrow :always}])}]})


(defn profile
  [{:keys [pubkey identities identity-metadata]}]
  (let [identity (first (filter (fn [id] (= (:public-key id) pubkey))
                                identities))]
    (log/debugf "Profile for %s with identity %s" pubkey identity)
    (if (and pubkey identity)
      {:fx/type keycard
       :fx/key pubkey
       :active? false
       :profile? true
       :identity_ identity
       :this-identity-metadata (get identity-metadata pubkey)}
      {:fx/type :label
       :text "No pubkey selected for profile"})))


(defn new-column-dialog
  [{:keys [all-columns visible-column-ids new-timeline show-add-timeline-dialog?]}]
  (let [items (hidden-columns all-columns visible-column-ids)]
    {:fx/type :choice-dialog
     :selected-item (first items)
     :title "New column"
     :showing show-add-timeline-dialog?
     :header-text "Add a column"
     :on-close-request {:event/type :add-timeline-close-request}
     :items items}))

(defn tab-pane
  [{:keys [all-columns visible-column-ids views selected-view temp-view
           relays show-add-timeline-dialog? new-timeline
           can-publish? active-reply-context active-contact-list
           active-key active-contact-pubkey identities
           identity-metadata metadata-cache
           ]}]
  (log/debugf "Tab pane with columns=%s and show=%s"
              (pr-str visible-column-ids)
              show-add-timeline-dialog?)
  {:fx/type fx/ext-let-refs
   :refs {:dialog {:fx/type view-reply/dialog
                   :active-reply-context active-reply-context}
          :timeline-dialog {:fx/type new-column-dialog
                            :all-columns all-columns
                            :visible-column-ids visible-column-ids
                            :new-timeline new-timeline
                            :show-add-timeline-dialog? show-add-timeline-dialog?}}
   :desc {:fx/type :tab-pane
          :side :top
          :tabs (for [[label content]
                      {"Home" {:fx/type main-panes
                               :all-columns all-columns
                               :visible-column-ids visible-column-ids
                               :can-publish? can-publish?
                               :show-add-timeline-dialog? show-add-timeline-dialog?
                               :active-reply-context active-reply-context
                               :active-contact-list active-contact-list
                               :active-contact-pubkey active-contact-pubkey
                               :metadata-cache metadata-cache
                               }
                       "Contacts" {:fx/type contacts
                                   :active-contact-list active-contact-list
                                   :active-contact-pubkey active-contact-pubkey
                                   :metadata-cache metadata-cache},
                       ;;"Messages" {:fx/type messages}
                       "Profile" {:fx/type profile
                                  :pubkey active-key
                                  :identities identities
                                  :identity-metadata identity-metadata},
                       "Views" {:fx/type tab-views/show-tab
                                :views views
                                :selected-view selected-view
                                :temp-view temp-view
                                },
                       ;;"Search" {:fx/type search}
                       }]
                  {:fx/type tab*
                   :label label
                   :content content})}})

(defn keycards
  [{:keys [active-key identities identity-metadata show-new-identity? new-identity-error]}]
  {:fx/type :v-box
   :style-class "ndesk-lhs-pane"
   :children (vec
               (concat
                 (map
                   #(hash-map
                      :fx/type keycard
                      :fx/key (:public-key %)
                      :active? (= active-key (:public-key %))
                      :identity_ %
                      :this-identity-metadata (get identity-metadata (:public-key %)))
                   identities)
                 [{:fx/type keycard-create-new
                   :show-new-identity? show-new-identity?
                   :new-identity-error new-identity-error}]))})

(defn relay-dot
  [{:keys [connected-info] {:keys [url read? write?] :as _relay} :relay}]
  {:fx/type :label
   :style {:-fx-padding [0 2]}
   :tooltip
   {:fx/type :tooltip
    :text (format "%s%s" url
            (cond
              (and read? write?) ""
              read? " (read-only)"
              write? " (write-only)"
              :else " (disabled)"))}
   :graphic
   (let [dim 12]
     {:fx/type :canvas
      :width dim
      :height dim
      :draw
      (fn [^Canvas canvas]
        (doto (.getGraphicsContext2D canvas)
          (.setFill (if (get connected-info url) Color/LIGHTGREEN Color/LIGHTGREY))
          (.fillOval 0 0 dim dim)))})})

(defn relay-dots
  [{:keys [relays connected-info]}]
  {:fx/type :h-box
   :style {:-fx-padding [0 5 0 0]}
   :children
   (mapv #(hash-map
            :fx/type relay-dot
            :relay %
            :connected-info connected-info) relays)})

(defn status-relays
  [{:keys [show-relays? relays refresh-relays-ts connected-info]}]
  {:fx/type fx/ext-let-refs
   :refs {:dialog {:fx/type view-relays/dialog
                   :show-relays? show-relays?
                   :relays relays
                   :refresh-relays-ts refresh-relays-ts}}
   :desc {:fx/type :h-box
          :style-class ["ndesk-status-relays"]
          :alignment :center
          :children [{:fx/type :text :text "Relays: "}
                     (if (nil? relays)
                       {:fx/type :text :text "..."}
                       {:fx/type relay-dots :relays relays :connected-info connected-info})]
          :cursor :hand
          :on-mouse-clicked {:event/type :show-relays}}})

(defn status-bar [{:keys [show-relays? relays refresh-relays-ts connected-info]}]
  {:fx/type :border-pane
   :style-class "ndesk-status-bar"
   :left {:fx/type :h-box :children []}
   :right {:fx/type status-relays
           :show-relays? show-relays?
           :relays relays
           :refresh-relays-ts refresh-relays-ts
           :connected-info connected-info}})

(defn identity-selector
  [{:keys [identities active-key identity-metadata show-new-identity?]}]
  {:fx/type :h-box
   ; :alignment :center
   :padding 10
   :children (let [label {:fx/type :label
                          :alignment :center
                          :padding 10
                          :text "Identity: "}
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
              

(defn root [{:keys [all-columns visible-column-ids views selected-view temp-view
                    show-relays? active-key identities identity-metadata
                    relays refresh-relays-ts connected-info
                    show-add-timeline-dialog? new-timeline
                    show-new-identity? new-identity-error active-reply-context contact-lists
                    identity-active-contact metadata-cache
                    last-refresh
                    ]}]
  (log/debugf "Root with columns=%s" (pr-str visible-column-ids))
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
            :all-columns all-columns
            :visible-column-ids visible-column-ids
            :views views
            :selected-view selected-view
            :temp-view temp-view
            :relays relays
            :show-add-timeline-dialog? show-add-timeline-dialog?
            :new-timeline new-timeline
            :can-publish? (util-domain/can-publish? active-key identities)
            :active-key active-key
            :active-reply-context active-reply-context
            :active-contact-list (get contact-lists active-key)
            :active-contact-pubkey (get identity-active-contact active-key)
            :identities identities
            :identity-metadata identity-metadata
            :metadata-cache metadata-cache
            }
   :bottom {:fx/type status-bar
            :show-relays? show-relays?
            :relays relays
            :refresh-relays-ts refresh-relays-ts
            :connected-info connected-info}})

(defn stage [{:keys [all-columns visible-column-ids
                     show-relays? active-key identities identity-metadata
                     relays refresh-relays-ts connected-info
                     show-add-timeline-dialog? new-timeline
                     show-new-identity? new-identity-error active-reply-context
                     contact-lists identity-active-contact metadata-cache
                     last-refresh views selected-view temp-view
                     ]}]
  (log/debugf "Stage with %d identities and active key %s" (count identities) active-key)
  {:fx/type :stage
   :showing true
   :title "Monstr"
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
           :all-columns all-columns
           :visible-column-ids visible-column-ids
           :views views
           :selected-view (or selected-view (:name (first (vals views))))
           :temp-view (or temp-view (first (vals views)))
           :show-add-timeline-dialog? show-add-timeline-dialog?
           :new-timeline new-timeline
           :metadata-cache metadata-cache
           }}})
