(ns monstr.view-home
  (:require
   [cljfx.api :as fx]
   [clojure.tools.logging :as log]
   [monstr.domain]
   [monstr.links :as links]
   [monstr.metadata :as metadata]
   [monstr.style :refer [BORDER|]]
   [monstr.rich-text :as rich-text]
   [monstr.util :as util]
   [monstr.cache :as cache]
   [monstr.avatar :as avatar]
   [monstr.load-event :as load-event]
   [monstr.util-domain :as util-domain]
   [monstr.util-fx :as util-fx]
   [monstr.util-fx-more :as util-fx-more]
   [monstr.util-java :as util-java]
   [monstr.domain :as domain]
   [monstr.view-home-new :as view-home-new]
   [monstr.store :as store]
   [clojure.string :as str])
  (:import
    (monstr.domain UITextNote UITextNoteWrapper)
    (javafx.scene.layout Region HBox Priority VBox)
    (javafx.geometry Insets Bounds)
    (javafx.scene.control ListView Hyperlink Label)
    (javafx.scene.image Image)
    (org.fxmisc.richtext GenericStyledArea)
    (java.util Optional)
    (javafx.event Event ActionEvent)
    (javafx.scene.input ScrollEvent MouseEvent)
    (javafx.scene Node)
    (javafx.stage Popup)))

(defn- show-reply-button!*
  [*state show? ^MouseEvent e]
  (let [{:keys [active-key identities]} @*state]
    (when-let [^Node target (.getTarget e)]
      (some-> target
        (.lookup ".ndesk-timeline-item-info-link")
        (.setVisible show?))
      (when (util-domain/can-publish? active-key identities)
        (some-> target
          (.lookup ".ndesk-content-controls")
          (.setVisible show?))))))

(defn timeline-item
  [{:keys [^UITextNote root-data ^UITextNote item-data *state db metadata-cache executor]}]
  (if (:missing? item-data)
    {:fx/type :h-box
     :style-class ["ndesk-timeline-item-missing"]
     :children [(do
                  ;; get off of fx thread
                  (util/submit! executor
                                (fn []
                                  (load-event/async-load-event! *state db executor (:id item-data))))
                  {:fx/type :hyperlink
                   :h-box/hgrow :always
                   :style-class ["ndesk-timeline-item-missing-hyperlink"]
                   :text "loading parent thread..."})]}
    ;; else -- not missing
    (let [item-id (:id item-data)
          pubkey (:pubkey item-data)
          pubkey-for-avatar (or (some-> pubkey (subs 0 3)) "?")
          pubkey-short (or (some-> pubkey util/format-pubkey-short) "?")
          timestamp (:timestamp item-data)
          content (:content item-data)
          tags (:tags item-data)
          {:keys [name about picture-url nip05-id created-at]} (some->> pubkey (metadata/get* metadata-cache))
          avatar-color (or (some-> pubkey avatar/color) :lightgray)]
      {:fx/type :border-pane
       :on-mouse-entered (partial show-reply-button!* *state true)
       :on-mouse-exited (partial show-reply-button!* *state false)
       :left (view-home-new/avatar-or-empty-space picture-url avatar-color pubkey-for-avatar)
       :center {:fx/type :border-pane
                :top {:fx/type :border-pane
                      :border-pane/margin (Insets. 0.0 5.0 0.0 5.0)
                      :left {:fx/type :h-box
                             :children [{:fx/type :label
                                         :style-class "ndesk-timeline-item-name"
                                         :text name}
                                        {:fx/type :label
                                         :style-class "ndesk-timeline-item-pubkey"
                                         :text pubkey-short}]}
                      :right {:fx/type :h-box
                              :children [{:fx/type :hyperlink
                                          :style-class ["label" "ndesk-timeline-item-info-link"] ;; used for .lookup
                                          :visible false
                                          :text "info"
                                        :on-action (partial view-home-new/show-info! db item-id pubkey)}
                                         {:fx/type :label
                                          :text (or (some-> timestamp util/format-timestamp) "?")}]}}
                :bottom {:fx/type :h-box
                         :children [{:fx/type view-home-new/timeline-item-content
                                     :h-box/hgrow :always
                                     :content content
                                     :tags tags
                                     :metadata-cache metadata-cache}
                                    {:fx/type :h-box
                                     :style-class ["ndesk-content-controls"] ;; used for .lookup
                                     :visible false
                                     :alignment :center-right
                                     :max-width Integer/MAX_VALUE
                                     :children [{:fx/type :button
                                                 :style-class ["button" "ndesk-reply-button"] ;; used for .lookup
                                                 :h-box/margin 3
                                                 :text "reply"
                                                 :on-action
                                                 (fn [_]
                                                   (swap! *state assoc :active-reply-context
                                                     (domain/->UIReplyContext
                                                       (:id root-data) item-id)))}]}]}}})))

(defn- tree-rows*
  [indent ^UITextNote root-data ^UITextNote item-data expand? *state db metadata-cache executor]
  (let [spacer-width (* indent 10)]
    (cons
      {:fx/type :h-box
       :children [{:fx/type :label
                   :min-width spacer-width
                   :max-width spacer-width
                   :text ""}
                  {:fx/type timeline-item
                   :h-box/hgrow :always
                   :spacer-width spacer-width
                   :item-data item-data
                   :root-data root-data
                   :*state *state
                   :db db
                   :metadata-cache metadata-cache
                   :executor executor}]}
      (when expand?
        (mapcat #(tree-rows* (inc indent) root-data % expand? *state db metadata-cache executor)
                (:children item-data))))))

(defn- find-note
  [^UITextNote note pred]
  (if (pred note) note (first (map #(find-note % pred) (:children note)))))

(defn- tree* [{:keys [^UITextNoteWrapper note-wrapper *state db metadata-cache executor]}]
  ;; note: we get nil note-wrapper sometimes when the list-cell is advancing
  ;; in some ways -- for now just render label w/ err which we'll see if
  ;; this matters --
  (if (nil? note-wrapper)
    {:fx/type :label :text "err"}
    (let [{:keys [root expanded? max-timestamp note-count]} note-wrapper]
      {:fx/type :v-box
       :children
       (vec
         (concat
           (tree-rows* 0
                       root
                       (if expanded?
                         root
                         (or (find-note root #(= (:timestamp %) max-timestamp))
                             ;; should never get:
                             root))
                       expanded?
                       *state
                       db
                       metadata-cache
                       executor)))})))

(defn home [{:keys [*state db metadata-cache executor]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created #(.setSelectionModel % util-fx-more/no-selection-model)
   :desc {:fx/type :list-view
          :focus-traversable false
          :pref-height 100000  ; trick to make it stretch vertically
          :cell-factory {:fx/cell-type :list-cell
                         :describe (fn [note-wrapper]
                                     {:graphic
                                      {:fx/type tree*
                                       :note-wrapper note-wrapper
                                       :*state *state
                                       :db db
                                       :metadata-cache metadata-cache
                                       :executor executor}})}}})

(defn create-list-view
  ^ListView [*state db metadata-cache executor]
  (fx/instance
    (fx/create-component {:fx/type home
                          :*state *state
                          :db db
                          :metadata-cache metadata-cache
                          :executor executor})))
