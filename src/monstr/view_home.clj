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

(defn thread-timeline-item
  [{:keys [^UITextNote root-data ^UITextNote item-data *state db metadata-cache executor]}]
  (if (:missing? item-data)
    {:fx/type :h-box
     :style-class ["ndesk-timeline-item-missing"]
     :children [(do
                  ;; Load the parent (Nostr) thread. 
                  (util/submit! executor ; get off of fx thread
                                (fn []
                                  ;; Wait a bit so we don't overload the relays.
                                  ;; TODO: we should use a queue instead of this black magic!
                                  (Thread/sleep 50)
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
  [indent ^UITextNote root-data ^UITextNote item-data *state db metadata-cache executor]
  (let [spacer-width (* indent 10)]
    (cons
      {:fx/type :h-box
       :children [{:fx/type :label
                   :min-width spacer-width
                   :max-width spacer-width
                   :text ""}
                  {:fx/type thread-timeline-item
                   :h-box/hgrow :always
                   :spacer-width spacer-width
                   :item-data item-data
                   :root-data root-data
                   :*state @domain/*state
                   :db store/db
                   :metadata-cache metadata/cache
                   :executor executor}]}
      (mapcat #(tree-rows* (inc indent) root-data % *state db metadata-cache executor)
              (:children item-data)))))

(defn- find-note
  [^UITextNote note pred]
  (if (pred note) note (first (map #(find-note % pred) (:children note)))))

(defn- tree* [{:keys [^UITextNoteWrapper note-wrapper *state db metadata-cache executor]}]
  ;; NOTE: we get nil note-wrapper sometimes when the list-cell is advancing
  ;; in some ways. For now just render label w/ err which we'll see if
  ;; this matters.
  (if (nil? note-wrapper)
    {:fx/type :label :text "err"}
    (let [root (:root note-wrapper)]
      {:fx/type :v-box
       :children (vec (tree-rows* 0 root root
                                  *state db metadata-cache executor))})))

(defn thread-home [{:keys [*state db metadata-cache executor]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created #(.setSelectionModel % util-fx-more/no-selection-model)
   :desc {:fx/type :list-view
          :focus-traversable false
          :style-class ["monstr-thread-pane-listview"]
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


#_
(defn create-list-view
  ^ListView [*state db metadata-cache executor]
  (fx/instance
    (fx/create-component {:fx/type home
                          :*state *state
                          :db db
                          :metadata-cache metadata-cache
                          :executor executor})))

(defonce ^Popup singleton-thread-pane
  (fx/instance
   (fx/create-component
    {:fx/type :popup
     :anchor-location :window-top-left
     :auto-hide true
     :auto-fix false
     :content [{:fx/type :v-box
                :style-class "ndesk-thread-pane-vbox"
                :padding 20
                :style {:-fx-background-color :white}
                :effect {:fx/type :drop-shadow}
                :on-mouse-exited (fn [^MouseEvent x]
                                   (let [popup (.getWindow (.getScene ^Node (.getSource x)))]
                                     (.hide popup)))
                :children [{:fx/type :label
                            :text "thread"
                            :style {:-fx-font-weight :bold}
                            :style-class ["label"]}
                           {:fx/type thread-home}]}]})))

(defn find-thread-listview []
  (let [v-box (first (seq (.getContent singleton-thread-pane)))]
    (.lookup v-box "monstr-thread-pane-listview")))
 
(defn prepare-thread-pane!
  [node-pos width event-id]
  ;; Nothing for now.
  singleton-thread-pane)

(defn show-thread-pane!
  [event-id ^ActionEvent e]
  (let [node (.getSource e)
        bounds (.getBoundsInLocal node)
        width 700
        node-pos (.localToScreen node (* 0.5 (.getWidth bounds)) 0.0)
        pane (prepare-thread-pane! node-pos width event-id)]
    (let [stage (-> node .getScene .getWindow)]
      (.show pane stage))))

