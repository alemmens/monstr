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

(def avatar-dim 40)

(defn avatar [{:keys [picture-url]}]
  {:fx/type :image-view
   :image (cache/get* avatar/image-cache [picture-url avatar-dim])})

(defn url-summarize [url]
  url)

(defn- hex-str? [s]
  (and (string? s) (re-matches #"[a-f0-9]{32,}" s)))

(defn- resolve-tag*
  [tag-idx tags-vec]
  (when (and (vector? tags-vec) (< tag-idx (count tags-vec)))
    (let [tag-val (get tags-vec tag-idx)]
      (when (and (vector? tag-val)
              (>= (count tag-val) 2)
              (#{"p" "e"} (first tag-val))
              (hex-str? (second tag-val)))
        tag-val))))

(defn- append-content-with-nostr-tags!*
  [^GenericStyledArea x content tags metadata-cache]
  (let [found-nostr-tags (links/detect-nostr-tags content)]
    (loop [cursor 0 [[i j tag-idx] :as more-found-nostr-tags] found-nostr-tags]
      (if i
        (let [[resolved-tag-letter resolved-tag-val] (resolve-tag* tag-idx tags)
              p-tag? (= "p" resolved-tag-letter)
              e-tag? (= "e" resolved-tag-letter)
              ;; todo and we need to use metadata names instead of pubkeys when possible
              tag-link-text (cond
                              p-tag? (format "@%s"
                                       (or
                                         (some->>
                                           resolved-tag-val
                                           (metadata/get* metadata-cache)
                                           :name
                                           not-empty)
                                         (util/format-pubkey-short resolved-tag-val)))
                              e-tag? (format "@%s" (util/format-event-id-short resolved-tag-val))
                              :else (format "#[%d]" tag-idx))]
          (rich-text/append-text! x (subs content cursor i))
          (if resolved-tag-letter
            (rich-text/append-hyperlink! x tag-link-text
              ;; todo this is all wrong of course
              (format "nostr://%s"
                (cond
                  p-tag? (format "%s" resolved-tag-val)
                  e-tag? (format "event/%s" resolved-tag-val)
                  :else "<missing>")))
            (rich-text/append-text! x tag-link-text))
          (recur j (next more-found-nostr-tags)))
        (rich-text/append-text! x (subs content cursor (count content)))))))

(defn create-content-node*
  [content tags metadata-cache]
  (let [use-content (-> content
                      (str/replace #"(\r\n){2,}" "\r\n")
                      (str/replace #"\n{2,}" "\n")
                      (str/replace #"\r{2,}" "\r"))
        ^GenericStyledArea x (rich-text/create*)]
    (util-fx/add-style-class! x "ndesk-timeline-item-content")
    (HBox/setHgrow x Priority/ALWAYS)
    (.setWrapText x true)
    ;; @see https://github.com/FXMisc/RichTextFX/issues/674#issuecomment-429606510
    (.setAutoHeight x true)
    (.setMaxHeight x Integer/MAX_VALUE)
    (.setEditable x false)
    (.addEventFilter x
      ScrollEvent/SCROLL
      (util-java/->EventHandler
        (fn [^Event e]
          (.consume e)
          (when-let [p (.getParent x)]
            (.fireEvent p (.copyFor e (.getSource e) p))))))
    (let [found (links/detect use-content)]
      (loop [cursor 0 [[a b] :as found] found]
        (if a
          (let [sub-content (subs use-content cursor a)
                link-url (subs use-content a b)]
            (append-content-with-nostr-tags!* x sub-content tags metadata-cache)
            (rich-text/append-hyperlink! x
              (url-summarize link-url) link-url)
            (recur b (next found)))
          (append-content-with-nostr-tags!*
            x (subs use-content cursor (count use-content)) tags metadata-cache))))
    ;; shall we not argue with this? there mere presence of this listener seems
    ;; to fix height being left rendered too short:
    (.addListener (.totalHeightEstimateProperty x)
      (util-java/->ChangeListener
        (fn [_])))
    x))

(defn timeline-item-content
  [{:keys [content tags metadata-cache]}]
  {:fx/type :h-box
   :style-class ["ndesk-timeline-item-content-outer"]
   :children [{:fx/type fx/ext-instance-factory
               :create #(create-content-node* content tags metadata-cache)}]})

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

(defonce ^Popup singleton-popup
  (fx/instance
    (fx/create-component
      {:fx/type :popup
       :anchor-location :window-top-left
       :auto-hide true
       :auto-fix false
       :on-hidden (fn [_])
       :content [{:fx/type :v-box
                  :style-class "ndesk-info-popup-region"
                  :padding 20
                  :style {:-fx-background-color :white}
                  :effect {:fx/type :drop-shadow}
                  :on-mouse-exited
                  (fn [^MouseEvent x]
                    (let [popup (.getWindow
                                  (.getScene ^Node
                                    (.getSource x)))]
                      (.hide popup)))
                  :children
                  [{:fx/type :label
                    :style {:-fx-font-weight :bold}
                    :style-class ["label" "ndesk-info-popup-event-id"]}
                   {:fx/type :label
                    :style-class ["label" "ndesk-info-popup-seen-on"]}
                   {:fx/type :hyperlink
                    :style-class ["hyperlink" "ndesk-info-popup-copy-event-link"] ;; used in .lookup
                    :text "Copy event id"
                    :on-action (fn [^ActionEvent e]
                                 (when-let [content
                                            (some-> e ^Hyperlink .getSource .getUserData :event-id)]
                                   (util/put-clipboard! content)))}
                   {:fx/type :hyperlink
                    :style-class ["hyperlink" "ndesk-info-popup-copy-author-pubkey-link"] ;; used in .lookup
                    :text "Copy author pubkey"
                    :on-action (fn [^ActionEvent e]
                                 (when-let [content
                                            (some-> e ^Hyperlink .getSource .getUserData :author-pubkey)]
                                   (util/put-clipboard! content)))}]}]})))

(defn- ready-popup!
  ^Popup [db popup-width item-id author-pubkey]
  (let [event-id-short (util/format-event-id-short item-id)
        seen-on-relays (store/get-seen-on-relays db item-id)
        ^VBox v-box (first (seq (.getContent singleton-popup)))
        ^Label event-id-label (.lookup v-box ".ndesk-info-popup-event-id")
        ^Label seen-on-label (.lookup v-box ".ndesk-info-popup-seen-on")
        ^Hyperlink copy-event-id-hyperlink (.lookup v-box ".ndesk-info-popup-copy-event-link")
        ^Hyperlink copy-author-pubkey-hyperlink (.lookup v-box ".ndesk-info-popup-copy-author-pubkey-link")]
    (.setUserData copy-event-id-hyperlink {:event-id item-id})
    (.setUserData copy-author-pubkey-hyperlink {:author-pubkey author-pubkey})
    (.setText event-id-label (str "Event: " event-id-short))
    (.setText seen-on-label (str/join "\n" (cons "Seen on:" seen-on-relays)))
    (.setMinWidth v-box popup-width)
    (.setMaxWidth v-box popup-width)
    (.setPrefWidth v-box popup-width)
    singleton-popup))

(defn- show-info!
  [db item-id author-pubkey ^ActionEvent e]
  (let [^Hyperlink node (.getSource e)
        popup-width 250
        popup (ready-popup! db popup-width item-id author-pubkey)]
    (let [bounds (.getBoundsInLocal node)
          node-pos (.localToScreen node (* 0.5 (.getWidth bounds)) 0.0)]
      (.show popup node
        (- (.getX node-pos) (* 0.5 popup-width))
        (.getY node-pos)))))

(defn timeline-item
  [{:keys [^UITextNote root-data ^UITextNote item-data *state db metadata-cache executor]}]
  (if (:missing? item-data)
    {:fx/type :h-box
     :style-class ["ndesk-timeline-item-missing"]
     :children [{:fx/type :hyperlink
                 :h-box/hgrow :always
                 :style-class ["ndesk-timeline-item-missing-hyperlink"]
                 :text "load parent message..."
                 :on-action (fn [^ActionEvent _e]
                              ;; get off of fx thread
                              (util/submit! executor
                                (fn []
                                  (load-event/async-load-event! *state db executor (:id item-data)))))}]}
    ;; else -- not missing
    (let [item-id (:id item-data)
          pubkey (:pubkey item-data)
          pubkey-for-avatar (or (some-> pubkey (subs 0 3)) "?")
          ;pubkey-short (or (some-> pubkey util/format-pubkey-short) "?")
          timestamp (:timestamp item-data)
          content (:content item-data)
          tags (:tags item-data)
          {:keys [name about picture-url nip05-id created-at]} (some->> pubkey (metadata/get* metadata-cache))
          avatar-color (or (some-> pubkey avatar/color) :lightgray)]
      {:fx/type :border-pane
       :on-mouse-entered (partial show-reply-button!* *state true)
       :on-mouse-exited (partial show-reply-button!* *state false)
       :left (if picture-url
               {:fx/type avatar
                :picture-url picture-url}
               {:fx/type :label
                :min-width avatar-dim
                :min-height avatar-dim
                :max-width avatar-dim
                :max-height avatar-dim
                :style {:-fx-background-color avatar-color}
                :style-class "ndesk-timeline-item-photo"
                :text pubkey-for-avatar})
       :center {:fx/type :border-pane
                :top {:fx/type :border-pane
                      :border-pane/margin (Insets. 0.0 5.0 0.0 5.0)
                      :left {:fx/type :h-box
                             :children [{:fx/type :label
                                         :style-class "ndesk-timeline-item-name"
                                         :text name}
                                        {:fx/type :label
                                         :style-class "ndesk-timeline-item-pubkey"
                                         :text pubkey}]}
                      :right {:fx/type :h-box
                              :children [{:fx/type :hyperlink
                                          :style-class ["label" "ndesk-timeline-item-info-link"] ;; used for .lookup
                                          :visible false
                                          :text "info"
                                        :on-action (partial show-info! db item-id pubkey)}
                                         {:fx/type :label
                                          :text (or (some-> timestamp util/format-timestamp) "?")}]}}
                :bottom {:fx/type :h-box
                         :children [{:fx/type timeline-item-content
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
  (let [spacer-width (* indent 25)]
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
        (mapcat #(tree-rows* (inc indent) root-data % expand? *state db metadata-cache executor) (:children item-data))))))

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
           (tree-rows*
             0
             root
             (if expanded?
               root
               (or
                 (find-note root
                   #(= (:timestamp %) max-timestamp))
                 ;; should never get:
                 root))
             expanded?
             *state
             db
             metadata-cache
             executor)
           ;; this is a bad experience so far so we disable collapse altogether for now
           #_(when (> note-count 1)
               [{:fx/type :hyperlink
                 :text (if expanded? "collapse" (format "expand (%d notes)" note-count))
                 :on-action (fn [_]
                              (timeline/toggle! *state (-> note-wrapper :root :id)))}])))})))

(defn home [{:keys [*state db metadata-cache executor]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created #(.setSelectionModel % util-fx-more/no-selection-model)
   :desc {:fx/type :list-view
          :focus-traversable false
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
