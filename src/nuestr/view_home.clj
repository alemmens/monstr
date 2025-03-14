(ns nuestr.view-home
  (:require [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [nuestr.cache :as cache]
            [nuestr.domain :as domain]
            [nuestr.links :as links]
            [nuestr.media :as media]
            [nuestr.modal :as modal]
            [nuestr.metadata :as metadata]
            [nuestr.nip19 :as nip19]
            [nuestr.relay-conn :as relay-conn]
            [nuestr.rich-text :as rich-text]
            [nuestr.status-bar :as status-bar]
            [nuestr.store :as store]
            [nuestr.style :as style :refer [BORDER|]]
            [nuestr.tab-profile :as tab-profile]
            [nuestr.timeline :as timeline]
            [nuestr.util :as util]
            [nuestr.util-domain :as util-domain]
            [nuestr.util-fx :as util-fx]
            [nuestr.util-fx-more :as util-fx-more]
            [nuestr.util-java :as util-java]
            )
  (:import
   (javafx.collections FXCollections ObservableList)
   (javafx.event ActionEvent Event)
   (javafx.geometry Insets)
   (javafx.scene Node)
   (javafx.scene.control Hyperlink Label ListView)
   (javafx.scene.input MouseEvent ScrollEvent)
   (javafx.scene.layout HBox Priority VBox)
   (javafx.stage Popup)
   (nuestr.domain TextNoteNew TextNote TextNoteWrapper)
   (org.fxmisc.richtext GenericStyledArea)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn url-summarize [url metadata-cache]
  (cond (str/starts-with? url "nostr:npub")
        (let [npub (subs url (count "nostr:"))
              [_ pubkey] (nip19/decode npub)]
          (str "@"
               (or (when pubkey
                     (:name (metadata/get* metadata-cache pubkey)))
                   (util/format-string-short npub))))
        (str/starts-with? url "nostr:note")
        (let [note (subs url (count "nostr:"))
              [_ event] (nip19/decode note)]
          (if event
            (str "@" (util/format-string-short note))
            url))
        :else url))

(defn- hex-str? [s]
  (and (string? s) (re-matches #"[a-f0-9]{32,}" s)))

(defn- resolve-tag* [tag-idx tags-vec]
  (when (and (vector? tags-vec) (< tag-idx (count tags-vec)))
    (let [tag-val (get tags-vec tag-idx)]
      (when (and (vector? tag-val)
                 (>= (count tag-val) 2)
                 (#{"p" "e"} (first tag-val))
                 (hex-str? (second tag-val)))
        tag-val))))

(defn- append-content-with-nostr-tags!*
  [^GenericStyledArea x content tags column-id pubkey metadata-cache]
  (let [found-nostr-tags (links/detect-nostr-tags content)]
    (loop [cursor 0 [[i j tag-idx] :as more-found-nostr-tags] found-nostr-tags]
      (if i
        (let [[resolved-tag-letter resolved-tag-val] (resolve-tag* tag-idx tags)
              p-tag? (= "p" resolved-tag-letter)
              e-tag? (= "e" resolved-tag-letter)
              tag-link-text (cond
                              p-tag? (format "@%s"
                                       (or (some->> resolved-tag-val
                                                    (metadata/get* metadata-cache)
                                                    :name
                                                    not-empty)
                                         (util/format-pubkey-short resolved-tag-val)))
                              e-tag? (format "@%s" (util/format-event-id-short resolved-tag-val))
                              :else (format "#[%d]" tag-idx))]
          (rich-text/append-text! x (subs content cursor i))
          (if resolved-tag-letter
            (let [url (format "nostr:%s"
                              (cond p-tag? (format "%s" (nip19/encode "npub" resolved-tag-val))
                                    e-tag? (format "%s" (nip19/encode "nevent" resolved-tag-val))
                                    :else "<missing>"))]
              (rich-text/append-hyperlink! x tag-link-text url column-id pubkey))
            (rich-text/append-text! x tag-link-text))
          (recur j (next more-found-nostr-tags)))
        (rich-text/append-text! x (subs content cursor (count content)))))))

(defn create-content-node* [content links tags column-id pubkey metadata-cache]
  (let [^GenericStyledArea x (rich-text/create* column-id pubkey)]
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
    (loop [cursor 0
           [[a b] :as found] links]
      (if a
        (let [sub-content (subs content cursor a)
              link-url (subs content a b)]
          (append-content-with-nostr-tags!* x sub-content tags column-id pubkey metadata-cache)
          (rich-text/append-hyperlink! x
                                       (url-summarize link-url metadata-cache)
                                       link-url
                                       column-id
                                       pubkey)
          (recur b (next found)))
        (append-content-with-nostr-tags!* x
                                          (subs content cursor (count content))
                                          tags
                                          column-id
                                          pubkey
                                          metadata-cache)))
    ;; Shall we not argue with this? The mere presence of this listener seems
    ;; to fix height being left rendered too short.
    (.addListener (.totalHeightEstimateProperty x)
      (util-java/->ChangeListener
        (fn [_])))
    x))

(defn timeline-item-content
  [{:keys [column-id pubkey event content tags metadata-cache]}]
  (let [column (domain/find-column-by-id column-id)
        view (:view column)
        clean-content (-> content
                          (str/replace #"(\r\n){2,}" "\r\n")
                          (str/replace #"\n{2,}" "\n")
                          (str/replace #"\r{2,}" "\r"))
        links (links/detect clean-content)
        link-strings (map (fn [[start end]] (subs content start end))
                          links)
        image-links (keep #(when (or (str/ends-with? % ".bmp")
                                     (str/ends-with? % ".gif")
                                     (str/ends-with? % ".jpg")
                                     (str/ends-with? % ".jpeg")
                                     (str/ends-with? % ".png"))
                             %)
                          link-strings)
        content-node {:fx/type :h-box
                      :style-class ["ndesk-timeline-item-content-outer"]
                      :children [{:fx/type fx/ext-instance-factory
                                  :create #(create-content-node* clean-content links tags
                                                                 column-id pubkey
                                                                 metadata-cache)}]}]
    (if (and (:show-pictures view)
             (seq image-links))
      {:fx/type :v-box
       :spacing 5
       :children (cons content-node
                       (map (fn [url]
                              {:fx/type :h-box
                               :padding 10
                               :children [(media/show-picture {:url url :width 300})]})
                            image-links))}
      content-node)))

(defonce ^Popup singleton-popup
  (fx/instance
    (fx/create-component
      {:fx/type :popup
       :anchor-location :window-top-left
       :auto-hide true
       :auto-fix false
       :on-hidden (fn [_])
       :content [{:fx/type :v-box
                  :style-class "nuestr-info-popup"
                  :padding 20
                  :spacing 5
                  :style {:-fx-background-color :white}
                  :effect {:fx/type :drop-shadow}
                  :on-mouse-exited
                  (fn [^MouseEvent x]
                    (let [popup (.getWindow (.getScene ^Node (.getSource x)))]
                      (.hide popup)))
                  :children
                  [{:fx/type :h-box
                    :spacing 8
                    :children [{:fx/type :label
                                :padding 2
                                ; :style {:-fx-font-weight :bold}
                                :style-class ["label" "ndesk-info-popup-event-id"]}
                               {:fx/type :hyperlink
                                :style-class ["hyperlink" "ndesk-info-popup-copy-event-link"] ; used in .lookup
                                :text "copy"
                                :on-action (fn [^ActionEvent e]
                                             (when-let [content
                                                        (some-> e ^Hyperlink .getSource .getUserData :event-id)]
                                               (util/put-clipboard! (nip19/encode "note" content))))}]}
                   {:fx/type :h-box
                    :spacing 8
                    :children [{:fx/type :label
                                :padding 2
                                ; :style {:-fx-font-weight :bold}
                                :style-class ["label" "ndesk-info-popup-pubkey"]}
                               {:fx/type :hyperlink
                                :style-class ["hyperlink" "ndesk-info-popup-copy-author-pubkey-link"] ; used in .lookup
                                :text "copy"
                                :on-action (fn [^ActionEvent e]
                                             (when-let [content
                                                        (some-> e ^Hyperlink .getSource .getUserData :author-pubkey)]
                                               (util/put-clipboard! (nip19/encode "npub" content))))}]}
                   {:fx/type :label
                    :text "Relays:"}
                   {:fx/type :list-view
                    :style-class "nuestr-info-relays"
                    :focus-traversable false
                    :pref-height 120
                    :pref-width 160
                    :items []}]}]})))

(defn- ready-popup! ^Popup [db node-pos popup-width item-id author-pubkey]
  (let [event-id-short (util/format-event-id-short item-id)
        seen-on-relays (store/get-seen-on-relays db item-id)
        ^VBox v-box (first (seq (.getContent singleton-popup)))
        ^Label event-id-label (.lookup v-box ".ndesk-info-popup-event-id")
        ^Label pubkey-label (.lookup v-box ".ndesk-info-popup-pubkey")
        ^ListView seen-on-list (.lookup v-box ".nuestr-info-relays")
        ^Hyperlink copy-event-id-hyperlink (.lookup v-box ".ndesk-info-popup-copy-event-link")
        ^Hyperlink copy-author-pubkey-hyperlink (.lookup v-box ".ndesk-info-popup-copy-author-pubkey-link")]
    ;; Set the content.
    (.setText event-id-label (str "Event id: " (util/format-string-short (nip19/encode "note" item-id))))
    (.setUserData copy-event-id-hyperlink {:event-id item-id})
    (.setText pubkey-label (str "Author: " (util/format-string-short (nip19/encode "npub" author-pubkey))))
    (.setUserData copy-author-pubkey-hyperlink {:author-pubkey author-pubkey})
    (let [items (FXCollections/observableList (vec seen-on-relays))]
      (.setItems seen-on-list items))
    ;; Set position and dimensions.
    (.relocate v-box 0 0)
    (.setMinWidth v-box popup-width)
    (.setMaxWidth v-box popup-width)
    (.setPrefWidth v-box popup-width)
    singleton-popup))

(defn show-info! [db item-id author-pubkey ^ActionEvent e]
  (let [node (.getSource e)
        popup-width 300
        bounds (.getBoundsInLocal node)
        node-pos (.localToScreen node (* 0.5 (.getWidth bounds)) 0.0)
        popup (ready-popup! db node-pos popup-width item-id author-pubkey)]
      (let [stage (-> node .getScene .getWindow)]
        (.show popup stage))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Action buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn- show-action-row!
  [*state show? ^MouseEvent e]
  (let [{:keys [active-key identities]} @*state]
    (when-let [^Node target (.getTarget e)]
      ;; Show/hide the action button row.
      (some-> target
              (.lookup ".ndesk-content-controls")
              (.setVisible show?))
      ;; Show/hide the info link.
      (some-> target
              (.lookup ".ndesk-timeline-item-info-link")
              (.setVisible show?))
      ;; Show/hide the reply button.
      (some-> target
              (.lookup ".ndesk-reply-button")
              (.setVisible (and show?
                                (util-domain/can-publish? active-key identities)))))))

(defn- info-button [db item-id pubkey]
  {:fx/type :button
   :style-class ["button" "ndesk-timeline-item-info-link"] ;; used for .lookup
   :text "info"
   :on-action (partial show-info! db item-id pubkey)})

(defn reply-button [*state root-id item-id]
  {:fx/type :button
   :style-class ["button" "ndesk-reply-button"] ;; used for .lookup
   :h-box/margin 3
   :text "reply"
   :on-action (fn [_] (swap! *state assoc :active-reply-context
                             (domain/->UIReplyContext (or root-id item-id)
                                                      item-id)))})
  
(defn- action-button-row
  [*state db event-obj root-data item-id pubkey column-id thread?]
  {:fx/type :h-box
   :style-class ["ndesk-content-controls"] ;; used for .lookup
   :style (BORDER| :lightgrey)
   :visible false
   :alignment :center
   :max-width Integer/MAX_VALUE
   :children (remove nil?
                     [(info-button db item-id pubkey)
                      (reply-button *state
                                    (:id (if (nil? root-data) event-obj root-data))
                                    item-id)
                      (when-not thread?
                        {:fx/type :button
                         :style-class ["button" "ndesk-thread-button"] ;; used for .lookup
                         :h-box/margin 3
                         :text "thread"
                         :on-action (fn [e]
                                      (let [column (and column-id (domain/find-column-by-id column-id))]
                                        #_(log/debugf "Thread button clicked for column %s" (:name (:view column)))
                                        (timeline/show-column-thread! *state column pubkey event-obj)))})])})


(defn- author-pane [name pubkey timestamp]
  {:fx/type :border-pane
   :style (BORDER| :white)
   :border-pane/margin (Insets. 0.0 5.0 0.0 5.0)
   :left {:fx/type :h-box
          :cursor :hand
          :alignment :center
          :style-class "nuestr-author-hbox"
          :on-mouse-clicked (fn [e] (timeline/open-profile e pubkey))
          :children (let [[n domain] (str/split (metadata/user-short-name pubkey) #"@")]
                      (remove nil?
                              [{:fx/type :label
                                :style-class "ndesk-timeline-item-name"
                                :text n}
                               #_
                               (when domain
                                 {:fx/type :label
                                  :style-class "ndesk-timeline-item-pubkey"
                                  :text (str "@" domain)})]))}
   :right {:fx/type :h-box
           :alignment :center
           :children [{:fx/type :label
                       :style-class "ndesk-timeline-item-timestamp"
                       :text (or (some-> timestamp util/format-timestamp) "?")}]}})

(defn show-timeline-item-info [e event-obj]
  (when-not (:missing? event-obj)
    (show-action-row! domain/*state true e)
    (let [seen-on (store/get-seen-on-relays store/db (:id event-obj))]
      (status-bar/message! (format "Seen on %s."
                                   (str/join " and "
                                             (map util/relay-url-short seen-on)))))))

(defn unshow-timeline-item-info [e]
  (show-action-row! domain/*state false e)
  (status-bar/message! ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn timeline-item
  [{:keys [root-data ^TextNoteNew text-note-new column-id *state db metadata-cache executor thread?]}]
  ;; ROOT-DATA is nil for flat view, some kind of event data for threaded view.
  (let [event-obj (:event-obj text-note-new)
        pubkey (:pubkey event-obj)
        pubkey-for-avatar (or (some-> pubkey (subs 0 3)) "?")
        {:keys [name picture-url]} (some->> pubkey (metadata/get* metadata-cache))
        avatar-color (or (some-> pubkey media/color) :lightgray)]
    {:fx/type :border-pane
     :padding (Insets. ; top right bottom left
               0.0 0.0 0.0 (* (:depth text-note-new) 10.0))
     :on-mouse-entered (fn [e] (show-timeline-item-info e event-obj))
     :on-mouse-moved (fn [e] (show-timeline-item-info e event-obj))
     :on-mouse-exited unshow-timeline-item-info
     :left (media/avatar-or-empty-space picture-url avatar-color pubkey-for-avatar)
     :center {:fx/type :border-pane
              :style (BORDER| :white)
              ;; Show name, pubkey and timestamp.              
              :top (author-pane name pubkey (:created_at event-obj))
              ;; Show the actual content.
              :bottom {:fx/type :h-box
                       :children [{:fx/type timeline-item-content
                                   :column-id column-id
                                   :pubkey pubkey
                                   :event event-obj
                                   :h-box/hgrow :always
                                   :content (:content event-obj)
                                   :tags (:tags event-obj)
                                   :metadata-cache metadata-cache}]}}
     ;; The action buttons (Reply, Thread, ...) are at the bottom of the timeline item.
     :bottom (action-button-row *state db event-obj root-data (:id event-obj) pubkey column-id thread?)}))


(defn timeline-item*
  [{:keys [column-id root-data ^TextNoteNew text-note-new relays *state db metadata-cache executor thread?]}]
  ;; note: we get nil note-wrapper sometimes when the list-cell is advancing
  ;; in some ways -- for now just render label w/ err which we'll see if
  ;; this matters --
  (if (nil? text-note-new)
    {:fx/type :label :text "err"}
    {:fx/type timeline-item
     :column-id column-id
     :text-note-new text-note-new
     :root-data root-data ; can be nil
     :relays relays
     :*state *state
     :db db
     :metadata-cache metadata-cache
     :executor executor
     :thread? thread?}))

(defn home [{:keys [column-id pubkey *state db metadata-cache executor relays thread?]}]
  (let [desc {:fx/type :list-view
              :focus-traversable false ; thread?
              :pref-height 100000  ; trick to make it stretch vertically
              :pref-width 100000
              :on-scroll (fn [e]
                           (let [delta-y (.getDeltaY ^ScrollEvent e)]
                             (when (and (< delta-y 0)
                                        ;; Threads don't grow dynamically.
                                        (not thread?))
                               ;; Reached scroll bar bottom: try to add new text notes.
                               (timeline/grow-timeline! column-id pubkey))))
              :cell-factory {:fx/cell-type :list-cell
                             :describe (fn [text-note-new]
                                         {:graphic
                                          {:fx/type timeline-item*
                                           :column-id column-id
                                           :pubkey pubkey
                                           :text-note-new text-note-new
                                           :relays relays
                                           :*state *state
                                           :db db
                                           :metadata-cache metadata-cache
                                           :executor executor
                                           :thread? thread?}})}}]
    (if thread?
      {:fx/type fx/ext-on-instance-lifecycle
       :desc desc}
      {:fx/type fx/ext-on-instance-lifecycle
       :on-created #(.setSelectionModel % util-fx-more/no-selection-model)
       :desc desc})))
      

(defn create-list-view
  ^ListView [column-id pubkey *state db metadata-cache executor]
  ;; Note that COLUMN-ID is nil for timeline list views in Profile tabs.
  (fx/instance
   (fx/create-component {:fx/type home
                         :column-id column-id
                         :pubkey pubkey
                         :*state *state
                         :db db
                         :metadata-cache metadata-cache
                         :executor executor
                         :thread? false})))


(defn create-thread-view
  ^ListView [column-id pubkey *state db metadata-cache executor]
  (fx/instance
   (fx/create-component {:fx/type home
                         :column-id column-id
                         :pubkey pubkey
                         :*state *state
                         :db db
                         :metadata-cache metadata-cache
                         :executor executor
                         :thread? true})))
