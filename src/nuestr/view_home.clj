(ns nuestr.view-home
  (:require [cljfx.api :as fx]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [nuestr.cache :as cache]
            [nuestr.domain :as domain]
            [nuestr.links :as links]
            [nuestr.media :as media]
            [nuestr.metadata :as metadata]
            [nuestr.relay-conn :as relay-conn]
            [nuestr.rich-text :as rich-text]
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
  (:import (javafx.event ActionEvent Event)
           (javafx.geometry Insets)
           (javafx.scene Node)
           (javafx.scene.control Hyperlink Label ListView)
           (javafx.scene.input MouseEvent ScrollEvent)
           (javafx.scene.layout HBox Priority VBox)
           (javafx.stage Popup)
           (nuestr.domain UITextNoteNew UITextNote UITextNoteWrapper)
           (org.fxmisc.richtext GenericStyledArea)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              ;; TODO: we need to use metadata names instead of pubkeys when possible.
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

(defn create-content-node* [content links tags metadata-cache]
  (let [^GenericStyledArea x (rich-text/create*)]
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
          (append-content-with-nostr-tags!* x sub-content tags metadata-cache)
          (rich-text/append-hyperlink! x
                                       (url-summarize link-url)
                                       link-url)
          (recur b (next found)))
        (append-content-with-nostr-tags!*
         x (subs content cursor (count content)) tags metadata-cache)))
    ;; Shall we not argue with this? The mere presence of this listener seems
    ;; to fix height being left rendered too short.
    (.addListener (.totalHeightEstimateProperty x)
      (util-java/->ChangeListener
        (fn [_])))
    x))

(defn timeline-item-content
  [{:keys [column-id content tags metadata-cache]}]
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
                                  :create #(create-content-node* clean-content links tags metadata-cache)}]}]
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
  ^Popup [db node-pos popup-width item-id author-pubkey]
  (let [event-id-short (util/format-event-id-short item-id)
        seen-on-relays (store/get-seen-on-relays db item-id)
        ^VBox v-box (first (seq (.getContent singleton-popup)))
        ^Label event-id-label (.lookup v-box ".ndesk-info-popup-event-id")
        ^Label seen-on-label (.lookup v-box ".ndesk-info-popup-seen-on")
        ^Hyperlink copy-event-id-hyperlink (.lookup v-box ".ndesk-info-popup-copy-event-link")
        ^Hyperlink copy-author-pubkey-hyperlink (.lookup v-box ".ndesk-info-popup-copy-author-pubkey-link")]
    ;; Set the content.
    (.setUserData copy-event-id-hyperlink {:event-id item-id})
    (.setUserData copy-author-pubkey-hyperlink {:author-pubkey author-pubkey})
    (.setText event-id-label (str "Event: " event-id-short))
    (.setText seen-on-label (str/join "\n" (cons "Seen on:" seen-on-relays)))
    ;; Set position and dimensions.
    (.relocate v-box
               (- (.getX node-pos) (* 0.5 popup-width))
               (.getY node-pos))
    (.setMinWidth v-box popup-width)
    (.setMaxWidth v-box popup-width)
    (.setPrefWidth v-box popup-width)
    singleton-popup))

(defn show-info!
  [db item-id author-pubkey ^ActionEvent e]
  (let [node (.getSource e)
        popup-width 250
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
  [*state db event-obj root-data item-id pubkey column-id]
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
                      {:fx/type :button
                       :style-class ["button" "ndesk-thread-button"] ;; used for .lookup
                       :h-box/margin 3
                       :text "thread"
                       :on-action (fn [e]
                                    (let [column (and column-id (domain/find-column-by-id column-id))]
                                      #_(log/debugf "Thread button clicked for column %s" (:name (:view column)))
                                      (timeline/show-column-thread! *state column pubkey event-obj)))}])})

(defn- thread-action-button-row
  ;; Like action-button-row but without the 'thread' button.
  [*state db root-data item-id pubkey column-id]
  {:fx/type :h-box
   :style-class ["ndesk-content-controls"] ;; used for .lookup
   :style (BORDER| :lightgrey)
   :visible false
   :alignment :center
   :max-width Integer/MAX_VALUE
   :children [(info-button db item-id pubkey)
              (reply-button *state
                            (if (nil? root-data) item-id (:id root-data))
                            item-id)]})

(defn- author-pane [name pubkey timestamp]
  {:fx/type :border-pane
   :style (BORDER| :white)
   :border-pane/margin (Insets. 0.0 5.0 0.0 5.0)
   :left {:fx/type :h-box
          :cursor :hand
          :style-class "nuestr-author-hbox"
          :on-mouse-clicked (fn [_] (timeline/maybe-add-open-profile-state! pubkey))
          :children [{:fx/type :label
                      :style-class "ndesk-timeline-item-name"
                      :text name}
                     {:fx/type :label
                      :style-class "ndesk-timeline-item-pubkey"
                      :text (or (some-> pubkey util/format-pubkey-short) "?")}]}
   :right {:fx/type :label
           :style-class "ndesk-timeline-item-timestamp"
           :text (or (some-> timestamp util/format-timestamp) "?")}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- load-from-store [db event-id]
  (when-let [event (store/load-event db event-id)]
    (assoc event :relays (store/load-relays-for-event db event-id))))

(defn- async-load-event!
  "Load the event with the given id from either the database or from the relays."
  [*state db column-id pubkey event-id]
  #_(log/debugf "Async load event with column-id=%s and pubkey=%s"
              column-id pubkey)
  (if-let [event-from-store (load-from-store db event-id)]
    (fx/run-later
     (timeline/thread-dispatch! *state
                                (if column-id
                                  (domain/find-column-by-id column-id)
                                  nil)
                                (get (:open-profile-states @*state) pubkey )
                                event-from-store
                                false))
    ;; Create a unique subscription id to load the event and subscribe to all relays in
    ;; the hope that we find the event.  We'll unsubscribe automatically when we get an
    ;; EOSE event.
    (let [profile-state (get (:open-profile-states @*state) pubkey)
          subscription-id (format "%s:%s:%s"
                                  (if column-id "thread" "profile")
                                  (or column-id (:id profile-state))
                                  (rand-int 1000000000))]
      (relay-conn/subscribe-all! subscription-id
                                 [{:ids [event-id] :kinds [1]}]))))

(defn thread-timeline-item
  [{:keys [^UITextNote root-data ^UITextNote item-data column-id pubkey *state db metadata-cache executor]}]
  (if (:missing? item-data)
    {:fx/type :h-box
     :style-class ["ndesk-timeline-item-missing"]
     :children [(do
                  ;; Load the parent.
                  (util/submit! executor ; get off of fx thread
                                (fn []
                                  ;; Wait a bit so we don't overload the relays.
                                  ;; TODO: we should use a queue instead of this black magic!
                                  (Thread/sleep 50) ; 50ms
                                  (async-load-event! *state db column-id pubkey (:id item-data))))
                  ;; Show that we're working on it.
                  {:fx/type :hyperlink
                   :h-box/hgrow :always
                   :style-class ["ndesk-timeline-item-missing-hyperlink"]
                   :text (format "missing %s..." (:id item-data))})]}
    (let [item-id (:id item-data)
          pubkey (:pubkey item-data)
          pubkey-for-avatar (or (some-> pubkey (subs 0 3)) "?")
          timestamp (:timestamp item-data)
          {:keys [name about picture-url nip05-id created-at]} (some->> pubkey (metadata/get* metadata-cache))
          avatar-color (or (some-> pubkey media/color) :lightgray)]
      {:fx/type :border-pane
       :on-mouse-entered (partial show-action-row! *state true)
       :on-mouse-exited (partial show-action-row! *state false)
       :left (media/avatar-or-empty-space picture-url avatar-color pubkey-for-avatar)
       :center {:fx/type :border-pane
                :top (author-pane name pubkey timestamp)
                :bottom {:fx/type :h-box
                         :children [{:fx/type timeline-item-content
                                     :h-box/hgrow :always
                                     :column-id column-id
                                     :pubkey pubkey
                                     :content (:content item-data)
                                     :tags (:tags item-data)
                                     :metadata-cache metadata-cache}]}}
     ;; The action buttons (Reply, Info, ...) are at the bottom of the timeline item.
     :bottom (thread-action-button-row *state db root-data item-id pubkey column-id)})))

(defn- tree-rows*
  [indent ^UITextNote root-data ^UITextNote item-data column-id pubkey *state db metadata-cache executor]
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
                   :column-id column-id
                   :pubkey pubkey
                   :*state *state
                   :db store/db
                   :metadata-cache metadata/cache
                   :executor executor}]}
      (mapcat #(tree-rows* (inc indent) root-data % column-id pubkey *state db metadata-cache executor)
              (:children item-data)))))

(defn- find-note
  [^UITextNote note pred]
  (if (pred note) note (first (map #(find-note % pred) (:children note)))))

(defn- tree* [{:keys [^UITextNoteWrapper note-wrapper column-id pubkey *state db metadata-cache executor]}]
  ;; NOTE: we get nil note-wrapper sometimes when the list-cell is advancing
  ;; in some ways. For now just render label w/ err which we'll see if
  ;; this matters.
  (if (nil? note-wrapper)
    {:fx/type :label :text "err"}
    (let [root (:root note-wrapper)]
      {:fx/type :v-box
       :children (vec (tree-rows* 0 root root column-id pubkey
                                  *state db metadata-cache executor))})))

(defn thread-home [{:keys [column-id pubkey *state db metadata-cache executor]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created #(.setSelectionModel % util-fx-more/no-selection-model)
   :desc {:fx/type :list-view
          :focus-traversable false
          :style-class ["nuestr-thread-pane-listview"]
          :pref-height 100000  ; trick to make it stretch vertically
          :pref-width 100000
          :cell-factory {:fx/cell-type :list-cell
                         :describe (fn [note-wrapper]
                                     {:graphic
                                      {:fx/type tree*
                                       :column-id column-id
                                       :pubkey pubkey
                                       :note-wrapper note-wrapper
                                       :*state *state
                                       :db db
                                       :metadata-cache metadata-cache
                                       :executor executor}})}}})

(defn create-thread-view
  ^ListView [column-id pubkey *state db metadata-cache executor]
  (fx/instance
   (fx/create-component {:fx/type thread-home
                         :column-id column-id
                         :pubkey pubkey
                         :*state *state
                         :db db
                         :metadata-cache metadata-cache
                         :executor executor})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flat timelines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn timeline-item
  [{:keys [root-data ^UITextNoteNew text-note-new column-id *state db metadata-cache executor]}]
  ;; ROOT-DATA is nil for flat view, some kind of event data for threaded view.
  (let [event-obj (:event-obj text-note-new)
        pubkey (:pubkey event-obj)
        pubkey-for-avatar (or (some-> pubkey (subs 0 3)) "?")
        {:keys [name about picture-url nip05-id created-at]} (some->> pubkey (metadata/get* metadata-cache))
        avatar-color (or (some-> pubkey media/color) :lightgray)]
    {:fx/type :border-pane
     :on-mouse-entered (partial show-action-row! *state true)
     :on-mouse-exited (partial show-action-row! *state false)
     :left (media/avatar-or-empty-space picture-url avatar-color pubkey-for-avatar)
     :center {:fx/type :border-pane
              :style (BORDER| :white)
              ;; Show name, pubkey and timestamp.              
              :top (author-pane name pubkey (:created_at event-obj))
              ;; Show the actual content.
              :bottom {:fx/type :h-box
                       :children [{:fx/type timeline-item-content
                                   :column-id column-id
                                   :h-box/hgrow :always
                                   :content (:content event-obj)
                                   :tags (:tags event-obj)
                                   :metadata-cache metadata-cache}]}}
     ;; The action buttons (Reply, Thread, ...) are at the bottom of the timeline item.
     :bottom (action-button-row *state db event-obj root-data (:id event-obj) pubkey column-id)}))


(defn timeline-item*
  [{:keys [column-id root-data ^UITextNoteNew text-note-new relays *state db metadata-cache executor]}]
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
     :executor executor}))

(defn home [{:keys [column-id *state db metadata-cache executor relays]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created #(.setSelectionModel % util-fx-more/no-selection-model)
   :desc {:fx/type :list-view
          :focus-traversable false
          :pref-height 100000  ; trick to make it stretch vertically
          :pref-width 100000
          :cell-factory {:fx/cell-type :list-cell
                         :describe (fn [text-note-new]
                                     {:graphic
                                      {:fx/type timeline-item*
                                       :column-id column-id
                                       :text-note-new text-note-new
                                       :relays relays
                                       :*state *state
                                       :db db
                                       :metadata-cache metadata-cache
                                       :executor executor}})}}})

(defn create-list-view
  ^ListView [column-id *state db metadata-cache executor]
  ;; Note that COLUMN-ID is nil for timeline list views in Profile tabs.
  (fx/instance
   (fx/create-component {:fx/type home
                         :column-id column-id
                         :*state *state
                         :db db
                         :metadata-cache metadata-cache
                         :executor executor})))


