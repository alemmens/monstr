(ns nuestr.view-relays
  (:require
    [cljfx.api :as fx]
    [clojure.tools.logging :as log]
    [nuestr.style :as style]
    [nuestr.domain :as domain]
    [nuestr.style :as style :refer [BORDER|]]
    [nuestr.store :as store]
    [clojure.string :as str])
  (:import
    (javafx.scene.control ButtonType ButtonBar$ButtonData DialogEvent Dialog DialogPane TableView TextField CheckBox)
    (javafx.util Callback)
    (javafx.scene.layout VBox HBox)
    (java.util UUID)))

(def dialog-width 800)
(def dialog-height 600)

(defn- result-converter
  [^ButtonType r ^Dialog d]
  (condp = (.getButtonData r)
    ButtonBar$ButtonData/CANCEL_CLOSE :cancel
    ButtonBar$ButtonData/LEFT :restore
    ButtonBar$ButtonData/OK_DONE
    (let [^DialogPane pane (.getDialogPane d)
          ^VBox v-box (last (.getChildren pane))]
      (reduce
        (fn [acc ^HBox h-box]
          (let [[^TextField _eventcount ^TextField tf _ ^CheckBox cb0 _ ^CheckBox cb1]
                (vec (.getChildren h-box))
                url (str/trim (.getText tf))]
            (if (str/blank? url)
              acc
              (conj acc (domain/->Relay url (.isSelected cb0) (.isSelected cb1))))))
        []
        (.getChildren v-box)))))

(defn- on-create-dialog
  [^Dialog d]
  (doto d
    ;; note we have to set result-converter this way instead of via
    ;; cljfx prop as we need ref to the dialog to do the conversion
    (.setResultConverter
      (reify Callback
        (call [_ r]
          (result-converter r d))))))

(defn row
  [{{:keys [url read? write? event-count]} :relay}]
  ;; result converter is very sensitive to our layout here so if this
  ;; changes that needs to, as well.
  {:fx/type :h-box
   :children
   [{:fx/type :text-field
     :disable true
     :style {:-fx-pref-width "6em"
             :-fx-alignment "center_right"}
     :text (str event-count)}
    {:fx/type :text-field
     :h-box/hgrow :always
     :text url}
    {:fx/type :label
     :text "read?"
     :style {:-fx-padding 3}}
    {:fx/type :check-box
     :selected read?
     :style {:-fx-padding 3}}
    {:fx/type :label
     :text "write?"
     :style {:-fx-padding 3}}
    {:fx/type :check-box
     :selected write?
     :style {:-fx-padding 3}}]})

;; special impl here using refresh-relays-ts, which allows upstream
;; to force an entire re-render of the content; as user edits fields, we
;; do NOT bring in cljfx render participation; we just let user mutate
;; controls knowing that no state updates will re-render over them; when
;; user dismisses modal we walk the text fields produce the result (see
;; result-converter above) and go from there.
(defn content
  [{:keys [relays refresh-relays-ts]}]
  (let [event-counts (store/count-events-on-relays store/db)]
    (log/debugf "Event counts %s" event-counts)
    {:fx/type :v-box
     :children (map-indexed
                #(identity
                   {:fx/type row
                    :fx/key (str %1 ":" refresh-relays-ts)
                    :relay (assoc %2 :event-count (get event-counts (:url %2)))})
                ;; Add 10 more empty rows.
                (concat relays
                        (repeatedly 10 #(domain/->Relay "" true true))))}))

;; 

(defn dialog
  [{:keys [show-relays? relays refresh-relays-ts]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created on-create-dialog
   :desc
   {:fx/type :dialog
    :showing show-relays?
    :on-close-request {:event/type :relays-close-request}
    :title "Relays"
    :dialog-pane
    {:fx/type :dialog-pane
     :stylesheets (style/css)
     :min-width dialog-width
     :min-height dialog-height
     :button-types [;; note: event handling is coupled to /LEFT here.
                    (ButtonType. "Restore Defaults (replaces current relays)" ButtonBar$ButtonData/LEFT)
                    (ButtonType. "Close (discard changes)" ButtonBar$ButtonData/CANCEL_CLOSE)
                    (ButtonType. "Save" ButtonBar$ButtonData/OK_DONE)]
     :content
     {:fx/type content
      :relays relays
      :refresh-relays-ts refresh-relays-ts}}}})
