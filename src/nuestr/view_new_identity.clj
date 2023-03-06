(ns nuestr.view-new-identity
  (:require [nuestr.style :as style]
            [cljfx.api :as fx]
            [clojure.tools.logging :as log])
  (:import (javafx.scene.control ButtonType Dialog ButtonBar$ButtonData DialogPane DialogEvent TextField CheckBox)
           (javafx.util Callback)))

(defn clear-fields!
  [^Dialog dialog]
  (let [pane (.getDialogPane dialog)]
    (.setText ^TextField
      (.lookup pane ".ndesk-new-identity-text-field") "")
    (.setSelected ^CheckBox
      (.lookup pane ".ndesk-new-identity-check-box") false)))

(defn- result-converter
  [^ButtonType r ^Dialog dialog]
  (condp = (.getButtonData r)
    ButtonBar$ButtonData/CANCEL_CLOSE :cancel
    ButtonBar$ButtonData/OK_DONE
    (let [^DialogPane pane (.getDialogPane dialog)
          ^TextField tf (.lookup pane ".ndesk-new-identity-text-field")
          ^CheckBox cb (.lookup pane ".ndesk-new-identity-check-box")]
      {:val (.getText tf) :public? (.isSelected cb)})))

(defn- on-create-dialog
  [^Dialog d]
  (doto d
    ;; note we have to set result-converter this way instead of via
    ;; cljfx prop as we need ref to the dialog to do the conversion
    (.setResultConverter
      (reify Callback
        (call [_ r]
          (result-converter r d))))))

(defn dialog
  [{:keys [show-new-identity? new-identity-error]}]
  (log/debugf "Creating new identity dialog with show-new-identity=%s" show-new-identity?)
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created on-create-dialog
   :desc
   {:fx/type :dialog
    :showing show-new-identity?
    :title "New Identity"
    :dialog-pane
    {:fx/type :dialog-pane
     :stylesheets (style/css)
     :button-types [(ButtonType. "Cancel" ButtonBar$ButtonData/CANCEL_CLOSE)
                    (ButtonType. "Save" ButtonBar$ButtonData/OK_DONE)]
     :content
     {:fx/type :v-box
      :children
      [{:fx/type :h-box
        :alignment :center-left
        :children
        [{:fx/type :text-field
          :style {:-fx-font-family :monospace}
          :style-class ["text-field" "text-input" "ndesk-new-identity-text-field"]
          :prompt-text "64 character key"
          :pref-column-count 64}
         {:fx/type :check-box
          :style {:-fx-padding [0 5]}
          :style-class ["check-box" "ndesk-new-identity-check-box"]}
         {:fx/type :label
          :text "This is a public key."}]}
       {:fx/type :label
        :style {:-fx-text-fill :red}
        :text new-identity-error}]}}
    :on-close-request {:event/type :new-identity-close-request}
    :on-hidden (fn [dialog-event]
                 (-> dialog-event .getSource clear-fields!))}})
