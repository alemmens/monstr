(ns monstr.view-reply
  (:require [monstr.style :as style]
            [cljfx.api :as fx]
            [clojure.tools.logging :as log]
            [monstr.view-common :as view-common]
            [monstr.util-java :as util-java])
  (:import (javafx.scene.control ButtonType Dialog ButtonBar$ButtonData DialogPane DialogEvent TextField CheckBox TextArea)
           (javafx.util Callback)))

(defn clear-fields!
  [^Dialog dialog]
  (let [pane (.getDialogPane dialog)]
    (.clear ^TextArea (.lookup pane ".ndesk-reply-box"))))

(defn- result-converter
  [^ButtonType r ^Dialog dialog]
  (condp = (.getButtonData r)
    ButtonBar$ButtonData/CANCEL_CLOSE :cancel
    ButtonBar$ButtonData/OK_DONE
    (let [^DialogPane pane (.getDialogPane dialog)
          ^TextArea ta (.lookup pane ".ndesk-reply-box")]
      {:content (.getText ta) :success-callback (fn [& _] (clear-fields! dialog))})))

(defn- on-create-dialog
  [^Dialog d]
  (doto d
    ;; note we have to set result-converter this way instead of via
    ;; cljfx prop as we need ref to the dialog to do the conversion
    (.setResultConverter
      (util-java/->Callback
        #(result-converter % d)))
    (.setOnShown
      ;; @see https://github.com/cljfx/cljfx/issues/138
      (util-java/->EventHandler
        (fn [^DialogEvent dialog-event]
          (let [^Dialog dialog (.getSource dialog-event)
                ^DialogPane pane (.getDialogPane dialog)
                ^TextArea ta (.lookup pane ".ndesk-reply-box")]
            (.requestFocus ta)))))))

(defn reply-box
  [_]
  {:fx/type :text-area
   :style-class ["text-area" "ndesk-reply-box"] ;; used for .lookup
   :prompt-text "Your reply..."
   :wrap-text true
   :text-formatter {:fx/type :text-formatter
                    :value-converter :default
                    :filter view-common/text-formatter-filter*}})

(defn dialog
  [{:keys [active-reply-context]}]
  {:fx/type fx/ext-on-instance-lifecycle
   :on-created on-create-dialog
   :desc
   {:fx/type :dialog
    :showing (some? active-reply-context)
    :title "Reply"
    :dialog-pane
    {:fx/type :dialog-pane
     :stylesheets (style/css)
     :button-types [(ButtonType. "Cancel" ButtonBar$ButtonData/CANCEL_CLOSE)
                    (ButtonType. "Reply" ButtonBar$ButtonData/OK_DONE)]
     :content
     {:fx/type :v-box
      :children [{:fx/type reply-box}]}}
    :on-close-request {:event/type :reply-close-request}
    :on-hidden (fn [dialog-event]
                 ;; for some cases we may set showing=false ourselves and
                 ;; otherwise consume the dialog event w/in on-close-request
                 ;; which means those paths will need to clear-fields themselves
                 (-> dialog-event .getSource clear-fields!))}})
