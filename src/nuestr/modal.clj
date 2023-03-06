(ns nuestr.modal
  (:import (javafx.scene.control Alert Alert$AlertType ButtonType Button ButtonBar$ButtonData)))

(defn blocking-yes-no-alert
  "Answers true iff yes selected."
  [header-text content-text
   & {:keys [ok-text cancel-text style-class]
      :or {ok-text "Yes" cancel-text "No"}}]
  (let [alert (Alert. Alert$AlertType/CONFIRMATION) ;; this confirmation type offers the ok and cancel buttons
        ^Button ok-button (-> alert .getDialogPane (.lookupButton ButtonType/OK))
        ^Button no-button (-> alert .getDialogPane (.lookupButton ButtonType/CANCEL))]
    (.setHeaderText alert header-text)
    (.setContentText alert content-text)
    (.setText ok-button ok-text)
    (.setText no-button cancel-text)
    (when style-class
      (doto (.getStyleClass (.getDialogPane alert))
        (.remove "confirmation")
        (.add style-class)))
    (some-> alert
      .showAndWait
      ^ButtonType (.orElse nil)
      .getButtonData
      (= ButtonBar$ButtonData/OK_DONE))))
