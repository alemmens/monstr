(ns nuestr.view-common
  (:import (javafx.scene.control TextFormatter$Change)))

;; defonce so we don't replace it on ns reload (cljfx fails if we try to)
(defonce text-formatter-filter*
  (fn [^TextFormatter$Change change]
    ;; nil rejects the change
    (let [new-text (-> change .getControlNewText)
          valid? (< (count new-text) 280)]
      (when valid?
        change))))
