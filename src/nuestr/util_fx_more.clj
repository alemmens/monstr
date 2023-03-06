(ns monstr.util-fx-more
  (:import (javafx.scene.control MultipleSelectionModel)
           (javafx.collections FXCollections)))

;; @see https://stackoverflow.com/questions/20621752/javafx-make-listview-not-selectable-via-mouse
(def ^MultipleSelectionModel no-selection-model
  (let [empty-list (FXCollections/emptyObservableList)]
    (proxy [MultipleSelectionModel] []
      (getSelectedIndices [] empty-list)
      (getSelectedItems [] empty-list)
      (selectIndices [& _])
      (selectAll [])
      (selectFirst [])
      (selectLast [])
      (clearAndSelect [idx])
      (select [_])
      (clearSelection
        ([])
        ([idx]))
      (isSelected [idx] false)
      (isEmpty [] true)
      (selectPrevious [])
      (selectNext []))))
