(ns monstr.util-fx
  (:require [monstr.util-java :as util-java])
  (:import (javafx.scene Parent Node)))

(defn add-style-class!
  ^Node [^Node n s]
  (.add (.getStyleClass n) s)
  n)

(defn add-stylesheet!
  ^Parent [^Parent n s]
  (.add (.getStylesheets n) s)
  n)

(defn on-mouse-clicked!
  ^Node [^Node n f]
  (.setOnMouseClicked n (util-java/->EventHandler f))
  n)
