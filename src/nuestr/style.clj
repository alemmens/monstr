(ns nuestr.style
  (:require [clojure.java.io :as io]))

(defn style-mode?
  []
  (= "true" (System/getProperty "cljfx.style.mode")))

(defn css
  []
  [(cond-> (str (io/resource "nuestr/style.css"))
     (style-mode?) (str "?q=" (rand-int 1000000)))])

(defn BORDER|
  "eg,
    :style (border| {...})

    :style (border| :green {...})

    :style (border|)

    :style (border| 10)

    :style (border| :red 10)
    "
  [& more]
  (let [color (or (first (filter keyword? more)) :red)
        width (or (first (filter number? more)) 1)
        existing (or (first (filter map? more)) {})]
    (merge
      existing
      {:-fx-border-color color
       :-fx-border-width width})))
