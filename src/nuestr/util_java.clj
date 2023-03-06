(ns monstr.util-java
  (:import (javafx.event EventHandler)
           (java.util.function Function BiFunction BiConsumer Predicate)
           (javafx.beans.value ChangeListener)
           (javafx.util Callback)))

(defn ->BiConsumer
  ^BiConsumer [f]
  (reify BiConsumer
    (accept [_ t u]
      (f t u))))

(defn ->BiFunction
  ^BiFunction [f]
  (reify BiFunction
    (apply [_ t u]
      (f t u))))

(defn ->Function
  ^Function [f]
  (reify Function
    (apply [_ t]
      (f t))))

(defn ->Predicate
  ^Predicate [f]
  (reify Predicate
    (test [_ t]
      (f t))))

;; javafx

(defn ->Callback
  ^Callback [f]
  (reify Callback
    (call [_ param]
      (f param))))

(defn ->EventHandler
  ^EventHandler [f]
  (reify EventHandler
    (handle [_ e]
      (f e))))

(defn ->ChangeListener
  ^ChangeListener [f]
  (reify ChangeListener
    (changed [_ _observable _old-value new-value]
      (f new-value))))

(defn ->ChangeListener*
  ^ChangeListener [f]
  (reify ChangeListener
    (changed [_ observable _old-value new-value]
      (f observable new-value))))
