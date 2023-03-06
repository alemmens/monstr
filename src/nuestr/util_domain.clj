(ns monstr.util-domain
  (:require
   [clojure.tools.logging :as log]))

(defn ->secret-key*
  [active-key identities]
  (let [x (group-by :public-key identities)]
    (some-> x (get active-key) first :secret-key)))

(defn can-publish?
  [active-key identities]
  (some? (->secret-key* active-key identities)))
