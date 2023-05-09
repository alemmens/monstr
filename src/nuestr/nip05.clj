(ns nuestr.nip05
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log])
  (:use clojure.test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord User
    [pubkey
     nip05-name  ; either nil or a full nip05 name like 'bob@example.com'.
     preferred-relays ; seq of relays
     created-at ; integer
     verified?
     ])

(defn domain [user]
  (when-let [n (:nip05-name user)]
    (second (str/split n #"@"))))

(defn pretty-name [user]
  (when-let [n (:nip05-name user)]
    (if (str/starts-with? n "_@")
      (subs n (count "_@"))
      n)))

(defn short-name [user]
  (when-let [n (:nip05-name user)]
    (if (str/starts-with? n "_@")
      (subs n (count "_@"))
      (or (first (str/split n #"@"))
          n))))
  
(def users-by-pubkey
  "Map from pubkeys to user records."
  (atom {}))

(def users-by-nip05
  "Sorted map from nip05 names, e.g. 'bob@example.com', to user records."
  (atom (sorted-map)))

(defn find-by-pubkey [pubkey]
  (get @users-by-pubkey pubkey))

(defn find-by-name [name]
  (get @users-by-nip05 name))

(defn maybe-add-or-update! [user]
  (let [do-it! #(do (swap! users-by-pubkey
                           assoc (:pubkey user) user)
                    (when (:nip05-name user)
                      (swap! users-by-nip05
                             assoc (:nip05-name user) user)))]
    (if-let [existing (find-by-pubkey (:pubkey user))]
      (when (<= (:created-at existing) (:created-at user))
        (do-it!))
      (do-it!))))

(defn with-prefix [prefix]
  "Returns a sorted sequence of users whose names start with the given prefix."
  (map second
       (take-while #(str/starts-with? (first %) prefix)
                   (drop-while #(not (str/starts-with? (first %) prefix))
                               (seq @users-by-nip05)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-users
  (let [users [{:pubkey "1234"
                :nip05-name "bob@example.com"}
               {:pubkey "deaf"
                :nip05-name "_@testing.com"}
               {:pubkey "5678"
                :nip05-name "bobbie@example.com"}
               {:pubkey "abcd"
                :nip05-name "charles@example.com"}
               {:pubkey "efgh"
                :nip05-name "arthur@test.eu"}]]
    ;; Names and domains.
    (is (= (domain (first users)) "example.com"))
    (is (= (pretty-name (first users)) "bob@example.com"))
    (is (= (short-name (first users)) "bob"))
    (is (= (pretty-name (second users)) "testing.com"))
    (is (= (short-name (second users)) "testing.com"))
    ;; Adding, finding, listing.
    (run! maybe-add-or-update! users)
    (is (= (find-by-pubkey "5678")
           {:pubkey "5678"
            :nip05-name "bobbie@example.com"}))
    (is (= (find-by-name "bobbie@example.com")
           {:pubkey "5678"
            :nip05-name "bobbie@example.com"}))
    (is (= (seq (with-prefix "bob"))
           (seq [{:pubkey "1234"
                  :nip05-name "bob@example.com"}
                 {:pubkey "5678"
                  :nip05-name "bobbie@example.com"}])))))

                           
  
