(ns me.untethr.nostr.store
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [me.untethr.nostr.domain :as domain]
            [me.untethr.nostr.json :as json]
            [me.untethr.nostr.parse :as parse]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def get-datasource*
  (memoize
    #(jdbc/get-datasource (str "jdbc:sqlite:" %))))

(defn- comment-line?
  [line]
  (str/starts-with? line "--"))

(defn parse-schema []
  (let [resource (io/resource "me/untethr/nostr/schema.sql")]
    (with-open [reader (io/reader resource)]
      (loop [lines (line-seq reader) acc []]
        (if (next lines)
          (let [[ddl more] (split-with (complement comment-line?) lines)]
            (if (not-empty ddl)
              (recur more (conj acc (str/join "\n" ddl)))
              (recur (drop-while comment-line? lines) acc)))
          acc)))))

(defn apply-schema! [db]
  (doseq [statement (parse-schema)]
    (jdbc/execute-one! db [statement])))

(defn init!
  [path]
  (doto (get-datasource* path)
    apply-schema!))

;; --

(defn- insert-event!*
  [db id pubkey created-at kind content raw-event-tuple]
  {:post [(or (nil? %) (contains? % :rowid))]}
  (jdbc/execute-one! db
    [(str
       "insert or ignore into n_events"
       " (id, pubkey, created_at, kind, content_, raw_event_tuple)"
       " values (?, ?, ?, ?, ?, ?) returning rowid")
     id pubkey created-at kind content raw-event-tuple]
    {:builder-fn rs/as-unqualified-lower-maps}))

(defn insert-event!
  "Answers inserted sqlite rowid or nil if row already exists."
  [db id pubkey created-at kind content raw-event-tuple]
  (:rowid (insert-event!* db id pubkey created-at kind content raw-event-tuple)))

(defn insert-e-tag!
  [db source-event-id tagged-event-id]
  (jdbc/execute-one! db
    [(str
       "insert or ignore into e_tags"
       " (source_event_id, tagged_event_id)"
       " values (?, ?)")
     source-event-id tagged-event-id]))

(defn insert-p-tag!
  [db source-event-id tagged-pubkey]
  (jdbc/execute-one! db
    [(str
       "insert or ignore into p_tags"
       " (source_event_id, tagged_pubkey)"
       " values (?, ?)")
     source-event-id tagged-pubkey]))

(defn- raw-event-tuple->event-obj
  [raw-event-tuple]
  (-> raw-event-tuple json/parse (nth 2)))

;; todo load which relay/s per event, too?
(defn load-timeline-events
  [db pubkeys]
  (when-not (empty? pubkeys)
    (mapv (comp raw-event-tuple->event-obj :raw_event_tuple)
      (jdbc/execute! db
        (vec
          (concat
            [(format
               (str
                 "select raw_event_tuple from n_events where id in ("
                 (str
                   "select distinct e.id from n_events e"
                   " left join p_tags p on e.id = p.source_event_id"
                   " where (e.pubkey in (%s) or p.tagged_pubkey in (%s)) and kind = 1")
                 ")")
               (str/join "," (repeat (count pubkeys) "?"))
               (str/join "," (repeat (count pubkeys) "?")))]
            pubkeys
            pubkeys))
        {:builder-fn rs/as-unqualified-lower-maps}))))

;; --

(defn load-relays
  [db]
  (mapv
    (fn [{:keys [url read_ write_]}]
      (domain/->Relay url (pos? read_) (pos? write_)))
    (jdbc/execute! db ["select * from relays_"]
      {:builder-fn rs/as-unqualified-lower-maps})))

(defn load-identities
  [db]
  (mapv
    (fn [{:keys [public_key secret_key]}]
      (domain/->Identity public_key secret_key))
    (jdbc/execute! db ["select * from identities_"]
      {:builder-fn rs/as-unqualified-lower-maps})))

(defn- raw-event-tuple->parsed-metadata
  [raw-event-tuple]
  (let []
    (let [{:keys [created_at content] :as _event-obj} (parse/raw-event-tuple->event-obj raw-event-tuple)
          {:keys [name about picture nip05]} (json/parse content)]
      (domain/->ParsedMetadata name about picture nip05 created_at))))

;; todo make efficient via delete by trigger or gc process
(defn load-metadata
  [db pubkeys]
  (into
    {}
    (map (juxt :pubkey #(-> % :raw_event_tuple raw-event-tuple->parsed-metadata)))
    (jdbc/execute! db
      (vec
        (concat
          [(format
             (str "select pubkey, raw_event_tuple, max(created_at) as max_ from n_events"
               " where pubkey in (%s) and kind = 0 and deleted_ is false"
               " group by pubkey")
             (str/join "," (repeat (count pubkeys) "?")))]
          pubkeys))
      {:builder-fn rs/as-unqualified-lower-maps})))

(defn- raw-event-tuple->parsed-contact-list
  [raw-event-tuple]
  (let [{:keys [pubkey created_at] :as event-obj} (-> raw-event-tuple json/parse (nth 2))]
    (domain/->ContactList
      pubkey
      created_at
      (parse/parse-contacts* event-obj))))

;; todo make efficient via delete by trigger or gc process
(defn load-contact-lists
  "Answers {<pubkey> ContactList}."
  [db identities]
  (let [public-keys (mapv :public-key identities)]
    (into
      {}
      (map (juxt :pubkey #(-> % :raw_event_tuple raw-event-tuple->parsed-contact-list)))
      (jdbc/execute! db
        (vec
          (concat
            [(format
               (str "select pubkey, raw_event_tuple, max(created_at) as max_ from n_events"
                 " where pubkey in (%s) and kind = 3 and deleted_ is false"
                 " group by pubkey")
               (str/join "," (repeat (count public-keys) "?")))]
            public-keys))
        {:builder-fn rs/as-unqualified-lower-maps}))))

(defn replace-relays!
  "Answers provided relays on success."
  [db relays]
  (jdbc/with-transaction [tx db]
    (jdbc/execute! tx ["delete from relays_"])
    (jdbc/execute-batch! tx
      "insert into relays_ (url,read_,write_) values (?,?,?)"
      (mapv (fn [{:keys [url read? write?]}] [url read? write?]) relays)
      {}))
  relays)

(defn contains-event-from-relay!
  [db relay-url event-id]
  (jdbc/execute-one! db
    ["insert or ignore into relay_event_id (relay_url, event_id) values (?,?)"
     relay-url event-id]))

(defn contains-event-from-relay?
  [db relay-url event-id]
  (pos?
    (:exists_
      (jdbc/execute-one! db
        ["select exists(select 1 from relay_event_id where event_id = ? and relay_url = ?) as exists_"
         event-id relay-url]
        {:builder-fn rs/as-unqualified-lower-maps}))))

(defn event-signature-by-id
  [db event-id]
  (:signature_
    (jdbc/execute-one! db
      ["select signature_ from signature_event_id where event_id = ?" event-id]
      {:builder-fn rs/as-unqualified-lower-maps})))

(defn event-signature!
  [db event-id sig]
  (jdbc/execute-one! db
    ["insert or ignore into signature_event_id (event_id, signature_) values (?,?)"
     event-id sig]))

(defn delete-identity!
  [db {:keys [public-key]}]
  (jdbc/execute-one! db
    ["delete from identities_ where public_key = ?" public-key]))
