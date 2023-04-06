(ns nuestr.store
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [nuestr.file-sys :as file-sys]            
            [nuestr.domain :as domain]
            [nuestr.json :as json]
            [nuestr.parse :as parse]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def get-datasource*
  (memoize
    #(jdbc/get-datasource (str "jdbc:sqlite:" %))))

(defn- comment-line?
  [line]
  (str/starts-with? line "--"))

(defn parse-schema []
  (let [resource (io/resource "nuestr/schema.sql")]
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

(defn- try-sql
  "Returns true if no SQL error, otherwise false."
  [db query]
  (try (do (jdbc/execute-one! db [query])
           true)
       (catch org.sqlite.SQLiteException e
         false)))

(defn column-exists? [db table column]
  (try-sql db
           (format "select %s from %s limit 1" column table)))

(defn maybe-add-column! [db table column type]
  (when-not (column-exists? db table column)
    (let [query (format "alter table %s add column %s %s"
                        table column type)]
      (jdbc/execute-one! db [query]))))

(defn migrate! [db]
  ;; 2023-03-29
  ;; Marker is "root" or "reply" or "mention" or empty (NIP-10).
  (maybe-add-column! db "e_tags" "relay_url" "varchar(64)")
  (maybe-add-column! db "e_tags" "marker" "varchar(8)")
  (maybe-add-column! db "p_tags" "relay_url" "varchar(64)"))

(defn init!
  [path]
  (doto (get-datasource* path)
    apply-schema!
    migrate!))

(defonce db (init! (file-sys/db-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inserting and deleting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  [db source-event-id tagged-event-id relay-url marker]
  (jdbc/execute-one! db
    [(str
       "insert or ignore into e_tags"
       " (source_event_id, tagged_event_id, relay_url, marker)"
       " values (?, ?, ?, ?)")
     source-event-id tagged-event-id relay-url marker]))

(defn insert-p-tag!
  [db source-event-id tagged-pubkey relay-url]
  (jdbc/execute-one! db
    [(str
       "insert or ignore into p_tags"
       " (source_event_id, tagged_pubkey, relay_url)"
       " values (?, ?, ?)")
     source-event-id tagged-pubkey relay-url]))

(defn event-signature!
  [db event-id sig]
  (jdbc/execute-one! db
    ["insert or ignore into signature_event_id (event_id, signature_) values (?,?)"
     event-id sig]))

(defn insert-identity!
  [db public-key secret-key]
  ;; secret-key could be nil
  (jdbc/execute-one! db
    [(str "insert into identities_ (public_key, secret_key) values (?,?)"
       " on conflict(public_key) do update set secret_key=?"
       " where excluded.secret_key is not null")
     public-key secret-key secret-key]))

(defn delete-identity!
  [db public-key]
  (log/debugf "Deleting account %s" public-key)
  (jdbc/execute-one! db
    ["delete from identities_ where public_key = ?" public-key]))

(defn insert-channel!
  [db channel]
  #_(log/debugf "Inserting channel %s" (:name channel))
  (let [{:keys [id pubkey name about picture-url recommended-relay-url]} channel
        query (str "insert or ignore into channels"
                   " (id, pubkey, name, about, picture_url, relay_url) "
                   " values (?, ?, ?, ?, ?, ?)")]
    (jdbc/execute-one! db
                       [query
                        id pubkey name about picture-url recommended-relay-url])))

(defn update-channel!
  [db channel]
  #_(log/debugf "Updating channel %s" (:name channel))
  (let [{:keys [id pubkey name about picture-url recommended-relay-url]} channel
        query (str "update channels"
                   " set pubkey=?, name=?, about=?, picture_url=?, relay_url=? "
                   " where id=?")]
    (jdbc/execute-one! db
                       [query
                        pubkey name about picture-url recommended-relay-url id])))

(defn delete-all-channels! [db]
  (jdbc/execute-one! db
                     ["delete from channels"]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn event-signature-by-id
  [db event-id]
  (:signature_
    (jdbc/execute-one! db
      ["select signature_ from signature_event_id where event_id = ?" event-id]
      {:builder-fn rs/as-unqualified-lower-maps})))

(defn- raw-event-tuple->event-obj
  [raw-event-tuple]
  (-> raw-event-tuple json/parse (nth 2)))

(defn load-event
  [db event-id]
  (some->
    (jdbc/execute-one! db ["select e.raw_event_tuple from n_events e where e.id = ?" event-id]
      {:builder-fn rs/as-unqualified-lower-maps})
    :raw_event_tuple
    raw-event-tuple->event-obj))


(defn timeline-query
  [pubkeys]
  [(format (str "select raw_event_tuple from n_events"
                " where pubkey in (%s) and kind = 1"
                " order by created_at"
                " limit 100")
               (str/join ", " (map #(str "'" % "'") pubkeys)))])

(defn load-timeline-events
  [db pubkeys]
  (log/debugf "Loading timeline events for %d pubkeys" (count pubkeys))
  (when-not (empty? pubkeys)
    (mapv (comp raw-event-tuple->event-obj :raw_event_tuple)
          (jdbc/execute! db
                         (timeline-query pubkeys)
                         {:builder-fn rs/as-unqualified-lower-maps}))))


(defn relay-events-query
  [relay-url pubkeys]
  [(format (str "select raw_event_tuple from n_events e"
                " inner join relay_event_id r on e.id=r.event_id"
                " where r.relay_url='" relay-url "'"
                (when pubkeys
                  " and e.pubkey in (%s) ")
                " and e.kind = 1"
                " order by e.created_at desc"
                " limit 200")
               (str/join ", " (map #(str "'" % "'") pubkeys)))])

(defn load-relay-events
  [db relay-url pubkeys]
  (log/debugf "Loading timeline events for %s with %d pubkeys" relay-url (count pubkeys))
  (when-not (empty? pubkeys)
    (map #(assoc % :relays (list relay-url))
         (mapv (comp raw-event-tuple->event-obj :raw_event_tuple)
               (jdbc/execute! db
                              (relay-events-query relay-url pubkeys)
                              {:builder-fn rs/as-unqualified-lower-maps})))))

(defn load-events-since
  "Loads (at most 10000) events since the given timestamp."
  [db since]
  (mapv (comp raw-event-tuple->event-obj :raw_event_tuple)
        (jdbc/execute! db
                       [(format "select raw_event_tuple from n_events where created_at >= %d limit 1000"
                                since)]
                       {:builder-fn rs/as-unqualified-lower-maps})))

(defn load-events-with-etag
  [db etag-id]
  (mapv (comp raw-event-tuple->event-obj :raw_event_tuple)
        (jdbc/execute! db
                       [(str "select raw_event_tuple from n_events e"
                                     " inner join e_tags t on t.source_event_id=e.id"
                                     " where t.tagged_event_id='" etag-id "'"
                                     " limit 1000")]
                       {:builder-fn rs/as-unqualified-lower-maps})))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading everything else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn load-identities
  [db]
  (mapv
    (fn [{:keys [public_key secret_key]}]
      (domain/->Identity public_key secret_key))
    (jdbc/execute! db ["select * from identities_"]
      {:builder-fn rs/as-unqualified-lower-maps})))

(defn load-channels [db]
  (mapv (fn [{:keys [id pubkey name about picture_url relay_url]}]
          (domain/->Channel id pubkey name about picture_url relay_url))
        (jdbc/execute! db ["select * from channels"]
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
  [db pubkeys]
  (log/debugf "Loading contact lists for %d pubkeys." (count pubkeys))
  (into
   {}
   (map (juxt :pubkey #(-> % :raw_event_tuple raw-event-tuple->parsed-contact-list)))
   (jdbc/execute! db
                  (vec
                   (concat [(format (str "select pubkey, raw_event_tuple, max(created_at) as max_ from n_events"
                                         " where pubkey in (%s) and kind = 3 and deleted_ is false"
                                         " group by pubkey")
                                    (str/join "," (repeat (count pubkeys) "?")))]
                           pubkeys))
                  {:builder-fn rs/as-unqualified-lower-maps})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-relays
  [db]
  (mapv (fn [{:keys [url read_ write_]}]
          (let [read? (pos? read_)
                write? (pos? write_)
                ;; We use all relays for metadata.
                meta? true]
            (domain/->Relay url read? write? meta?)))
        (jdbc/execute! db ["select * from relays_"]
                       {:builder-fn rs/as-unqualified-lower-maps})))

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


(defn delete-all-relays! [db]
  (jdbc/execute-one! db
                     ["delete from relays_"]))

(defn delete-non-active-relays! [db]
  (jdbc/execute-one! db
                     ["delete from relays_ where read_ is 0 and write_ is 0"]))
  
(defn insert-relay! [db relay]
  (let [{:keys [url read? write?]} relay]
    (jdbc/execute-one! db
                       [(str
                         "insert or ignore into relays_ (url,read_,write_)"
                         " values (?, ?, ?)")
                        url read? write?])))

(defn update-relay! [db relay]
  (let [{:keys [url read? write?]} relay]
    (jdbc/execute-one! db
                       [(str "update relays_ set read_=?, write_=?"
                             " where url=?")
                        read? write? url])))
  

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

(defn get-seen-on-relays
  [db event-id]
  (vec
    (sort
      (map
        :relay_url
        (jdbc/execute! db
          ["select relay_url from relay_event_id where event_id = ?" event-id]
          {:builder-fn rs/as-unqualified-lower-maps})))))

(defn count-events-on-relays
  "Returns a map from relay urls to event counts."  
  [db]
  (let [result (jdbc/execute! db
                              ["select relay_url, count(*) from relay_event_id group by relay_url"])]
    (zipmap (map :relay_event_id/relay_url result)
            (map (keyword "count(*)") result))))
             


(defn load-relays-for-event
  "Returns a list of relay urls."
  [db event-id]
  (map :relay_url
       (jdbc/execute! db
                      [(str "select relay_url from relay_event_id where event_id='" event-id "'")]
                      {:builder-fn rs/as-unqualified-lower-maps})))

