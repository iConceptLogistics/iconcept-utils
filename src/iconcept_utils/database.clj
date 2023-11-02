(ns iconcept-utils.database
  (:require [clojure.string :as s]
            [taoensso.timbre :as log]
            ;;
            [next.jdbc            :as jdbc]
            [next.jdbc.sql        :as sql]
            [next.jdbc.prepare    :as p]
            [next.jdbc.result-set :as rs]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [oberon.utils :refer [->kebab-case-keyword ->snake-case-string ->screaming-snake-case-string]]
            [spectacular.core :as sp])
  (:import [org.postgresql.util PGobject]
           [java.time LocalDate Instant]
           [java.sql
            Date Timestamp
            Array
            Clob
            PreparedStatement
            ResultSet ResultSetMetaData
            Statement
            SQLException]
           [java.time LocalDate Instant]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp (atom nil))
(defn-   ^:private set-tmp!
  [value]
  (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------

;;; FIXME: Need to add set-from-java-util-time and read-as-java-util-time.

(defn sql-dates<-java-time
  []
  (extend-protocol p/SettableParameter
    LocalDate (set-parameter [^java.time.LocalDate v ^PreparedStatement s ^long i] (.setDate      s i (Date/valueOf   v)))
    Instant   (set-parameter [^java.time.Instant   v ^PreparedStatement s ^long i] (.setTimestamp s i (Timestamp/from v)))

    ;; I want usage of java.util.Date to cause an error, we shouldn't
    ;; have any of these in our code from now on.
    ;;
    ;; java.util.Date
    ;; (set-parameter [^java.util.Date v ^PreparedStatement s ^long i]
    ;;   (.setTimestamp s i (Timestamp/from (.toInstant v))))

    ;; Avoid unnecessary conversions
    java.sql.Date      (set-parameter [^java.sql.Date      v ^PreparedStatement s ^long i] (.setDate      s i v))
    java.sql.Timestamp (set-parameter [^java.sql.Timestamp v ^PreparedStatement s ^long i] (.setTimestamp s i v))))

(defn sql-dates->java-time
  []
  (extend-protocol rs/ReadableColumn
    Array
    (read-column-by-label [^Array v _]        (vec (.getArray v)))
    (read-column-by-index [^Array v rsmeta _] (vec (.getArray v)))
    ;;
    Date
    (read-column-by-label [^String v _]          (.toLocalDate v))
    (read-column-by-index [^String v rsmeta idx] (.toLocalDate v))
    ;;
    Timestamp
    (read-column-by-label [^String v _]          (.toInstant v))
    (read-column-by-index [^String v rsmeta idx] (.toInstant v))
    ;;
    String
    (read-column-by-label [^String v _] v)
    (read-column-by-index [^String v rsmeta idx]
      (let [type-name (.getColumnTypeName rsmeta idx)]
        (if (= type-name "text")
          v
          ;; It could be an enum that has a mapping.
          (let [table-name (.getTableName rsmeta idx)]
            ;; FIXME: do something smart and look it up here, in the
            ;; meantime just turn it into a keyword as that'll be the
            ;; case almost all of the time.
            (keyword v)))))))

(defn sql-dates<->java-time
  []
  (sql-dates<-java-time)
  (sql-dates->java-time))

(defn make-pg-object
  [object-type object-value]
    (doto (PGobject.)
    (.setType  (name object-type))
    (.setValue object-value)))

;;; --------------------------------------------------------------------------------

(def ^:dynamic *datasource*)
(def ^:dynamic *connection*)
(def ^:dynamic *transaction*)

(defmacro with-datasource [datasource & body]
  `(let [datasource# ~datasource]
     (binding [*datasource* datasource#]
       ~@body)))

(defmacro with-connection [& body]
  `(with-open [connection# (jdbc/get-connection *datasource* {:read-only true})]
     (binding [*connection* connection#]
       ~@body)))

(defmacro in-transaction [& body]
  `(with-open [connection# (jdbc/get-connection *datasource*)]
     (binding [*connection* connection#]
       (jdbc/with-transaction [tx# *connection*]
         (binding [*transaction* tx#]
           ~@body)))))

#_
(defmacro in-transaction [& body]
  `(jdbc/with-transaction [tx# *connection*]
     ~@body))

;;; --------------------------------------------------------------------------------

(defn make-pg-object
  [object-type object-value]
  (doto (PGobject.)
    (.setType  (name object-type))
    (.setValue object-value)))

(defn ->ltree
  [v]
  (make-pg-object :ltree v))

;;;

(def get-pg-value (memfn getValue))
(def get-pg-type  (memfn getType))

(defn pg-object->clj
  [^PGobject v]
  (condp = (get-pg-type v)
    "ltree"     (get-pg-value v)
    ;; "daterange" FIXME: create a java.time/Period object out of this.
    ;;
    ;; What about all of the other GIS objects?
    :else (do
            (log/warn (str "No translator for: PGobject/" (get-pg-type v) "."))
            v)))

(extend-protocol rs/ReadableColumn
  PGobject
  (read-column-by-label [^PGobject v _]          (pg-object->clj v))
  (read-column-by-index [^PGobject v rsmeta idx] (pg-object->clj v)))

(defn to-sql-array
  [pg-type coll]
  (when (seq coll)
    (.createArrayOf *connection* pg-type (to-array coll))))

(defn pg-array?
  [x]
  (instance? org.postgresql.jdbc.PgArray x))

;;; --------------------------------------------------------------------------------

(defn get-column-names
  [^ResultSetMetaData rsmeta]
  (mapv (fn [^Integer i]
          (let [type  (-> (.getColumnTypeName rsmeta i) ->kebab-case-keyword)
                label (.getColumnLabel rsmeta i)]
            (-> (case type
                  :bool (str label "?")
                  label)
                ->kebab-case-keyword)))
        (range 1 (inc (.getColumnCount rsmeta)))))

(defn as-sane-maps
  [^ResultSet rs opts]
  (let [rsmeta (.getMetaData rs)
        cols   (get-column-names rsmeta)]
    (rs/->MapResultSetBuilder rs rsmeta cols)))

(defn as-pg-map
  [m]
  (if (map? m)
    (cske/transform-keys ->snake-case-string m)
    ;; Otherwise it's probably a vector of [SQL-STRING params...]
    m))

(defn execute
  [sql-params]
  (jdbc/execute! *connection*
                 sql-params
                 {:builder-fn as-sane-maps}))

(defn execute-one
  [sql & [params]]
  (jdbc/execute-one! *connection*
                     (into [sql] params)
                     {:builder-fn as-sane-maps}))

(defn rollback
  []
  (.rollback *transaction*))

(defn updated?
  [result]
  (cond
    (map? result) (some-> result :next.jdbc/update-count zero? not)
    :else (-> (some-> result
                      first
                      :next.jdbc/update-count
                      zero?
                      not)
              boolean)))

(defn insert-row
  [tablename record]
  (sql/insert! *connection* (->snake-case-string tablename) (as-pg-map record)))

(defn update-rows
  "Woefully inefficient way of getting data but will suffice for now."
  [tablename record where]
  (sql/update! *connection* (->snake-case-string tablename) (as-pg-map record) (as-pg-map where)))

(defn delete-rows
  "Woefully inefficient way of getting data but will suffice for now."
  [tablename where]
  (sql/delete! *connection* (->snake-case-string tablename) (as-pg-map where)))

;;; --------------------------------------------------------------------------------
;;  Spectacular layer

(defn get-table     [k] (or (sp/-get k ::table)
                            (throw (ex-info (format "Failed to get DB Table Name for %s" k)
                                            {:k k}))))
(defn get-name      [k] (sp/-get k ::name))
(defn get-type      [k] (sp/-get k ::type))
(defn get->db-value [k] (sp/-get k ::->db-value))

(defn record->sql
  [record & {:keys [db-names?]}]
  (let [key->q (fn [k]
                 ;; Remove namespace for destructuring in query
                 ;; module.
                 (-> k name keyword))
        get-key (if db-names?
                  #(or (get-name %)
                       (key->q   %))
                  key->q)]
    (->> record
         (map (fn [[k v]]
                (let [v-type  (if (sp/attr? k)
                                (sp/get-attribute-type k)
                                k)
                      ;; The database type can be registered on the
                      ;; attribute or the scalar so check both but
                      ;; preference the attribute.
                      db-type    (or (get-type k)
                                     (get-type v-type))
                      ->db-value (get->db-value v-type)]
                  [(get-key k)
                   (cond
                     (nil? v) nil
                     ;;
                     (and (sp/enum? v-type) (set? v))
                     (to-sql-array (csk/->SCREAMING_SNAKE_CASE_STRING (or db-type v-type))
                                   (map name v))

                     (sp/enum? v-type)
                     (make-pg-object (csk/->SCREAMING_SNAKE_CASE_STRING (or db-type v-type))
                                     (name v))
                     ;;
                     (and db-type ->db-value)
                     (make-pg-object db-type (->db-value v))
                     ;;
                     ->db-value (->db-value v)
                     ;;
                     (= db-type :text-array) (to-sql-array "TEXT" v)
                     ;;
                     db-type
                     (make-pg-object db-type v)
                     ;;
                     ;; Do after db-type as it may coerce keywords to pg enums.
                     (keyword? v) (name v)
                     ;;
                     (or (seq? v) (vector? v)) (when-let [head (first v)]
                                                 (cond
                                                   (string?  head) (to-sql-array "TEXT"    v)
                                                   (integer? head) (to-sql-array "INTEGER" v)
                                                   :else nil))
                     ;;
                     :else v)])))
         (into {}))))

(defn get-identity
  [entity-key record]
  (or (some-> (sp/get-entity-identity entity-key record)
              (record->sql :db-names? true))
      (throw (ex-info (format "Failed to extract identity for %s" entity-key)
                      {:record record}))))

(defn get-values
  [entity-key record values extras]
  (some-> (merge (or values
                     (sp/get-entity-values entity-key record))
                 extras)
          (record->sql :db-names? true)))

;;;

(defn add-entity
  [entity-key record & {:keys [values extras]}]
  (insert-row (get-table entity-key)
              (merge (get-identity entity-key record)
                     (get-values   entity-key record values extras))))

(defn modify-entity
  [entity-key record & {:keys [values extras]}]
  (let [values (or (get-values   entity-key record values extras)
                   (throw (ex-info (format "Failed to extract values for %s" entity-key)
                                   {:entity-key entity-key
                                    :record     record
                                    :values     values
                                    :extras     extras})))]
    (update-rows (get-table    entity-key)
                 values
                 (get-identity entity-key record))))

(defn rename-entity
  [entity-key record field-keys->new-field-keys]
  (update-rows (get-table    entity-key)
               (-> (->> field-keys->new-field-keys
                        (map (fn [[id-field-key new-value-key]]
                               [id-field-key (get record new-value-key)]))
                        (into {}))
                   (record->sql :db-names? true))
               (get-identity entity-key record)))

(defn remove-entity
  [entity-key record]
  (delete-rows (get-table    entity-key)
               (get-identity entity-key record)))
