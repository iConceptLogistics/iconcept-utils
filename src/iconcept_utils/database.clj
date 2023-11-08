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
            [oberon.utils :refer [dump-> dump->> ->kebab-case-keyword ->snake-case-string ->screaming-snake-case-string]]
            [spectacular.core :as sp])
  (:import [java.sql
            Date Timestamp
            Array
            Clob
            PreparedStatement
            ResultSet ResultSetMetaData
            Statement
            SQLException]
           [org.postgresql.util PGobject]
           ;;
           [java.time LocalDate Instant]
           [java.time.format DateTimeFormatter]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp (atom nil))
(defn-   ^:private set-tmp!
  [value]
  (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------
;;  Spectacular layer

(defn get-table [k] (or (sp/-get k ::table)
                        (throw (ex-info (format "Failed to get DB Table Name for %s" k)
                                        {:k k}))))
(defn get-name  [k] (or (sp/-get k ::name)
                        (when (sp/attr? k)
                          (-> k sp/get-attribute-type (sp/-get ::name)))))
(defn get-type  [k] (or (sp/-get k ::type)
                        (when (sp/attr? k)
                          (-> k sp/get-attribute-type (sp/-get ::type)))))

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

;;; --------------------------------------------------------------------------------
;;  Simple value converters

(def +yyyy-mm-dd+ (DateTimeFormatter/ofPattern "yyyy-MM-dd"))

(defn yyyy-mm-dd->ld
  [^String ld]
  (LocalDate/parse ld))

(defn ld->yyyy-mm-dd
  [^LocalDate ld]
  (.format ld +yyyy-mm-dd+))


;;; --------------------------------------------------------------------------------
;;  WRITING TO DATABASE, INCLUDING COERCION FOR QUERIES

(defn make-pg-object
  [object-type object-value]
  (doto (PGobject.)
    (.setType  (name object-type))
    (.setValue object-value)))

(defn make-pg-array
  [pg-type coll]
  (.createArrayOf *connection* pg-type (to-array coll)))

(def get-pg-value (memfn getValue))
(def get-pg-type  (memfn getType))

;;; --------------------------------------------------------------------------------
;;  Custom PG type handling

(defn make-enum
  [enum-type enum-value]
  (make-pg-object (csk/->SCREAMING_SNAKE_CASE_STRING enum-type) (name enum-value)))

(defn make-daterange
  [[from to]]
  (make-pg-object :daterange
                  (str "["
                       (some-> from ld->yyyy-mm-dd)
                       ","
                       (some-> to ld->yyyy-mm-dd)
                       "]")))

;;; FIXME: Add these ranges in too
;; int4range — Range of integer, int4multirange — corresponding Multirange
;; int8range — Range of bigint, int8multirange — corresponding Multirange
;; numrange — Range of numeric, nummultirange — corresponding Multirange
;; tsrange — Range of timestamp without time zone, tsmultirange — corresponding Multirange
;; tstzrange — Range of timestamp with time zone, tstzmultirange — corresponding Multirange

(defn make-ltree
  [v]
  (make-pg-object :ltree v))

;;;

(defmulti clj->array (fn [array-type v options]
                       array-type))

(defmethod clj->array :default
  [_ v options]
  nil)

(defmethod clj->array java.lang.String
  [_ v options]
  (make-array "TEXT" v))

(defmethod clj->array java.lang.Integer
  [_ v options]
  (make-array "INT" v))

(defmethod clj->array java.lang.Long
  [_ v options]
  (make-array "INT" v))

(defmethod clj->array java.util.Date
  [_ v options]
  (->> v
       (map #(Date/valueOf %))
       (make-array "DATE")))

(defmethod clj->array java.time.LocalDate
  [_ v options]
  (->> v
       (map #(Date/valueOf %))
       (make-array "DATE")))

(defmethod clj->array java.time.Instant
  [_ v options]
  (->> v
       (map #(Timestamp/valueOf %))
       (make-array "TIMESTAMP")))

(defn clj->array*
  [v {:keys [k domain] :as options}]
  (or (some-> (get-type k) (clj->array v options))
      ;;
      (when (and domain k) (clj->array [domain k] v options))
      (when k              (clj->array k          v options))
      (when domain         (clj->array domain     v options))
      (clj->array (-> v first type) v options)
      v))

(defn arrayable?
  [v]
  (or (vector? v) (list? v)))

;;;

(defmulti clj->db (fn [type-info v {:as options}]
                    type-info))

(defmethod clj->db :default
  [_ v _]
  nil)

(defmethod clj->db java.util.Date
  [_ v _]
  (Date/valueOf v))

(defmethod clj->db java.time.LocalDate
  [_ v _]
  (Date/valueOf v))

(defmethod clj->db java.time.Instant
  [_ v _]
  (Timestamp/valueOf v))

(defmethod clj->db clojure.lang.Keyword
  [_ v {:keys [k enum-type]}]
  (if-let [enum-type (or enum-type (get-type k))]
    (make-enum enum-type v)
    (name v)))

(defn clj->db*
  [v {:keys [k domain] :as options}]
  (or (some-> (get-type k) (clj->db v options))
      (when (and domain k) (clj->db [domain k] v options))
      (when k              (clj->db k          v options))
      (when domain         (clj->db domain     v options))
      (clj->db (type v) v options)
      v))

(defn record->row
  [record & {:keys [domain db-names?]}]
  (->> record
       (map (fn [[k v]]
              (when v
               (let [options {:k k :domain domain}]
                 [(or (and db-names? (get-name k))
                      ;; Rows don't know about namespaces.
                      (keyword (name k)))
                  (if (arrayable? v)
                    (clj->array* v options)
                    (clj->db*    v options))]))))
       (into {})))

;;; --------------------------------------------------------------------------------
;;; READING FROM DATABASE

(defmulti read-object (fn [object-type object]
                        object-type))

(defmethod read-object :default
  [object-type object]
  (log/warn (format "No read-object method found for: `%s`." object-type))
  object)

(defmethod read-object :ltree
  [_ v]
  (get-pg-value v))

(let [object-reader (fn [object]
                      (read-object (-> object get-pg-type keyword) object))]
  (extend-protocol rs/ReadableColumn
    PGobject
    (read-column-by-label [^PGobject object _]          (object-reader object))
    (read-column-by-index [^PGobject object rsmeta idx] (object-reader object))))

#_
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

(defn get-identity
  [entity-key record]
  (or (some-> (sp/get-entity-identity entity-key record)
              (record->row :domain    (get-table entity-key)
                           :db-names? true))
      (throw (ex-info (format "Failed to extract identity for %s" entity-key)
                      {:record record}))))

(defn get-values
  [entity-key record values extras]
  (some-> (merge (or values
                     (sp/get-entity-values entity-key record))
                 extras)
          (record->row :domain    (get-table entity-key)
                       :db-names? true)))

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
    (update-rows (get-table entity-key)
                 values
                 (get-identity entity-key record))))

(defn rename-entity
  [entity-key record field-keys->new-field-keys]
  (update-rows (get-table    entity-key)
               (-> (->> field-keys->new-field-keys
                        (map (fn [[id-field-key new-value-key]]
                               [id-field-key (get record new-value-key)]))
                        (into {}))
                   (record->row :domain    (get-table entity-key)
                                :db-names? true))
               (get-identity entity-key record)))

(defn remove-entity
  [entity-key record]
  (delete-rows (get-table    entity-key)
               (get-identity entity-key record)))
