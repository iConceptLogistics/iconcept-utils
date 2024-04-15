(ns iconcept-utils.deprecated.database
  (:require [clojure.string :as s]
            ;;
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            next.jdbc.date-time
            [next.jdbc.sql :as sql]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske])
  (:import [org.postgresql.util PGobject]
           [java.sql
            Array
            Clob
            PreparedStatement
            ResultSet ResultSetMetaData
            Statement
            SQLException]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp (atom nil))
(defn-   ^:private set-tmp!
  [value]
  (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------
;;  Sometimes we need to override how the translations happen, ie,
;;  maybe we want to cache aggressively or handle the alpha/digit
;;  barrier in words differently to the default csk way of doing it.
;;
;;  Or maybe we are integrating with some sort of system that has
;;  really weird casing for particular field names.

(defonce ->kebab-case-keyword          csk/->kebab-case-keyword)
(defonce ->snake-case-string           csk/->snake_case_string)
(defonce ->screaming-snake-case-string csk/->SCREAMING_SNAKE_CASE_STRING)

;;; --------------------------------------------------------------------------------

(def +enums+           (atom {}))
(def +enum-namespaces+ (atom {}))

(defn register-enum-types
  [enum-types]
  (swap! +enums+ merge enum-types)
  (let [namespaces (->> @+enums+
                        (map (fn [[k v]]
                               [(->snake-case-string k) (name v)]))
                        (into {}))]
    (swap! +enum-namespaces+ (constantly namespaces))))

(defn get-enum-type
  [key]
  (get @+enums+ key))

(defn get-enum-ns
  [key]
  (get @+enum-namespaces+ key))

(defn kw->pgenum
  ([enum-value]
   (when enum-value
     (let [type  (-> enum-value namespace ->screaming-snake-case-string)
           value (-> enum-value name)]
       (doto (PGobject.)
         (.setType  type)
         (.setValue value)))))
  ([enum-type enum-value]
   (when enum-value
     (doto (PGobject.)
       (.setType  (-> enum-type  ->screaming-snake-case-string))
       (.setValue (-> enum-value name))))))

(defn clj->pg
  [m]
  (reduce (fn [m k]
            (let [value (m k)]
              (cond
                (nil? value) m
                ;;
                :else (if-let [enum-type (get-enum-type k)]
                        (assoc m k (kw->pgenum enum-type (m k)))
                        m))))
          m
          (keys m)))

(extend-protocol rs/ReadableColumn
  Array
  (read-column-by-label [^Array v _]    (vec (.getArray v)))
  (read-column-by-index [^Array v _ _]  (vec (.getArray v)))
  ;;
  String
  (read-column-by-label [^String v _] v)
  (read-column-by-index [^String v rsmeta idx]
    (if-let [ns (-> (.getColumnTypeName rsmeta idx) get-enum-ns)]
      ;; FIXME: workout how to have the namespace but also have it get
      ;; marshalled correctly.
      #_(keyword ns v)
      (keyword v)
      v)))

;;; --------------------------------------------------------------------------------

(def ^:dynamic *datasource*)
(def ^:dynamic *connection*)

(defmacro with-datasource [datasource & body]
  `(let [datasource# ~datasource]
     (binding [*datasource* datasource#]
       ~@body)))

(defmacro with-connection [& body]
  `(with-open [connection# (jdbc/get-connection *datasource*)]
     (binding [*connection* connection#]
       ~@body)))

(defmacro in-transaction* [& body]
  `(with-connection
     (jdbc/with-transaction [tx# *connection*]
       ~@body)))

(defmacro in-transaction [& body]
  `(jdbc/with-transaction [tx# *connection*]
     ~@body))

;;; --------------------------------------------------------------------------------

(defn util-date->sql-date
  [^java.util.Date v]
  (when v
    (java.sql.Date. (.getTime v))))

(defn sql-date->util-date
  [^java.sql.Date v]
  (when v
    (java.util.Date. (.getTime v))))

(defn to-sql-array
  "Warning: assumes *connection* is bound."
  ([coll]
   (when-let [head (first coll)]
     (cond
       (string?  head) (to-sql-array "text" coll)
       (keyword? head) (to-sql-array "text" (map name coll))
       ;;
       ;; (instance? java.util.Date head) (to-sql-array "date" (map #(coerce % java.sql.Date) coll))
       ;; take a punt on integers
       :else
       (to-sql-array "integer" coll))))

  ([pg-type coll]
   (when (seq coll)
     (.createArrayOf *connection* (->snake-case-string pg-type)
                     (if (keyword? (first coll))
                       (->> coll (map name) to-array)
                       (to-array coll))))))

;;;

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
