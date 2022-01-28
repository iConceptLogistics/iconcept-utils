(ns iconcept-utils.database
  (:require [clojure.string :as s]
            ;;
            [next.jdbc            :as jdbc]
            [next.jdbc.sql        :as sql]
            [next.jdbc.prepare    :as p]
            [next.jdbc.result-set :as rs]
            ;;
            [camel-snake-kebab.extras :as cske]
            ;;
            [oberon.utils :refer [->kebab-case-keyword ->snake-case-string ->screaming-snake-case-string]])
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

#_
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
