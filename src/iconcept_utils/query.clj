(ns iconcept-utils.query
  (:require [taoensso.timbre :as log]
            ;;
            [honey.sql         :as sql]
            [honey.sql.helpers :as h]
            [honey.sql.pg-ops :refer [<at at>]]
            ;;
            [spectacular.core :as sp]
            [oberon.utils :refer [prefix-keyword]]
            [iconcept-utils.database :as db]))

;;; --------------------------------------------------------------------------------

(defonce ^:private tmp      (atom nil))
(defn-   ^:private set-tmp! [value] (swap! tmp (constantly value)))

;;; --------------------------------------------------------------------------------
;;  Honeysql Helpers

(defn like%
  [v]
  ;; FIXME: workout how to replace this with an extention to honeysql
  ;; so we can just use [:ilike% :field "value"] and it'll stick the
  ;; % in.
  (str "%" v "%"))

;;; --------------------------------------------------------------------------------

(def order-by* #(apply h/order-by %1 %2))
(def group-by* #(apply h/group-by %1 %2))

(defn select*
  ([prefix colls]
   (select* nil prefix colls))
  ([sql prefix colls]
   (->> colls
        (map #(prefix-keyword prefix %))
        (into (if sql [sql] []))
        (apply h/select))))

(defn page-by
  [sql {{:keys [index size]} :paging}]
  (cond-> sql
    size (-> (h/offset (* size (or index 0)))
             (h/limit size))))

(defn row-count
  [sql {:keys [count?]}]
  (cond-> sql
    count? (-> (h/select [[:over [[:count :*]]]
                          :total]))))

(defn add-options
  [sql options]
  (-> sql
      (page-by   options)
      (row-count options)))

(defn execute-sql
  [sql & [debug?]]
  (let [[sql & params :as query] (sql/format sql :pretty debug?)]
    (when debug?
      (println sql)
      (println params))
    (db/execute query)))

(defn execute-raw-sql
  [sql & params]
  (db/execute (into [sql] params)))

(defn fetch-all
  [sql debug?]
  (-> (execute-sql sql debug?)
      doall))

;;; --------------------------------------------------------------------------------
;;  We use tags a lot and this helps us keep them all in sync.

(defn remove-tag
  [table column where tag-id & {:keys [array-type debug?]
                                :or   {array-type :text}}]
  (let [sql (-> (h/update table)
                (h/set {column [:remove_from_array column
                                 [:cast tag-id array-type]]})
                (h/where [:and where
                           [:= tag-id [:any :tags]]]))]
    (execute-sql sql debug?)))

(defn rename-tag
  [table column where tag-id new-tag-id & {:keys [array-type debug?]
                                           :or   {array-type :text}}]
  (let [sql (-> (h/update table)
                (h/set {column [:replace_in_array column
                                [:cast tag-id     array-type]
                                [:cast new-tag-id array-type]]})
                (h/where [:and where
                          [:= tag-id [:any :tags]]]))]
    (execute-sql sql debug?)))

;;; --------------------------------------------------------------------------------

(defmulti fetch (fn [kind & [matching & {:as opts}]]
                  kind))

(defn one
  [kind matching & {:keys [throw?] :as opts}]
  (let [opts (->> (-> opts
                      (assoc  :page {:size 2})
                      (dissoc :throw?))
                  (into (list))
                  flatten)
        records (apply fetch kind matching opts)]
    (if (= (count records) 1)
      (first records)
      (when throw?
        (throw (ex-info "Failed to find exactly 1 record."
                        {:kind kind :matching matching}))))))

;;;

(defn entity-present?
  [entity-type entity & {:keys [matching debug?]}]
  (let [table    (db/get-table entity-type)
        matching (merge (sp/get-entity-identity entity-type entity)
                        matching)]
    (one table matching :debug? debug?)))

(def entity-absent? (complement entity-present?))
