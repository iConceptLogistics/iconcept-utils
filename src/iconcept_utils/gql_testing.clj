(ns iconcept-utils.gql-testing
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.walk   :refer [postwalk]]
            [clojure.java.io :as io]
            ;;
            [cheshire.core :refer [generate-string parse-string]]
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [oberon.utils :refer [dump-> dump->>]]
            ;;
            [coerce.core  :refer [coerce]]))


;;; --------------------------------------------------------------------------------
;;  Loading of the GQL fragments

(defn load-graphql-file
  [path kind]
  (some-> (format "%s/%s.graphql" path (name kind))
          io/resource
          slurp))

(defn split-graphql
  [template]
  (let [section-start (fn [line]
                        (or (s/starts-with? line "fragment ")
                            (s/starts-with? line "query ")
                            (s/starts-with? line "mutation ")))
        section-name (fn [line]
                       (-> (re-find #"^(fragment|query|mutation)\W*(\w*)" line) (nth 2)))]
    ;;
    (->> template
         s/split-lines
         ;; Don't care about indented comments with mutations
         (remove #(s/starts-with? % "#"))
         (remove s/blank?)
         (partition-by section-start)
         (partition 2)
         (map (fn [[head body]]
                (let [head (first head)
                      body (format "%s\n%s" head (s/join "\n" body))
                      id   (->> head
                                section-name
                                csk/->kebab-case-keyword)]
                  [id body])))
         (into {}))))

(def +templates+ nil)

(defn init-templates!
  [path]
  (let [tmpls {:fragments (->> (load-graphql-file path :fragments) split-graphql)
               :mutations (->> (load-graphql-file path :mutations) split-graphql)
               :queries   (->> (load-graphql-file path :queries)   split-graphql)}]
    (alter-var-root (var +templates+) (constantly tmpls))))

(defn get-graphql
  [query-id]
  ;; FIXME: add a lookup for including any required fragments and
  ;; include them at the top.
  (or (get-in +templates+ [:queries query-id])
      (get-in +templates+ [:mutations query-id])))

;;; --------------------------------------------------------------------------------

(defn absent?
  [s]
  (s/includes? s "must be absent"))

(defn present?
  [s]
  (s/includes? s "must be present"))

(defn date?
  [x]
  (instance? java.util.Date x))

(defn map-entry [k v]
  (clojure.lang.MapEntry/create k v))

(defn clj->vars
  [m & [transformers]]
  (let [default-transformer (fn [v]
                              (cond
                                (keyword? v) (csk/->SCREAMING_SNAKE_CASE_STRING v)
                                (date?    v) (coerce v :string)
                                (set?     v) (mapv csk/->SCREAMING_SNAKE_CASE_STRING v)
                                :else v))
        transform           (fn [[k v]]
                              (let [transformer (get transformers k default-transformer)]
                                (map-entry (csk/->camelCaseString k)
                                           (transformer v))))
        f (fn [x]
            (cond
              (map-entry? x) (transform x)
              ;; (map?       x) x
              ;; (vector?    x) x
              ;;
              :else x))]
    (postwalk f m)))

(defn parse-response
  [body]
  (let [payload (parse-string body)
        data    (get payload "data")
        errors  (get payload "errors")]
    (cond
      ;; Just getting the first error is enough for unit testing.
      errors {:data  nil
              :error (or (get-in errors [0 "extensions" "errors"])
                         (->> errors
                              first
                              (cske/transform-keys csk/->kebab-case-keyword)))}
      ;; The data is always nested under the name of the mutation, so
      ;; it's always a map with only one entry, just promote it to the
      ;; data to avoid having to destructure it later on
      :else {:data  (->> data first second (cske/transform-keys csk/->kebab-case-keyword))
             :error nil})))


(defn send-gql
  [query vars gql-request-handler & {:keys [request debug?]}]
  ;; Mimic sending a request, later we can add security headers, etc,
  ;; to the testing.
  (let [query (cond
                (keyword? query) (or (get-graphql query)
                                     (throw (ex-info (format "Graphql not found for: %s" query)
                                                     {:query query})))
                (string?  query) query
                ;;
                :else (throw (ex-info "Query can't be used." {:query query})))

        vars  (clj->vars vars)
        body  (-> {:query     query
                   :variables vars}
                  generate-string)]
    (when debug?
      (println query)
      (println (generate-string vars)))
    (-> (gql-request-handler (assoc request :body (java.io.StringReader. body)))
        :body parse-response)))

;;; --------------------------------------------------------------------------------

(defn log-ignore
  []
  (println "\nIGNORE!!! {:error-type :content ...} below, it's the backend logging the error that we are then checking here."))
