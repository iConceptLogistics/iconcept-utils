(ns iconcept-utils.gql-testing
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.walk   :refer [postwalk]]
            [clojure.java.io :as io]
            ;;
            [cheshire.core     :refer [generate-string parse-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            ;;
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

(defn fragment? [tmpl] (s/starts-with? tmpl "fragment "))
(defn query?    [tmpl] (s/starts-with? tmpl "query "))
(defn mutation? [tmpl] (s/starts-with? tmpl "mutation "))

(defn split-graphql
  [template]
  (let [section-start (fn [line]
                        (let [line (s/trim line)]
                          (or (fragment? line)
                              (query?    line)
                              (mutation? line))))
        section-name  (fn [line]
                        (-> (re-find #"^(fragment|query|mutation)\W*(\w*)" line) (nth 2)))]
    ;;
    (->> (s/split-lines template)
         (partition-by section-start)
         (partition 2)
         (map (fn [[head body]]
                (let [head (-> head first s/trim)
                      type (cond
                             (fragment? head) :fragment
                             (query?    head) :query
                             (mutation? head) :mutation)
                      ;;
                      id   (->> head section-name csk/->kebab-case-keyword)
                      body (format "%s\n%s" head (s/join "\n" body))
                      ;;
                      fragments (some->> (re-seq #"\W\.{3}([^\W]*)" body)
                                         (mapv #(-> % second csk/->kebab-case-keyword)))]
                  {:type      type
                   :id        id
                   :body      body
                   :fragments fragments}))))))

(defonce +fragments+ nil)
(defonce +templates+ nil)
(defonce +lookups+   nil)

(defn get-all-fragment-ids
  [fragments]
  (let [fragment-ids (atom [])]
    (loop [queue   fragments
           visited #{}]
      (let [[fragment-id & queue] queue]
        (cond
          (nil? fragment-id) (reverse @fragment-ids)
          ;;
          (contains? visited fragment-id)
          (recur queue visited)
          ;;
          :else
          (do
            (swap! fragment-ids #(conj % fragment-id))
            (recur (concat queue (get-in +fragments+ [fragment-id :fragments]))
                   (conj visited fragment-id))))))))

(defn init-templates!
  [path]
  (let [sections (->> path
                      io/file
                      file-seq
                      (map #(when (and (.isFile %)
                                       (let [path (.getAbsolutePath %)]
                                         (or (s/ends-with? path ".gql")
                                             (s/ends-with? path ".graphql"))))
                              (slurp %)))
                      (remove empty?)
                      (mapcat split-graphql))
        ;;
        fragment? #(-> % :type (= :fragment))
        ;;
        fragments (->> sections (filter fragment?) (map (juxt :id identity)) (into {}))
        templates (->> sections (remove fragment?) (map (juxt :id identity)) (into {}))
        lookups   (->> sections
                       (filter :fragments)
                       (map (juxt :id :body))
                       (into {}))]
    (alter-var-root (var +fragments+) (constantly fragments))
    (alter-var-root (var +templates+) (constantly templates))
    (alter-var-root (var +lookups+)   (constantly lookups))))

(defn get-graphql
  [query-id]
  (let [{:keys [body fragments] :as tmpl} (or (get +templates+ query-id)
                                              (throw (ex-info (format "Failed to find template for `%s`" query-id)
                                                              {:query-id query-id})))
        fragment-ids (get-all-fragment-ids fragments)]
    (with-out-str
      (doseq [fragment-id fragment-ids]
        (println (get-in +fragments+ [fragment-id :body])))
      (println body))))

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

;; There are also helpers for common encoding actions:
(defn encode-local-date
  "Encode a date object to the json generator."
  [d jg]
  (.writeString jg (.toString d)))

(add-encoder java.time.LocalDate encode-local-date)

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
        body  (generate-string {:query query :variables vars})]
    (when debug?
      (println query)
      (println (generate-string vars {:pretty true})))
    (-> (gql-request-handler (assoc request :body (java.io.StringReader. body)))
        :body parse-response)))

;;; --------------------------------------------------------------------------------

(defn log-ignore
  []
  (println "\nIGNORE!!! {:error-type :content ...} below, it's the backend logging the error that we are then checking here."))
