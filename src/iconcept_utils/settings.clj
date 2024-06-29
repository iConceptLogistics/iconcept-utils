(ns iconcept-utils.settings
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.walk :refer [postwalk]]))

(defn env-var?
  [v]
  (and (map? v)
       (:env-name v)))

(defn get-env-var*
  [env-name]
  (let [var (System/getenv env-name)]
    (if-not (s/blank? var)
      (s/trim var))))

(defmulti get-env-var (fn [{:keys [env-type]}]
                        env-type))

(defmethod get-env-var :default
  [{:keys [env-name default]}]
  (or (get-env-var* env-name)
      default))

(defmethod get-env-var :integer
  [{:keys [env-name default]}]
  (or (some-> env-name get-env-var* Integer/parseInt)
      default))

(defn resolve-env-vars
  [m]
  (postwalk #(cond
               (env-var? %) (get-env-var %)
               :else %)
            m))

;;;

(defn get-setting-value
  [config k]
  (let [v (get config k)]
    (if (env-var? v)
      (get-env-var v)
      v)))

(defn canonicalise-env-specs
  [env-specs]
  (->> env-specs
       flatten
       (remove nil?)
       (map keyword)
       seq))

(defn read-settings
  [settings]
  (some-> settings
          name
          (str ".edn")
          io/resource
          slurp
          clojure.edn/read-string
          resolve-env-vars))

(defn load-settings
  "
  `env-key` is the environment to load.

  `settings` is a seq of keywords that will get translated into
  filenames that contain the settings to be loaded.

  Priority for env-keys is the same as for merge, ie, last one wins.

  First Loads the environment for env-key from the last environment.

  Then looks for the 'uses' key and then merges in the values from
  the 'shared' sections.
"
  ([env-key settings]
   (let [envs    (->> settings
                      (map read-settings)
                      ;; They can pass settings that don't correspond
                      ;; to edn files, we just ignore them.  This
                      ;; allows for a local override file to be used
                      ;; if it exists.
                      (remove nil?))
         shared  (->> (map :shared envs)
                      (apply merge))
         ;;
         {:keys [uses] :as primary} (-> envs last (get-in [:envs env-key]))]

     (when-not primary
       (throw (ex-info (format "Primary settings for env-key do not exist in `%s`." (last settings))
                       {:env-key  env-key
                        :settings settings})))
     (-> (merge (reduce (fn [result k]
                          (when-not (contains? shared k)
                            (throw (ex-info "Uses key not found in shared environments."
                                            {:env-key      env-key
                                             :settings settings
                                             :uses         uses})))
                          (merge result (get shared k)))
                        nil
                        uses)
                primary)
         (dissoc :uses)))))
