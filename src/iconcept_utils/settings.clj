(ns iconcept-utils.settings
  (:require [clojure.java.io :as io]))

(defn read-settings
  [settings]
  (some-> (str (name settings) ".edn")
          io/resource
          slurp
          clojure.edn/read-string))

(defn load-settings
  "
  env-key is the environment to load.

  env-keys are the keys that represent the available environments
  that will be loaded from resources.

  Priority for env-keys is the same as for merge, ie, last one wins.

  First Loads the environment for env-key from the last environment.

  Then looks for the 'uses' key and then merges in the values from
  the 'shared' sections.
"
  [env-key & env-keys]
  (let [envs    (map read-settings env-keys)
        shared  (->> (map :shared envs)
                     (apply merge))
        ;;
        {:keys [uses] :as primary} (-> envs last (get-in [:envs env-key]))]

    (when-not primary
      (throw (ex-info "Primary settings for env-key don't exist." {:env-key  env-key
                                                                   :env-keys env-keys})))
    (-> (merge (reduce (fn [result k]
                         (when-not (contains? shared k)
                           (throw (ex-info "Uses key not found in shared environments."
                                           {:env-key  env-key
                                            :env-keys env-keys
                                            :uses     uses})))
                         (merge result (get shared k)))
                       nil
                       uses)
               primary)
        (dissoc :uses))))
