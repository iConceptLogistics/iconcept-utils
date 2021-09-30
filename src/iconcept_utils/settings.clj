(ns iconcept-utils.settings
  (:require [clojure.java.io :as io]))

(defn read-settings
  [settings]
  (some-> settings
          name
          (str ".edn")
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
  ([env-key setting-key & setting-keys]
   (load-settings env-key (into [setting-key] setting-keys)))
  ([env-key setting-keys]
   (let [envs    (->> (if (seqable? setting-keys)
                        setting-keys
                        [setting-keys])
                      flatten
                      (map read-settings)
                      ;; They can pass setting-keys that don't
                      ;; correspond to edn files, we just ignore them.
                      ;; This allows for a local override file to be
                      ;; used if it exists.
                      (remove nil?))
         shared  (->> (map :shared envs)
                      (apply merge))
         ;;
         {:keys [uses] :as primary} (-> envs last (get-in [:envs env-key]))]

     (when-not primary
       (throw (ex-info "Primary settings for env-key don't exist." {:env-key      env-key
                                                                    :setting-keys setting-keys})))
     (-> (merge (reduce (fn [result k]
                          (when-not (contains? shared k)
                            (throw (ex-info "Uses key not found in shared environments."
                                            {:env-key      env-key
                                             :setting-keys setting-keys
                                             :uses         uses})))
                          (merge result (get shared k)))
                        nil
                        uses)
                primary)
         (dissoc :uses)))))
