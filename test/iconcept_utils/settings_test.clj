(ns iconcept-utils.settings-test
  (:require [clojure.test :refer :all])
  (:require [iconcept-utils.settings :refer :all]
            :reload-all))

(deftest loading-settings
  (testing "Single Settings File"
    (is (= (load-settings :env-1 :settings-1)
           {:value-1 :value-1
            :value-2 :value-2
            :config  :config-1}))

    (is (= (load-settings :env-1 :settings-2)
           {:value-1 :value-3
            :value-2 :value-4
            :config  :config-1}))

    (is (= (load-settings :env-2 :settings-2)
           {:value-1  :value-3
            :value-2  :value-4
            :config   nil}))

    (is (= (load-settings :env-3 :settings-2)
           {:value-1  :value-3
            :value-2  :value-4
            :config   :config})))

  (testing "Multiple Setting Files"
    (is (= (load-settings :env-1 :settings-2 :settings-1)
           {:value-1 :value-1
            :value-2 :value-2
            :config  :config-1}))

    (is (= (load-settings :env-1 [:settings-2 :settings-1])
           {:value-1 :value-1
            :value-2 :value-2
            :config  :config-1}))

    (is (= (load-settings :env-1 :settings-2 [:settings-1])
           {:value-1 :value-1
            :value-2 :value-2
            :config  :config-1}))

    (is (= (load-settings :env-1 :settings-2 :settings-1 [])
           {:value-1 :value-1
            :value-2 :value-2
            :config  :config-1}))

    (is (= (load-settings :env-1 :settings-2 :settings-1 [nil] nil)
           {:value-1 :value-1
            :value-2 :value-2
            :config  :config-1}))

    (is (= (load-settings :env-1 :settings-1 :settings-2)
           {:value-1 :value-3
            :value-2 :value-4
            :config  :config-1}))))

(deftest bad-settings
  (testing "Bad Key"
    (is (thrown? clojure.lang.ExceptionInfo
                 (load-settings :no-here :settings-2 :settings-1)))

    (is (thrown? clojure.lang.ExceptionInfo
                 (load-settings :env-4 :settings-2)))))
