(ns yaml-config-manager.config-test
  (:require [clojure.test :refer :all]
            [yaml-config-manager.config :refer :all]))

(def db-secrets {"dbUser" "DB-USER-SUCCESS", "dbPassword" "DB-PASSWORD-SUCCESS"})
(def empty-secrets {})

(deftest secrets-test
  (testing "Secrets are replaced"
    (are [s secrets target] (= (replace-secrets s secrets) target )
      "${dbUser} doesn't change ${dbPassword}" db-secrets "DB-USER-SUCCESS doesn't change DB-PASSWORD-SUCCESS"
      "${dbUser} doesn't change ${dbPassword}" empty-secrets "${dbUser} doesn't change ${dbPassword}"
      "dbUser doesn't change dbPassword" db-secrets "dbUser doesn't change dbPassword"
      "dbUser doesn't change dbPassword" empty-secrets "dbUser doesn't change dbPassword"
    )))

(deftest to-prop-name-test
  (testing "To Prop Name"
    (are [ks expected] (= (to-prop-name ks) expected)
      [:a :b :c] "a.b.c"
      [:a] "a"
      [:a :b] "a.b"
      [] ""
    )))

(deftest assoc-property-data-test
  (testing "Properties are assoc"
    (is (= (assoc-property-data {} [:a :b :c :world] "hello") {:prop "a.b.c.world" :ks [:a :b :c :world] :val "hello"} ))
    (is (= (assoc-property-data {} [:a] "hello") {:prop "a" :ks [:a] :val "hello"} ))
    ))