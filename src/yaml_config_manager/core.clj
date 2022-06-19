(ns yaml-config-manager.core
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io])
  (:use [clojure.walk :as [walk]])
  )

(use 'yaml-config-manager.core :reload-all)
; Something to note is that clj-yaml uses flatland ordered maps behind the scenes
(def a-yaml (slurp "sample_yaml/a.yaml"))
(def b-yaml (slurp "sample_yaml/b.yaml"))
(def c-yaml (slurp "sample_yaml/c.yaml"))
(def a-parsed (yaml/parse-string a-yaml))

(def app-db (atom {}) )

(def m a-parsed)
(def l (get-in a-parsed [:top :properties]))
(def n (name :properties))
(def props [])

(to-properties a-parsed)

; Idea for this is that we'll pop things over to properties because I like reading them that way more, but do all edits only to yaml?
(defn to-properties [m]
  ; Idea: form smaller and smaller maps by appending the key. to the key of every child map
  (prn "m" m)
  (if (map? m)
    (map (fn [child-map]
           (prn "Child Map" child-map)
           (let [n (name (first child-map))
                 l (second child-map)]
             (if (list? l)
               (map to-properties (clojure.set/rename-keys l (reduce #(assoc %1 %2 (clojure.string/join [n "." (name %2)])) {} (keys l))))
               child-map)
             )
           ) m)
    m)
  )

(defn my-walk [path terms m]
    (if (map? m)
      (reduce #(apply conj %1 ((fn [k]
                             (if (map? (get m k))
                               (my-walk (conj path k) terms (get m k))
                               [[(conj path k) (get m k)]]
                               )
                             ) %2)) []  (keys m))
      terms
      ))

(my-walk [] [] a-parsed)
(my-walk [] [] {:a {:b 2 :c {:d 3 :e 4}}})


(defn read-file [f]
  (if (.exists (io/file f))
    (swap! app-db assoc f (slurp f))))




(comment "Loads all yaml files used for testing purposes"
  (read-file "sample_yaml/a.yaml")
  (read-file "sample_yaml/b.yaml")
  (read-file "sample_yaml/c.yaml")
  (read-file "sample_yaml/non_existant.yaml")
)
