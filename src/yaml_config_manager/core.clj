(ns yaml-config-manager.core
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io])
  (:use [clojure.walk :as [walk]])
  )

(use 'yaml-config-manager.core :reload-all)
; Something to note is that clj-yaml uses flatland ordered maps behind the scenes
(comment "Helper definitions for development"
  (def a-yaml (slurp "sample_yaml/a.yaml"))
  (def b-yaml (slurp "sample_yaml/b.yaml"))
  (def c-yaml (slurp "sample_yaml/c.yaml"))
  (def a-parsed (yaml/parse-string a-yaml))
  (def b-parsed (yaml/parse-string b-yaml))
  (def c-parsed (yaml/parse-string c-yaml))
)

(def app-db (atom {}) )
(defn reset-db [] (reset! app-db {}))

(defn to-properties
  ([m] (to-properties [] [] m))
  ([path terms m]
   (if (map? m)
     (reduce #(apply conj %1 ((fn [k]
                                (if (map? (get m k))
                                  (to-properties (conj path k) terms (get m k))
                                  [[(conj path k) (get m k)]]
                                  )
                                ) %2)) [] (keys m))
     terms
     )))

(comment "Turns a parsed yaml map into a format which lists all keys next to the value"
  (to-properties a-parsed)
  (to-properties b-parsed)
  (to-properties c-parsed)
  (to-properties {:a {:b 2 :c {:d 3 :e 4}}})
)

(defn to-prop-name [ks] "Takes a key structure. Returns the spring properties that would be associated with it"
  (clojure.string/join "." (map name ks)))

(defn attach-metadata [parsed-yaml]
  (let [props (to-properties parsed-yaml)]
    (reduce #(assoc %1 (to-prop-name (first %2)) {:prop (to-prop-name (first %2)), :ks (first %2), :val (second %2)}) {} props)
    )
  )

(comment [] "Helpers for debugging attach-metadata"
  (attach-metadata a-parsed)
  (attach-metadata b-parsed)
  (attach-metadata c-parsed)
)

(defn read-file [f]
  (if (.exists (io/file f))
    (let [yaml (slurp f)
          parsed-yaml (yaml/parse-string yaml)]
      (swap! app-db assoc f {:yaml yaml, :parsed-yaml parsed-yaml, :properties (attach-metadata parsed-yaml)}))))

(comment "Loads all yaml files used for testing purposes"
  (read-file "sample_yaml/a.yaml")
  (read-file "sample_yaml/b.yaml")
  (read-file "sample_yaml/c.yaml")
  (read-file "sample_yaml/non_existant.yaml")
)

; Test properties
(def files (keys @app-db))
(def props (reduce #(assoc %1 %2 (:properties (get @app-db %2))) {} files))
(def all-property-names (reduce (fn [s p] (apply conj s (keys (second p)))) #{} props))
(def n "middle.zzz.apple")
(def properties [{:key "bottom", :ks [:bottom], :val "test", :file "sample_yaml/a.yaml"}
                 {:key "bottom", :ks [:bottom], :val "Btest", :file "sample_yaml/b.yaml"}
                 {:key "bottom", :ks [:bottom], :val "test", :file "sample_yaml/c.yaml"}])
(def equal-properties [{:key "bottom", :ks [:bottom], :val "test", :file "sample_yaml/a.yaml"}
                 {:key "bottom", :ks [:bottom], :val "test", :file "sample_yaml/b.yaml"}
                 {:key "bottom", :ks [:bottom], :val "test", :file "sample_yaml/c.yaml"}])

(defn get-properties [files] (reduce #(assoc %1 %2 (:properties (get @app-db %2))) {} files))
(defn add-file [file-name properties] (map #(assoc (second %) :file file-name) properties))
(defn group-files-by-key [files] (->> files
                              get-properties
                              (map #(add-file (first %) (second %)))
                              flatten
                              (group-by :key)
                              ))
(defn same-values? [properties n]
  (and (= (count properties) n) (apply = (map :val properties))))
(defn diff [files]
  (->> (group-files-by-key files)
       (filter (complement #(same-values? (second %) (count files))))
       (into {})))

(defn diff-all [] (diff (keys @app-db)))