(ns yaml-config-manager.core
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io])
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
(comment "Helpers for testing functions"
         (def files (keys @app-db))
         (def props (reduce #(assoc %1 %2 (:properties (get @app-db %2))) {} files))
         (def all-property-names (reduce (fn [s p] (apply conj s (keys (second p)))) #{} props))
         (def n "middle.zzz.apple")
         (def properties [{:prop "bottom", :ks [:bottom], :val "test", :file "sample_yaml/a.yaml"}
                          {:prop "bottom", :ks [:bottom], :val "Btest", :file "sample_yaml/b.yaml"}
                          {:prop "bottom", :ks [:bottom], :val "test", :file "sample_yaml/c.yaml"}])
         (def equal-properties [{:prop "bottom", :ks [:bottom], :val "test", :file "sample_yaml/a.yaml"}
                                {:prop "bottom", :ks [:bottom], :val "test", :file "sample_yaml/b.yaml"}
                                {:prop "bottom", :ks [:bottom], :val "test", :file "sample_yaml/c.yaml"}])
         )

(defn get-properties [files] (reduce #(assoc %1 %2 (:properties (get @app-db %2))) {} files))
(defn add-file [file-name properties] (map #(assoc (second %) :file file-name) properties))
(defn group-files-by-key [files] (->> files
                              get-properties
                              (map #(add-file (first %) (second %)))
                              flatten
                              (group-by :prop)
                              ))
(defn same-values? [properties n]
  (and (= (count properties) n) (apply = (map :val properties))))
(defn diff [files]
  (->> (group-files-by-key files)
       (filter (complement #(same-values? (second %) (count files))))
       (into {})
       ))

(defn diff-all [] (diff (keys @app-db)))
(comment "Diff-all easy access"
   (diff-all))

(comment "Helpers for creating generated strings"
         (def props [{:prop "top.properties.end",
                      :ks   [:top :properties :end],
                      :val  "world",
                      :file "sample_yaml/a.yaml"}
                     {:prop "top.properties.end",
                      :ks   [:top :properties :end],
                      :val  "worldC",
                      :file "sample_yaml/c.yaml"}])
         (def prop-map {"top.properties.end" {"sample_yaml/a.yaml" {:prop "top.properties.end", :ks [:top :properties :end], :val "world", :file "sample_yaml/a.yaml"}, "sample_yaml/c.yaml" {:prop "top.properties.end", :ks [:top :properties :end], :val "worldC", :file "sample_yaml/c.yaml"}}, "middle.zzz.watermelon" {"sample_yaml/a.yaml" {:prop "middle.zzz.watermelon", :ks [:middle :zzz :watermelon], :val "isWatermelon", :file "sample_yaml/a.yaml"}, "sample_yaml/c.yaml" {:prop "middle.zzz.watermelon", :ks [:middle :zzz :watermelon], :val "isWatermelonFromC", :file "sample_yaml/c.yaml"}}, "top.properties.list" {"sample_yaml/a.yaml" {:prop "top.properties.list", :ks [:top :properties :list], :val "a,b,c,d,e", :file "sample_yaml/a.yaml"}, "sample_yaml/c.yaml" {:prop "top.properties.list", :ks [:top :properties :list], :val "a,b,c,d,e,f", :file "sample_yaml/c.yaml"}}, "cat" {"sample_yaml/c.yaml" {:prop "cat", :ks [:cat], :val "cat", :file "sample_yaml/c.yaml"}}})
         )

(defn rekey-props [props]
  (reduce #(assoc %1 (:file %2) %2) {} props))
(defn generate-strings [files prop-map]
  (clojure.string/join "\n\n"
     (map
       (fn [[n props]] (str n "\n" (clojure.string/join "\n"
          (map
            (fn [f-name] (if (map? (get props f-name))
                           (str f-name "=" (get-in props [f-name :val]))
                           (str f-name "=DOES NOT EXIST"))) files)))) prop-map)))

(defn diff-to-str [files]
  (->> (diff files)
       (reduce (fn [m [k ps]] (assoc m k (rekey-props ps))) {})
       (generate-strings (sort files))
   ))

(comment "diff-to-str easy access"
         (diff-to-str (keys @app-db))
         )
