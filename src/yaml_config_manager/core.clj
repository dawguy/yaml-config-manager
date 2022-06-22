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
(defn reset-db! [] (reset! app-db {}))

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

(defn attach-metadata [mapped-yaml]
  (let [props (to-properties mapped-yaml)]
    (reduce (fn [m [ks ps]]
              (if (nil? (first ks))
                m
                (assoc m (to-prop-name ks) {:prop (to-prop-name ks), :ks ks, :val ps}))
              ) {} props)
    )
  )

(comment [] "Helpers for debugging attach-metadata"
  (attach-metadata a-parsed)
  (attach-metadata b-parsed)
  (attach-metadata c-parsed)
)

(defn update-yaml! [f yaml mapped-yaml]
  (swap! app-db assoc f {:yaml yaml, :mapped-yaml mapped-yaml, :properties (attach-metadata mapped-yaml)}))
(defn read-file! [f]
  (if (.exists (io/file f))
    (let [yaml (slurp f)
          mapped-yaml (yaml/parse-string yaml)]
      (update-yaml! f yaml mapped-yaml))))

(comment "Loads all yaml files used for testing purposes"
  (read-file! "sample_yaml/a.yaml")
  (read-file! "sample_yaml/b.yaml")
  (read-file! "sample_yaml/c.yaml")
  (read-file! "sample_yaml/non_existant.yaml")
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
(defn key-by-file-names [prop-map]
  (reduce (fn [m [k ps]] (assoc m k (rekey-props ps))) {} prop-map)
  )
(defn diff-to-str [files]
  (->> (diff files)
       (key-by-file-names)
       (generate-strings (sort files))
   ))

(comment "diff-to-str easy access"
         (diff-to-str (keys @app-db))
         )

(defn get-changed-properties [from-file to-file properties]
  (->> (diff [from-file to-file])
       (filter (fn [[k ps]] (contains? properties k)))
       (into {})
       (key-by-file-names)
       (vals)
       (map #(get % from-file))
       (filter (complement nil?))
       ))

(defn apply-diff
  ([from-file to-file] "Default is apply everything that has changed" (apply-diff from-file to-file (set (keys (diff [from-file to-file])))))
  ([from-file to-file properties]
   (let [changed-properties (get-changed-properties from-file to-file properties)
         orig-yaml (:mapped-yaml (get @app-db to-file))]
         (reduce #(assoc-in %1 (:ks %2) (:val %2)) orig-yaml changed-properties) ; TODO: This could probably be refactored out and commonalized so allvariations of properties can use it
     )))
(defn apply-diff!
  ([from-file to-file] "Default is apply everything that has changed" (apply-diff! from-file to-file (set (keys (diff [from-file to-file])))))
  ([from-file to-file properties]
   (let [yaml (apply-diff from-file to-file properties)]
     (do
       (update-yaml! to-file (yaml/generate-string yaml) yaml)
       (:yaml (get @app-db to-file))))
   ))

(comment "apply-diff test helper"
         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (apply-diff! "sample_yaml/c.yaml" "sample_yaml/a.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (apply-diff! "sample_yaml/a.yaml" "sample_yaml/c.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (apply-diff "sample_yaml/a.yaml" "sample_yaml/c.yaml")
         (apply-diff "sample_yaml/c.yaml" "sample_yaml/a.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/b.yaml")
         (apply-diff! "sample_yaml/b.yaml" "sample_yaml/a.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/b.yaml")
         (apply-diff "sample_yaml/a.yaml" "sample_yaml/b.yaml")
         (apply-diff "sample_yaml/b.yaml" "sample_yaml/a.yaml")
)

; https://stackoverflow.com/questions/9047231/read-a-file-into-a-list-each-element-represents-one-line-of-the-file
(defn get-lines [file]
  (clojure.string/split-lines (slurp file))
  )
(defn property-to-ks [s]
  (if (string? s)
    (mapv #(keyword %) (clojure.string/split s #"\."))
    nil))
(defn property-to-kv [s]
  (if (string? s)
    (let
      [i (clojure.string/index-of s "=")
       [k v] (if (nil? i) [s nil] [(subs s 0 i) (subs s (inc i))])]
      {:key k :ks (property-to-ks k) :val v})
    nil))
(defn properties-to-kvs [props] "Turns spring style properties into a format where they can be applied to a yaml map"
  (->> props
       (map property-to-kv)
       (filter #((complement nil?) (:val %)))))

; Assumption: Properties files will never be loaded into app-db, so we'll use slurp
(defn apply-properties [prop-lines to-file]
  (let [orig-yaml (if (contains? @app-db to-file)
                        (:mapped-yaml (get @app-db to-file))
                        {})]
    (reduce #(assoc-in %1 (:ks %2) (:val %2)) orig-yaml (properties-to-kvs prop-lines))

    (assoc-in orig-yaml (:ks (first (properties-to-kvs prop-lines))) "TESTABC")
    (assoc-in orig-yaml [:top :properties :abc] "TESTABC")
    ))
(defn apply-properties! [prop-lines to-file]
   (let [yaml (apply-properties prop-lines to-file)]
     (do
       (update-yaml! to-file (yaml/generate-string yaml) yaml)
       (:yaml (get @app-db to-file))))
   )

(comment "Helpers for spring properties apply"
         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (def prop-lines (get-lines "sample_yaml/apply_from.properties"))
         (def to-file "sample_yaml/c.yaml")
         (def orig-yaml (if (contains? @app-db to-file)
                          (:mapped-yaml (get @app-db to-file))
                          {}))
         (apply-properties prop-lines to-file)
         (apply-properties! prop-lines to-file)
 )
