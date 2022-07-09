(ns yaml-config-manager.config
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io])
  )

(use 'yaml-config-manager.config :reload-all)
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
       (remove #(nil? (:val %)))))

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
     terms)))

(comment "Turns a parsed yaml map into a format which lists all keys next to the value"
  (to-properties a-parsed)
  (to-properties b-parsed)
  (to-properties c-parsed)
  (to-properties {:a {:b 2 :c {:d 3 :e 4}}})
)

(defn to-prop-name [ks] "Takes a key structure. Returns the spring properties that would be associated with it"
  (clojure.string/join "." (map name ks)))
(defn assoc-property-data [m ks ps]
  (-> m
      (assoc :prop (to-prop-name ks))
      (assoc :ks ks)
      (assoc :val ps)))
(defn convert-to-properties [yaml]
  (map (fn [[ks ps]]
         (if (nil? (first ks))
           nil
           (assoc-property-data {} ks ps)))
       (to-properties yaml)))

(comment [] "Helpers for debugging convert-to-properties"
  (convert-to-properties a-parsed)
  (convert-to-properties b-parsed)
  (convert-to-properties c-parsed)
)

(defn update-yaml! [f yaml] "Updates app-db with the yaml map"
  (swap! app-db assoc f yaml))
(defn read-file! [f] "Reads a file and saves it to app-db"
  (if (.exists (io/file f))
      (update-yaml! f (yaml/parse-string (slurp f)))))
(defn has-file? [f] (contains? @app-db f))

(comment "Loads all yaml files used for testing purposes"
  (read-file! "sample_yaml/a.yaml")
  (read-file! "sample_yaml/b.yaml")
  (read-file! "sample_yaml/c.yaml")
  (read-file! "sample_yaml/non_existant.yaml")
)

(defn get-props [files] (->> (select-keys @app-db files)
                             (map (fn [[f props]] (map #(assoc % :file f) (convert-to-properties props))))
                             (flatten)))
(defn fill-missing-props [files props] "Attaches all properties to all files with missing properties deafulting to nil"
  (let [props-set (into #{} (map :prop props))
        files-set (into #{} files)
        lookup-table (reduce #(assoc-in %1 [(:prop %2) (:file %2)] %2) {} props)]
    (for [p props-set
          f files-set]
      (if (get-in lookup-table [p f])
        (get-in lookup-table [p f])
        {:prop p, :ks (property-to-ks p) :val nil, :file f}))))
(defn diff [files]
  (->> (get-props files)
       (fill-missing-props files)
       (group-by :prop)
       (remove #(apply = (map :val (second %))))
       (into {})))
(defn get-changeset [from-file to-file properties-to-check]
  (->> (diff [from-file to-file])
       (filter (fn [[k _]] (contains? properties-to-check k)))
       (map #(second %))
       (flatten)
       (filter #(= from-file (:file %)))))

(defn diff-all [] (diff (keys @app-db)))

(defn diff-to-txt [prop-diff]
  (clojure.string/join "\n\n"
     (map
       (fn [[n props]] (str n "\n" (clojure.string/join "\n"
          (map
            (fn [prop] (if (map? prop)
                           (str (:file prop) "=" (:val prop))
                           (str (:file prop) "=DOES NOT EXIST"))) props)))) prop-diff)))
(defn readable-diff [files]
    (diff-to-txt (diff files)))

(def from-file "sample_yaml/a.yaml")
(def to-file "sample_yaml/c.yaml")
(def properties-to-check (set (keys (diff [from-file to-file]))))
(get-changeset from-file to-file properties-to-check)

(defn apply-diff
  ([from-file to-file] "Default is apply everything that has changed" (apply-diff from-file to-file (set (keys (diff [from-file to-file])))))
  ([from-file to-file properties]
   (let [changed-properties (get-changeset from-file to-file properties)
         orig-yaml (get @app-db to-file)]
         (reduce #(do (prn %2 " " (:val %2) " " (nil? (:val %2)))  (if (nil? (:val %2))
                             (dissoc %1 (apply identity (:ks %2)))
                             (assoc-in %1 (:ks %2) (:val %2)))) orig-yaml changed-properties))))
(defn apply-diff!
  ([from-file to-file] "Default is apply everything that has changed" (apply-diff! from-file to-file (set (keys (diff [from-file to-file])))))
  ([from-file to-file properties]
   (let [yaml (apply-diff from-file to-file properties)]
     (do
       (update-yaml! to-file yaml)
       (get @app-db to-file)))))

(comment "apply-diff test helper"
         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (readable-diff ["sample_yaml/c.yaml" "sample_yaml/a.yaml"])
         (apply-diff! "sample_yaml/c.yaml" "sample_yaml/a.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (readable-diff ["sample_yaml/a.yaml" "sample_yaml/c.yaml"])
         (apply-diff! "sample_yaml/a.yaml" "sample_yaml/c.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (readable-diff ["sample_yaml/a.yaml" "sample_yaml/c.yaml"])
         (apply-diff "sample_yaml/a.yaml" "sample_yaml/c.yaml")
         (apply-diff "sample_yaml/c.yaml" "sample_yaml/a.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/b.yaml")
         (readable-diff ["sample_yaml/a.yaml" "sample_yaml/b.yaml"])
         (apply-diff! "sample_yaml/b.yaml" "sample_yaml/a.yaml")

         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/b.yaml")
         (readable-diff ["sample_yaml/b.yaml" "sample_yaml/a.yaml"])
         (apply-diff "sample_yaml/a.yaml" "sample_yaml/b.yaml")
         (apply-diff "sample_yaml/b.yaml" "sample_yaml/a.yaml")
)

; Assumption: Properties files will never be loaded into app-db, so we'll use slurp
(defn apply-properties [prop-lines to-file]
  (let [orig-yaml (if (contains? @app-db to-file)
                        (get @app-db to-file)
                        {})]
    (reduce #(assoc-in %1 (:ks %2) (:val %2)) orig-yaml (properties-to-kvs prop-lines))))
(defn apply-properties! [prop-lines to-file]
   (let [yaml (apply-properties prop-lines to-file)]
     (do
       (update-yaml! to-file yaml)
       (get @app-db to-file))))

(comment "Helpers for spring properties apply"
         (read-file! "sample_yaml/a.yaml")
         (read-file! "sample_yaml/c.yaml")
         (def prop-lines (get-lines "sample_yaml/apply_from.properties"))
         (def to-file "sample_yaml/c.yaml")
         (def orig-yaml (if (contains? @app-db to-file)
                          (get @app-db to-file)
                          {}))
         (apply-properties prop-lines to-file)
         (apply-properties! prop-lines to-file)
 )