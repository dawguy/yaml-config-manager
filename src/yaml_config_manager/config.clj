(ns yaml-config-manager.config
  (:require [flatland.ordered.map :refer (ordered-map)])
)

(use 'yaml-config-manager.config :reload-all)

; https://stackoverflow.com/questions/9047231/read-a-file-into-a-list-each-element-represents-one-line-of-the-file
(defn get-lines [file] (clojure.string/split-lines (slurp file)))
(defn property-to-ks [s] "Transforms a spring boot property such as otherService.refresh.url into a list of key-values
                          used by yaml in the form of [otherService refresh url]"
  (if (string? s)
    (mapv #(keyword %) (clojure.string/split s #"\."))
    nil))
(defn property-to-kv [s] "Parses a spring boot property as a string of format `otherService.refresh.url=127.0.0.1:9001`
                          into the value, key, and assocable list ks"
  (if (string? s)
    (let
      [i (clojure.string/index-of s "=")
       [k v] (if (nil? i)
               [s nil]
               [(subs s 0 i) (subs s (inc i))])]
      {:key k :ks (property-to-ks k) :val v})
    nil))

(defn yaml-to-kvs
  ([m] "Takes the root map and turns it into a vector of format [[kvs prop-value] [kvs prop-value] [kvs prop-value]]"
    (yaml-to-kvs [] [] m))
  ([path terms m] "Helper function for recursively generating key value paths [kvs]"
   (if (map? m)
     (reduce #(apply conj %1 ((fn [k]
                                (if (map? (get m k))
                                  (yaml-to-kvs (conj path k) terms (get m k))
                                  [[(conj path k) (get m k)]]
                                  )
                                ) %2)) [] (keys m))
       terms)))

(defn to-prop-name [ks] "Takes a key structure. Returns the spring properties that would be associated with it"
  (clojure.string/join "." (map name ks)))
(defn assoc-property-data [m ks ps]
  (-> m
      (assoc :prop (to-prop-name ks))
      (assoc :ks ks)
      (assoc :val ps)))

(defn assoc-yaml-as-spring-properties [m]
  (let [kvs (yaml-to-kvs (:yaml m))]
    (assoc m :spring-properties
      (map (fn [[ks ps]]
             (if (nil? (first ks))
               nil
               (assoc-property-data {} ks ps)))
           kvs))))

(defn fill-missing-props [props] "Attaches all properties to all files with missing properties deafulting to nil"
  (let [props-set (into #{} (map :prop props))
        files-set (into #{} (map :full-path props))
        lookup-table (reduce #(assoc-in %1 [(:prop %2) (:full-path %2)] %2) {} props)]
    (for [p props-set
          f files-set]
      (if (get-in lookup-table [p f])
        (get-in lookup-table [p f])
        (do
          (prn "NILLL")
          {:prop p, :ks (property-to-ks p) :val nil, :full-path f})))))

(comment
  (def development-file-info {:name "serviceA.yml", :service "serviceA", :env "development", :full-path "./sample_project_configs/development/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "NPE") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled true) :featureCFlag false :featureD (ordered-map :url "featureDURL" :enabled true))})
  (def prod-file-info {:name "serviceA.yml", :service "serviceA", :env "production", :full-path "./sample_project_configs/production/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "PROD") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled false) :featureCFlag true :featureD (ordered-map :url "featureDURL" :enabled false) :featureRemoved (ordered-map :enabled true))})
  (def file-infos [development-file-info prod-file-info])
)

(defn diff [file-infos] "Takes in file-infos of format {:full-path, :yaml}, compares the yaml files,
                         and returns a list of all properties that are different in the format
                         {springProperty.key [{:full-path, :val, :ks}, {:full-path, :val, :ks}]
                          springProperty.key [{:full-path, :val, :ks}, {:full-path, :val, :ks}]}. Note that missing
                         properties are treated as nil"
    (->> file-infos
         (map assoc-yaml-as-spring-properties)
         (map (fn [file-info] (map #(assoc % :full-path (:full-path file-info)) (:spring-properties file-info))))
         (flatten)
         (fill-missing-props)
         (group-by :prop)
         (remove #(apply = (map :val (second %))))
         (into {})))

(defn get-changeset [from-file to-file properties-to-check]
  (->> (diff [from-file to-file])
       (filter (fn [[k _]] (contains? properties-to-check k)))
       (map #(second %))
       (flatten)
       (filter #(= from-file (:full-path %)))))

(defn diff-to-txt [prop-diff]
  (clojure.string/join "\n\n"
     (map
       (fn [[n props]] (str n "\n" (clojure.string/join "\n"
          (map
            (fn [prop] (if (map? prop)
                           (str (:full-path prop) "=" (:val prop))
                           (str (:full-path prop) "=DOES NOT EXIST"))) props)))) prop-diff)))
(defn readable-diff [file-infos]
    (diff-to-txt (diff file-infos)))

(comment [] "Helpers for debugging convert-to-properties"
         (assoc-yaml-as-spring-properties a-parsed)
         (assoc-yaml-as-spring-properties b-parsed)
         (assoc-yaml-as-spring-properties c-parsed)
         )

(comment "Helpers for spring properties apply"
         (def prop-lines (get-lines "sample_yaml/apply_from.properties"))
         (def file-info {:env "development", :service-name "serviceA", :file-name "serviceA.yml", :file-path "./sample_project_configs/development/serviceA/serviceA.yml", :env-path "./sample_project_configs/development"})
         (def data  {:featureCFlag "true", :featureD {:url "updatedURLForSpringProperties"}})
         (def data (ordered-map :a (ordered-map :a 1) :b (ordered-map :b 2)))
 )

(comment "Turns a parsed yaml map into a format which lists all keys next to the value"
         (yaml-to-kvs a-parsed)
         (yaml-to-kvs b-parsed)
         (yaml-to-kvs c-parsed)
         (yaml-to-kvs {:a {:b 2 :c {:d 3 :e 4}}})
         )