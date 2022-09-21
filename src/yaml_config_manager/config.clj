(ns yaml-config-manager.config
  (:require [flatland.ordered.map :refer (ordered-map)]
            [clojure.core.incubator :refer (dissoc-in)])
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
(defn assoc-property-data [m ks ps] "Attach :prop (springified ps), :ks ks, and :val ps to the map"
  (-> m
      (assoc :prop (to-prop-name ks))
      (assoc :ks ks)
      (assoc :val ps)))

(defn assoc-yaml-as-spring-properties [file-info]
  (let [kvs (yaml-to-kvs (:yaml file-info))]
    (assoc file-info :spring-properties
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
          {:prop p, :ks (property-to-ks p) :val nil, :full-path f}))))

(comment
  (def development-file-info {:name "serviceA.yml", :service "serviceA", :env "development", :full-path "./sample_project_configs/development/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "NPE") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled true) :featureCFlag false :featureD (ordered-map :url "featureDURL" :enabled true))})
  (def prod-file-info {:name "serviceA.yml", :service "serviceA", :env "production", :full-path "./sample_project_configs/production/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "PROD") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled false) :featureCFlag true :featureD (ordered-map :url "featureDURL" :enabled false) :featureRemoved (ordered-map :enabled true))})
  (def file-infos [development-file-info prod-file-info])
  (def changeset [{:prop "featureB.enabled", :ks [:featureB :enabled], :val true, :full-path "./sample_project_configs/development/serviceA/serviceA.yml"} {:prop "featureRemoved.enabled", :ks [:featureRemoved :enabled], :val nil, :full-path "./sample_project_configs/development/serviceA/serviceA.yml"} {:prop "featureD.enabled", :ks [:featureD :enabled], :val true, :full-path "./sample_project_configs/development/serviceA/serviceA.yml"} {:prop "featureCFlag", :ks [:featureCFlag], :val false, :full-path "./sample_project_configs/development/serviceA/serviceA.yml"} {:prop "serviceA.deploymentType", :ks [:serviceA :deploymentType], :val "NPE", :full-path "./sample_project_configs/development/serviceA/serviceA.yml"}]))

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

(defn get-changeset [from-file-info to-file-info]
  (->> (diff [from-file-info to-file-info])
       (map #(second %))
       (flatten)
       (filter #(= (:full-path from-file-info) (:full-path %)))))

(comment "Helpers for migrating properties"
  (diff file-infos)
  (get-changeset (first file-infos) (second file-infos))
)

(defn spring-to-txt [kvs] "Takes list of spring properties in format [{:prop, :ks, :val}, ...] and turns it into a string"
  (clojure.string/join "\n"
     (map (fn [prop]
            (let [{:keys [prop val]} prop]
              (str prop "=" val))) kvs)))
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

(comment "Helpers showing shape of file-info and selected-prop"
         (def development-file-info {:name "serviceA.yml", :service "serviceA", :env "development", :full-path "./sample_project_configs/development/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "NPE") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled true) :featureCFlag false :featureD (ordered-map :url "featureDURL" :enabled true))})
         (def prod-file-info {:name "serviceA.yml", :service "serviceA", :env "production", :full-path "./sample_project_configs/production/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "PROD") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled false) :featureCFlag true :featureD (ordered-map :url "featureDURL" :enabled false) :featureRemoved (ordered-map :enabled true))})
         (def file-info development-file-info)
         (def selected-prop {:prop "serviceA.deploymentType", :ks [:serviceA :deploymentType], :val "PROD", :full-path "./sample_project_configs/production/serviceA/serviceA.yml"})
         (def selected-prop {:prop "serviceA.deploymentType", :ks [:serviceA :deploymentType], :val nil, :full-path "./sample_project_configs/production/serviceA/serviceA.yml"})
         (def selected-prop-b {:prop "serviceB.deploymentType", :ks [:serviceB :deploymentType], :val "PROD-B", :full-path "./sample_project_configs/production/serviceB/serviceB.yml"})
         (def selected-props [selected-prop selected-prop-b])
         )
(defn assoc-updated-prop [file-info selected-prop] "Updates the yaml of a file-info to include the value from the selected-prop
                                                    Something for the future could be to allow functions for val (or some other way to do values based on serviceName)"
  (if (nil? (:val selected-prop))
    (dissoc-in file-info (cons :yaml (:ks selected-prop)))
    (assoc-in file-info (cons :yaml (:ks selected-prop)) (:val selected-prop))))
(defn assoc-selected-props [file-info selected-props]
    (reduce #(assoc-updated-prop %1 %2) file-info selected-props))

(comment "Helpers for spring properties apply"
         (def prop-lines (get-lines "sample_yaml/apply_from.properties"))
         (def file-info {:env "development", :service-name "serviceA", :file-name "serviceA.yml", :file-path "./sample_project_configs/development/serviceA/serviceA.yml", :env-path "./sample_project_configs/development"})
         (def data  {:featureCFlag "true", :featureD {:url "updatedURLForSpringProperties"}})
         (def data (ordered-map :a (ordered-map :a 1) :b (ordered-map :b 2)))
 )

(defn lookup-secret [word secrets]
  (let [s (clojure.string/join word)]
    (if (contains? secrets s)
      (get secrets s)
      (concat "${" s "}")))
  )

; Be careful. The into is required to avoid StackOverflowExceptions which occur when mixing lazy-seqs with concat
; https://stuartsierra.com/2015/04/26/clojure-donts-concat
; https://stackoverflow.com/questions/24958907/why-does-reduce-give-a-stackoverflowerror-in-clojure
(defn replace-secrets [s secrets] "Replaces all occurrences of ${secretName} with their matching secret when the secret exsits"
  (clojure.string/join (loop [built-s []
                              remaining-s (vec (char-array s))
                              cur-secret-s nil]
                         (if (empty? remaining-s)
                           (into built-s cur-secret-s)
                           (let [next-c (str (first remaining-s))
                                 rem (rest remaining-s)]
                             (case next-c
                               ; $ always represents ending the current secret sequence and potentially beginning another
                               "$" (recur (into built-s cur-secret-s)
                                          rem
                                          (vec "$"))
                               ; { denotes the start of a secret if it follows a $ otherwise treat as character with no special meaning
                               "{" (if (and (= 1 (count cur-secret-s))
                                            ; https://stackoverflow.com/questions/3970830/which-is-the-most-clojuresque-way-to-compare-characters-and-string-single-char
                                            (= (first cur-secret-s) \$)) ; clojure literal for $
                                     (recur built-s
                                            rem
                                            (vec "${"))
                                     (recur (into (into built-s cur-secret-s) next-c)
                                            rem
                                            nil)
                                     )
                               ; } denotes the end of a secret if the secret list is full otherwise treat as character with no special meaning
                               "}" (if (<= 2 (count cur-secret-s))
                                     (recur (into built-s (lookup-secret (drop 2 cur-secret-s) secrets))
                                            rem
                                            nil)
                                     (recur (into (into built-s cur-secret-s) next-c)
                                            rem
                                            nil)
                                     )
                               ; Other characters get appended to cur-secret if we have one started (length >= `(count ${`)
                               ; Or the built string if no cur secret exists
                               (if (<= 2 (count cur-secret-s))
                                 (recur built-s
                                        rem
                                        (into cur-secret-s next-c))
                                 (recur (into (into built-s cur-secret-s) next-c)
                                        rem
                                        nil))
                               ))))))

(comment "Helper for replace-secrets function"
  (replace-secrets s secrets)

  (def built-s (concat (seq (char-array "abc")) (seq "$")))
  (def cur-secret-s (seq (char-array "${asdf")))
  (def cur-secret-s (seq (char-array "$")))
  (def cur-secret-s (seq (char-array "${")))
  (def cur-secret-s (seq (char-array "a{b")))
  (def cur-secret-s nil)

  (def cur-secret-s nil)
  (def next-c "d")
   (def next-c (str (first (char-array "dbUser"))))

   ; Note: Need to convert char-array to vec
  (into (take 3 (char-array "${abc} a")) (take 2 (char-array "bc} a"))) ; cb${a
  (into (vec (take 3 (char-array "${abc} a"))) (take 2 (char-array "bc} a"))) ; ${abc
  (into (take 3 (char-array "${abc} a")) (reverse (take 2 (char-array "bc} a")))) ; bc${a
  (into (vec (take 3 (char-array "${abc} a"))) (flatten [(take 2 (char-array "bc} a")) (take 3 (char-array "} a"))])) ; ${abc

  (def built-s nil)
  (def rem (rest "dbUser"))

  ; String that hits all the edge cases I could think of. $ at start. $ for non-existent, $ duplicate, $ at end,
  (def s "${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser}")
  (def s "${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ${dbUser} went to the${dbPassword} and ${secretDoesNotExist} but a{b c$d e}f duplicate ${dbUser} ")
  (def s "${dbUser}")
  (def s "dbUser")
  (def secrets {"dbUser" "DB-USER-SUCCESS", "dbPassword" "DB-PASSWORD-SUCCESS"})
  (replace-secrets s secrets)
)

(defn yaml-to-spring-properties-with-secrets [file-info secrets]
  "Takes a file-info and returns a string version of the spring properties with secrets filled in."
  (replace-secrets (spring-to-txt (:spring-properties (assoc-yaml-as-spring-properties file-info)))
                   secrets))

; Idea from stackoverflow (can't link due to lack of internet on laptop)
; String reader. Output list, when you see pattern ${ move to lookup map and replace
(clojure.string/join (seq (char-array "Hello world ${name} ${name} you rock! ${power} ^ 2")))
(concat (seq (char-array "ABC")) "d")

(comment "Helpers for yaml to spring properties with secrets"
  ; Has 3 secrets. ${dbUser}, ${dbPassword}, and ${doesNotExist}
  (def spring-prop {:prop "database.username", :ks [:database :username], :val "${dbUser}"})
  (def spring-prop {:prop "database.username", :ks [:database :username], :val "$(dbPassword}"})
  (def spring-prop {:prop "database.username", :ks [:database :username], :val "${dbUser}$(dbPassword}"})
  (def spring-prop {:prop "database.username", :ks [:database :username], :val "${doesNotExist}"})
  (def spring-prop {:prop "database.username", :ks [:database :username], :val "aaaaa"})
  (def spring-prop {:prop "database.username", :ks [:database :username], :val true})
  (def spring-prop {:prop "database.username", :ks [:database :username], :val nil})
  (def file-info {:name "serviceA.yml", :service "serviceA", :env "development", :full-path "./sample_project_configs/development/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "${dbUser}" :password "${dbPassword}" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "${doesNotExist}" :deploymentType "NPE") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled true) :featureCFlag false :featureD (ordered-map :url "featureDURL" :enabled true))})
)