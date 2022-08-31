(ns yaml-config-manager.manager
  (:require [clj-yaml.core :as yaml]
            [clojure.tools.namespace.file :as ns-file]
            [yaml-config-manager.config :as config]
            [clojure.java.io :as io]
            [cheshire.core :as json])

  (:import (java.io File))
  )

; This one is gonna be a bit specific to our sample_project_configs directory, but that's okay
(use 'yaml-config-manager.manager :reload-all)
(def from-file-infos (atom {}))
(def to-file-infos (atom {}))

(comment "Redefine helper atoms into def'd vars"
  (def from-file-infos (deref from-file-infos))
  (def to-file-infos (deref to-file-infos))
)

(defonce app-db (atom {}))
(def target-dir "./sample_project_configs")

(defn read-file [f] "Reads a yaml file"
  (if (.exists f)
    (yaml/parse-string (slurp f))))
(defn write-file! [file-name data] (spit file-name (yaml/generate-string data :dumper-options {:flow-style :block
                                                                                               :indent 2})))
(defn save-file-info! [file-info]
  (write-file! (:full-path file-info) (:yaml file-info)))

(defn assoc-file-path-info [m f]
  (let [service-f (.getParentFile f)
        env-f (if (nil? f) nil (.getParentFile service-f))]
    (-> m
        (assoc :name (.getName f))
        (assoc :service (if (nil? service-f) nil (.getName service-f)))
        (assoc :env (if (nil? env-f) nil (.getName env-f)))
        (assoc :full-path (.getPath f))
        (assoc :f f)
        (assoc :exists (.exists f)))))
(defn assoc-yaml-info [m f]
  (let [yaml (read-file f)]
    (-> m
      (assoc :yaml yaml))))

(defn is-yaml? [f] (ns-file/file-with-extension? f ["yaml" "yml"]))

(defn assoc-to-db [db file-info] "Assocs the file-info multiple places in @app-db"
  (-> db
      (assoc-in [:files (:name file-info) (:env file-info)] file-info)
      (assoc-in [:environments (:env file-info) (:name file-info)] file-info)
      (assoc-in [:services (:service file-info) (:env file-info) (:name file-info)] file-info)
      (assoc-in [:paths (:full-path file-info)] file-info)))

(comment "helpers for parsing files into app-db"
  (def f (io/file "sample_yaml/a.yaml"))
  (def f (io/file (str target-dir "development/serviceA/serviceA.yml")))
  (load-file! f)
)
(defn load-file!
  [^File f] "Loads a file based on file name and adds info for them"
  (let [file-info (-> {}
                      (assoc-file-path-info f)
                      (assoc-yaml-info f))]
    (if (:exists file-info)
      (swap! app-db assoc-to-db file-info)
      file-info)))
(defn find-yaml-files [target-dir]
  (->> (clojure.java.io/file target-dir)
      (file-seq)
      (filter #(is-yaml? %))
    ))
(defn load-files!
  ([] (load-files! target-dir))
  ([target-dir]
   (let [yaml-files (find-yaml-files target-dir)]
     (vec (for [f yaml-files] ; Note: vec is required to realize the lazy-seq for side-effects
            (load-file! f))))))

(comment "Helpers for developing diff and diff-to-txt"
         (diff "staging" "production" "serviceA.yml")
         (diff-to-txt "staging" "production" "serviceA.yml")
)
(defn diff [env-a env-b f-name]
  (let [f-a (get-in @app-db [:environments env-a f-name])
        f-b (get-in @app-db [:environments env-b f-name])]
  ; Return the diff of the files
  (config/diff [f-a f-b])))
(defn diff-to-txt [env-a env-b f-name]
  (config/diff-to-txt (diff env-a env-b f-name)))

(defn json-to-properties [props]
  (mapv #(str (first %) "=" (second %)) props)
  )
(defn- kv-to-spring-properties [body]
  (condp #(contains? %2 %1) body
    "propertiesText" (filterv not-empty (clojure.string/split (get body "propertiesText") #"\n"))
    "properties" (json-to-properties (json/parse-string (get body "properties")))
    []))

(defn assoc-file-info [m]
  (-> {}
      (assoc :env (get m "env"))
      (assoc :service-name (get m "serviceName"))
      (assoc :file-name (get m "fileName"))
      (#(assoc % :file-path (str target-dir "/" (:env %) "/" (:service-name %) "/" (:file-name %))))
      (#(assoc % :env-path (str target-dir "/" (:env %)))))
)
(defn assoc-file-paths [body]
  (do
    (cond
      (contains? body "paths") (map assoc-file-info (get body "paths"))
      (and (contains? body "to") (contains? body "from")) [(assoc-file-info (get body "from" {}))
                                                           (assoc-file-info (get body "to" {}))]
      :else (assoc-file-info body))))

(defn group-env-file-infos [from-file-infos to-file-infos]
  (loop [rem from-file-infos
         to-file-infos-lookup (group-by :name to-file-infos)
         file-infos []]
    (if (empty? rem)
      file-infos
      (recur (rest rem)
             to-file-infos-lookup
             (conj file-infos [(first rem) (first (get to-file-infos-lookup (:name (first rem))))])))))

(comment "Group env file infos helper"
  (first from-file-infos)
  (group-env-file-infos from-file-infos to-file-infos)
)

(comment "Helper defs applying properties via postman"
  (def service-name "serviceA")
  (def env "development")
  (def body {"propertiesText" "\nfeatureCFlag=true\nfeatureD.url=updatedURLForSpringProperties\n", "env" "development", "serviceName" "serviceA", "fileName" "serviceA.yml"})
  (def body {"properties" "{ \"featureCFlag\": true, \"featureD.url\": \"updatedURLForSpringPropertiesAA\" }", "env" "development", "serviceName" "serviceA", "fileName" "serviceA.yml"})
  (def props {"featureCFlag" true
              "featureD.url" "updatedURLForSpringProperties"})
  (def selected-props (map config/property-to-kv (kv-to-spring-properties body)))
  (def file-info (get-in @app-db [:paths "./sample_project_configs/development/serviceA/serviceA.yml"]))
  (def info (assoc-file-paths body))
)
; Format is target_dir/<env>/<service>/<file>.yml
(defn apply-properties-file [body]
  (let [selected-props (map config/property-to-kv (kv-to-spring-properties body))
        body-parsed (assoc-file-paths body)
        file-info (get-in @app-db [:paths (:file-path body-parsed)])]
      (config/assoc-selected-props file-info selected-props)
    ))

(comment "Helpers for grabbing all files from a particular environment"
         (def body {"propertiesText" "\nfeatureCFlag=AAAAAAAAA\nfeatureD.url=updatedURLForSpringProperties\n", "env" "development"})
         (def b body)
         (def selected-props (map config/property-to-kv (kv-to-spring-properties body)))
         (def body-parsed (assoc-file-paths body))
)
(defn apply-properties-env [body]
  (let [selected-props (map config/property-to-kv (kv-to-spring-properties body))
        body-parsed (assoc-file-paths body)
        file-infos (vals (get-in @app-db [:environments (:env body-parsed)]))]
    (map #(config/assoc-selected-props % selected-props) file-infos)))
(defn migrate-properties-file [body]
  (let [body-parsed (assoc-file-paths body)
        from-file-info (get-in @app-db [:paths (:file-path (first body-parsed))])
        to-file-info (get-in @app-db [:paths (:file-path (second body-parsed))])]
    (config/assoc-selected-props to-file-info (config/get-changeset from-file-info to-file-info))))
(defn migrate-properties-env [body]
  (let [body-parsed (assoc-file-paths body)
        from-file-infos (vals (get-in @app-db [:environments (:env (first body-parsed))]))
        to-file-infos (vals (get-in @app-db [:environments (:env (second body-parsed))]))]
    ;(reset! yaml-config-manager.manager/from-file-infos from-file-infos)
    ;(reset! yaml-config-manager.manager/to-file-infos to-file-infos)
    (mapv identity (for [[from-file-info to-file-info] (group-env-file-infos from-file-infos to-file-infos)]
                (config/assoc-selected-props to-file-info (config/get-changeset from-file-info to-file-info))))))

(load-files!)

(comment "Helper values for development of file reading features"
  (def body {"fromFile" {"env" "development", "fileName" "serviceA.yml", "serviceName" "serviceA"},
             "toFile" {"env" "staging", "fileName" "serviceA.yml", "serviceName" "serviceA"}})
  (def target-dir ".")
  (def target-dir "./sample_project_configs")
  (def m {:TMP "val"})
)