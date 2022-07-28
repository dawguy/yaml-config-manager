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

(defonce app-db (atom {}))
(def target-dir "./sample_project_configs/")

(defn read-file [f] "Reads a yaml file"
  (if (.exists f)
    (yaml/parse-string (slurp f))))
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
  (assoc m :yaml (read-file f)))

(defn is-yaml? [f] (ns-file/file-with-extension? f ["yaml" "yml"]))

(defn assoc-to-db [db file-info]
  (-> db
      (assoc-in [:files (:name file-info) (:env file-info)] file-info)
      (assoc-in [:environments (:env file-info) (:name file-info)] file-info)
      (assoc-in [:services (:service file-info) (:env file-info) (:name file-info)] file-info)
      (assoc-in [:paths (:full-path file-info)] file-info)))
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
(defn load-files! []
  (let [yaml-files (find-yaml-files target-dir)]
    (for [f yaml-files]
      (load-file! f))))

(defn diff [env-a env-b f-name]
  (let [f-a (get-in @app-db [:environments env-a f-name])
        f-b (get-in @app-db [:environments env-b f-name])]
  ; Read the files
  (do
    (if (complement (config/has-file? (:full-path f-a))) (config/read-file! (:full-path f-a)))
    (if (complement (config/has-file? (:full-path f-b))) (config/read-file! (:full-path f-b)))
  )
  ; Return the diff of the files
  (config/diff [(:full-path f-a) (:full-path f-b)])))
(defn diff-to-txt [env-a env-b f-name]
  (config/diff-to-txt (diff env-a env-b f-name)))

(defn json-to-properties [props]
  (mapv #(str (first %) "=" (second %)) props)
  )
(defn kv-to-spring-properties [body]
  (condp #(contains? %2 %1) body
    "propertiesText" (filterv not-empty (clojure.string/split (get body "propertiesText") #"\n"))
    "properties" (json-to-properties (json/parse-string (get body "properties")))
    []))

(defn assoc-file-paths [body]
  (-> {}
      (assoc :env (get body "env"))
      (assoc :service-name (get body "serviceName"))
      (assoc :file-name (get body "fileName"))
      (#(assoc % :file-path (str target-dir "/" (:env %) "/" (:service-name %) "/" (:file-name %))))
      (#(assoc % :env-path (str target-dir "/" (:env %))))))

; Format is target_dir/<env>/<service>/<file>.yml
(defn apply-properties-file [body]
  (let [property-lines (kv-to-spring-properties body)
        info (assoc-file-paths body)]
    (prn (str property-lines " " info))
      (config/apply-properties! property-lines info)))
(defn apply-properties-env [body] (prn body))
(defn migrate-properties-file [body] (prn body))
(defn migrate-properties-env [body] (prn body))
(defn save-app-db [_] (let [db (deref config/app-db)]
                       (for [f db] (config/write-file! (first f) (second f)))))

(comment "helpers for parsing files into app-db"
         (def f (io/file "sample_yaml/a.yaml"))
         (def f (io/file (str target-dir "development/serviceA/serviceA.yml")))
         )
(comment "Helpers for developing diff and diff-to-txt"
         (diff "staging" "production" "serviceA.yml")
         (diff-to-txt "staging" "production" "serviceA.yml"))
(comment "Helper values for development of file reading features"
         (def target-dir ".")
         (def target-dir "./sample_project_configs/")

         (def f "./sample_project_configs/development/serviceA/serviceA.yml")
         (def f (io/file "./sample_project_configs/development/serviceA/serviceA.yml"))
         (def f (io/file "./sample_project_configs/development/MissingService/MissingService.yml"))
         (def m {:TMP "val"})
         )
(comment "Helper defs for development"
         (def service-name "serviceA")
         (def env "development")
         (def body {"propertiesText" "\nfeatureCFlag=true\nfeatureD.url=updatedURLForSpringProperties\n", "env" "development", "serviceName" "serviceA", "fileName" "serviceA.yml"})
         (def body {"properties" "{ \"featureCFlag\": true, \"featureD.url\": \"updatedURLForSpringPropertiesAA\" }", "env" "development", "serviceName" "serviceA", "fileName" "serviceA.yml"})
         (def props {"featureCFlag" true
                     "featureD.url" "updatedURLForSpringProperties"})
         (def property-lines (kv-to-spring-properties body))
         (def info (assoc-file-paths body)))