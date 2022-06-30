(ns yaml-config-manager.manager
  (:require [clojure.tools.namespace.file :as ns-file]
             [yaml-config-manager.config :as config]))

; This one is gonna be a bit specific to our sample_project_configs directory, but that's okay
(use 'yaml-config-manager.manager :reload-all)

(def target-dir ".")
(def target-dir "./sample_project_configs")
(defonce app-db (atom {}))

(defn file-path [f]
  (let [service-f (.getParentFile f)
        env-f (if (nil? f) nil (.getParentFile service-f))]
    {
     :name      (.getName f)
     :service   (if (nil? service-f) nil (.getName service-f))
     :env       (if (nil? env-f) nil (.getName env-f))
     :full-path (.getPath f)
     :f f
     }))
(defn is-yaml? [f] (ns-file/file-with-extension? f ["yaml" "yml"]))
(defn mapped-files [fs] (map file-path (filter #(is-yaml? %) fs)))

(defn load-files! [target-dir]
  (reset! app-db (reduce (fn [m s]
                          (-> m
                              (assoc-in [:files (:name s) (:env s)] s)
                              (assoc-in [:environments (:env s) (:name s)] s)
                              (assoc-in [:services (:service s) (:env s) (:name s)] s)
                              )) {} (mapped-files (file-seq (clojure.java.io/file target-dir))))))

(load-files! target-dir)

(def env-a "staging")
(def env-b "production")
(def f-name "serviceA.yml")
(defn diff [env-a env-b f-name]
  (let [f-a (get-in @app-db [:environments env-a f-name])
        f-b (get-in @app-db [:environments env-b f-name])]
  ; Read the files
  (do
    (config/read-file! (:full-path f-a))
    (config/read-file! (:full-path f-b)))
  ; Return the diff of the files
  (config/diff-to-txt (config/diff [f-a f-b]))
  ))
(diff "staging" "production" "serviceA.yml")