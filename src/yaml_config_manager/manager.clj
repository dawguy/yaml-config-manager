(ns yaml-config-manager.manager
  (:require [clojure.tools.namespace.file :as ns-file]
             [yaml-config-manager.config :as config]
            [cheshire.core :as json]))

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

;(load-files! target-dir)                                    ; LOADS EVERYTHING INTO APP-DB

(def env-a "staging")
(def env-b "production")
(def f-name "serviceA.yml")
(def f-a (get-in @app-db [:environments env-a f-name]))
(def f-b (get-in @app-db [:environments env-b f-name]))

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

(comment "Helpers for developing diff and diff-to-txt"
         (diff "staging" "production" "serviceA.yml")
         (diff-to-txt "staging" "production" "serviceA.yml"))

(defn select-diffs [diffs] nil)                             ; Goal for this is to create a process which can manually select via CLI or other method.
(defn select-diffs-with-pred [diffs pred])                  ; Goal for this is to allow a predicate to be ran across all prop-maps, and the first matching prop is returned. The most common pred should be based on environement
(defn apply-diff [from-env to-env f-name]
  (let [diffs (diff env-a env-b f-name)
        selected-options ()                                 ; Gaol
        ]

  ))

(defn json-to-properties [props]
  (mapv #(str (first %) "=" (second %)) props)
  )
(defn to-property-lines [body]
  (if (contains? body "propertiesText")
    (filterv not-empty (clojure.string/split (get body "propertiesText") #"\n"))
    (if (contains? body "properties")
      (json-to-properties (json/parse-string (get body "properties")))
      [])))

(defn target-info [body]
  (-> {}
      (assoc :env (get body "env"))
      (assoc :service-name (get body "serviceName"))
      (assoc :file-name (get body "fileName"))
      (#(assoc % :file-path (str target-dir "/" (:env %) "/" (:service-name %) "/" (:file-name %))))
      (#(assoc % :env-path (str target-dir "/" (:env %))))))

(comment "Helper defs for development"
   (def service-name "serviceA")
   (def env "development")
   (def body {"propertiesText" "\nfeatureCFlag=true\nfeatureD.url=updatedURLForSpringProperties\n", "env" "development", "serviceName" "serviceA", "fileName" "serviceA.yml"})
   (def body {"properties" "{ \"featureCFlag\": true, \"featureD.url\": \"updatedURLForSpringProperties\" }", "env" "development", "serviceName" "serviceA", "fileName" "serviceA.yml"})
   (def props {"featureCFlag" true
               "featureD.url" "updatedURLForSpringProperties"})
   (def property-lines (to-property-lines body))
   (def info (target-info body))
)

; Format is target_dir/<env>/<service>/<file>.yml
(defn unload-files [_] (reset! app-db {}))
(defn load-f [body]
  (let [info (target-info body)]
    (do (load-files! (:file-path info))
        (let [f (get-in @app-db [:files (:file-name info) (:env info)])]
          (dissoc f :f)))))
(defn load-env [body]
  (do (load-files! (str target-dir "/" (get body "env")))
      (keys (get @app-db :files))))
(defn apply-properties-file [body]
  (let [property-lines (to-property-lines body)
        info (target-info body)]
    (do
      (prn property-lines)
      (prn info)
      ;(config/apply-properties! property-lines (:file-path info))) ; TODO - Need to update apply-properties! to use info instead of file-path
    )))
(defn apply-properties-env [body] (prn body))
(defn migrate-properties-file [body] (prn body))
(defn migrate-properties-env [body] (prn body))