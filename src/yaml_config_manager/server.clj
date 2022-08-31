(ns yaml-config-manager.server
  (:use [ring.adapter.jetty])
  (:require [clojure.data.json]
            [ring.util.response :as r]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [yaml-config-manager.manager :as m]
            [flatland.ordered.map :refer (ordered-map)]
            ))

(use 'yaml-config-manager.server :reload-all)

(def re (atom {}))
(def re-body (atom {}))
(def re-res (atom {}))
(def server (atom nil))

(defn apply-rem-wrappers [m wrappers]
  (if (empty? wrappers)
    m
    ((first wrappers) m (rest wrappers))))
(defn wrapper-force-success [file-info wrappers]
  (apply-rem-wrappers {:code 200, :message "Success"} wrappers))
(defn wrapper-apply-tar-fun [tar-fun]
  (fn [m wrappers] (apply-rem-wrappers (tar-fun m) wrappers))
  )
(defn wrapper-remove-f [file-info wrappers]
  (apply-rem-wrappers (dissoc file-info :f) wrappers))
(defn wrapper-save-file [file-info wrappers]
  (apply-rem-wrappers (do (m/save-file-info! file-info) file-info) wrappers))
; https://stackoverflow.com/questions/4019249/clojure-finding-out-if-a-collection-is-seq-able
(defn wrapper-handle-multiple [file-infos wrappers]
  (let [infos (if (sequential? file-infos) file-infos [file-infos])]
    (map #(apply-rem-wrappers % wrappers) infos)))
(defn wrapper-to-yaml [file-info wrappers]
  (apply-rem-wrappers (get file-info :yaml) wrappers))
(defn wrapper-include-service [file-info wrappers]
  (apply-rem-wrappers {:service (:service file-info) :yaml (wrapper-to-yaml file-info [])} wrappers))

(defn not-found [_] {:status 404 :body {:error "Endpoint not found."}})
(defn router [uri body]
(let [target (last (clojure.string/split uri #"/"))]
  (prn body)
  (case target
    "load-files" [(fn [_] (m/load-files!)) [wrapper-force-success]]
    "apply-properties-file" [m/apply-properties-file [wrapper-save-file wrapper-to-yaml]]
    "apply-properties-env" [m/apply-properties-env [wrapper-handle-multiple wrapper-save-file wrapper-include-service]]
    "migrate-properties-file" [m/migrate-properties-file [wrapper-save-file wrapper-to-yaml]]
    "migrate-properties-env" [m/migrate-properties-env [wrapper-handle-multiple wrapper-include-service]]
    [not-found []]
    )))

(comment "Helpers for the wrappers"
  (def development-file-info {:f nil, :name "serviceA.yml", :service "serviceA", :env "development", :full-path "./sample_project_configs/development/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "NPE") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled true) :featureCFlag false :featureD (ordered-map :url "featureDURL" :enabled true))})
  (def prod-file-info {:f nil, :name "serviceA.yml", :service "serviceA", :env "production", :full-path "./sample_project_configs/production/serviceA/serviceA.yml", :exists true, :yaml (ordered-map :database (ordered-map :username "databaseUser" :password "databasePassword" :connection (ordered-map :url "databaseIP,databaseIP2" :port 4567)) :serviceA (ordered-map :name "Service Alpha" :deploymentType "PROD") :featureA (ordered-map :enabled true) :featureB (ordered-map :url "featureBURL" :enabled false) :featureCFlag true :featureD (ordered-map :url "featureDURL" :enabled false) :featureRemoved (ordered-map :enabled true))})
  (def file-info development-file-info)
  (def file-infos [development-file-info prod-file-info])
  (def wrappers [wrapper-remove-f])
  (def wrappers [wrapper-handle-multiple wrapper-remove-f])
  (def wrappers [wrapper-remove-f wrapper-to-yaml])
  (def wrappers [wrapper-handle-multiple wrapper-remove-f wrapper-to-yaml])
  (apply-rem-wrappers file-info wrappers)
  (apply-rem-wrappers file-infos wrappers)
)

(defn handler [request]
  (do
    (reset! re request)
    (reset! re-body (:body request))
    (let [[tar-func router-wrappers] (router (:uri request) (:body request))]
      (if (= tar-func not-found)
        (tar-func {})
        (let [res (apply-rem-wrappers (tar-func (:body request)) router-wrappers)]
          (do
            (reset! re-res res)
            {:status (= tar-func 200)
             :body   {:body res}}))))))

(defn header-adder [handler]
  (fn [request]
    (r/header (handler request) "Content-Type" "application/json")))

(defn app [request]
  ((-> handler
       header-adder
       wrap-json-body
       wrap-json-response
       ) request))

(comment "Helper for app"
  (def request {:body "Hello!" :uri "asdfasdf"})
)

(defn h [request]
  (app request))

(defn start []
  (reset! server (run-jetty h {:port 3000 :join? false})))

(defn stop []
  (when-some [s @server]
    (.stop s)
    (reset! server nil)))