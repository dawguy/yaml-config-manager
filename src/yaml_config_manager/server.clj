(ns yaml-config-manager.server
  (:use [ring.adapter.jetty])
  (:require [clojure.data.json]
            [ring.util.response :as r]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [yaml-config-manager.manager :as m]
            ))

;(use 'yaml-config-manager.server :reload-all)

(def re (atom {}))
(def re-body (atom {}))
(def server (atom nil))

(defn not-found [_] {:status 404 :body {:error "Endpoint not found."}})
(defn router [uri body]
(let [target (last (clojure.string/split uri #"/"))]
  (case target
    "apply-properties-file" m/apply-properties-file
    "apply-properties-env" m/apply-properties-env
    "migrate-properties-file" m/migrate-properties-file
    "migrate-properties-env" m/migrate-properties-env
    not-found
    )))

(defn handler [request]
  (do
    (reset! re request)
    (reset! re-body (:body request))
    (let [tar-func (router (:uri request) (:body request))
          res (tar-func (:body request))]
      (if (= tar-func not-found)
        res
        {:status (= tar-func 200)
         :body   {:body res}}))))

(defn header-adder [handler]
  (fn [request]
    (r/header (handler request) "Content-Type" "application/json")))

(defn app [request]
  ((-> handler
       header-adder
       wrap-json-body
       wrap-json-response
       ) request))

(defn h [request]
  (app request))

(defn start []
  (reset! server (run-jetty h {:port 3000 :join? false})))

(defn stop []
  (when-some [s @server]
    (.stop s)
    (reset! server nil)))