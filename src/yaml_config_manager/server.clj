(ns yaml-config-manager.server
  (:use [ring.adapter.jetty])
  (:require [clojure.data.json]
            [cheshire.core :as json]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            ))

(use 'yaml-config-manager.server :reload-all)

(def re (atom {}))
(def re-body (atom {}))
(def server (atom nil))

(defn handler [request]
  (do
    (reset! re request)
    (reset! re-body request)
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    {:body "woot woot!" :a (:server-name request)}}))

(defn app [request]
  ((-> handler
       wrap-json-body
       wrap-json-response) request)
  )

(defn h [request]
  (app request))

(defn start []
  (reset! server (run-jetty h {:port 3000 :join? false})))

(defn stop []
  (when-some [s @server]
    (.stop s)
    (reset! server nil)))