(ns yaml-config-manager.server
  (:use [ring.adapter.jetty])
  (:require [clojure.data.json]))

(use 'yaml-config-manager.server :reload-all)

(def re (atom {}))
(def server (atom nil))

(defn handler [request]
  (do
    (reset! re request)
    {:status 200
   :headers {"Content-Type" "application/json"}
   :body "{'response': 'woot woot'"}))
(clojure.data.json/pprint (deref re))

(defn start []
  (reset! server (run-jetty handler {:port 3000 :join? false})))

(defn stop []
  (when-some [s @server]
    (.stop s)
    (reset! server nil)))