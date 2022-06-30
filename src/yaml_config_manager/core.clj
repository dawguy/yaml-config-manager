(ns yaml-config-manager.core
  (:require [ring.util.response :as response])
  (:use ring.adapter.jetty)
  )

(use 'yaml-config-manager.core :reload-all)

(defn handler [request]
  (ring.util.response/response request))
(run-jetty handler {:port 3000})

((intern 'yaml-config-manager.core (symbol "handler")) "aa")