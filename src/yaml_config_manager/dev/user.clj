(ns yaml-config-manager.dev.user
  (:require [yaml-config-manager.server] [nrepl.cmdline])
  )

(defn start [args]
    (prn "hi")
    (nrepl.cmdline/start-server {:port 4001 :join? true})
    (prn "nREPL Server started")
    (yaml-config-manager.server/start)
    (prn "Server started")
    (yaml-config-manager.manager/load-files!)
  )

(defn -main []
  (start {}))