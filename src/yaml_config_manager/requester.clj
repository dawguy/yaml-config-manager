(ns yaml-config-manager.requester
  (:require [clj-http.core :as http]
            [clj-http.client :as client]
            [cheshire.core :as json]))

(client/head "http://example.com")

(defn say-hi [status url] (prn (str "Hello! You got a " status " response from " url)))

(say-hi (:status (client/head "http://example.com")) "http://example.com")

; It appears that in order to send this it needs to be turned into a json string.
(def body-vars {
           "env" "development",
           "fileName" "serviceA.yml",
           "serviceName" "serviceA",
           "properties" { "featureCFlag" true,
                          "featureD.url" "updatedURLForSpringProperties"
                        }
           })

(def body-vars-str "{
                \"env\": \"development\",
                \"fileName\": \"serviceA.yml\",
                \"serviceName\": \"serviceA\",
                \"properties\": { \"featureCFlag\": true,
                              \"featureD.url\": \"updatedURLForSpringProperties\"
                              }
                }")

(def body-vars-jsonified (json/generate-string body-vars))

; Fails because we can't send a map
(client/post "http://localhost:3000/apply-properties-file"
             {
              :body body-vars
              :headers {}
              :content-type :json
              :accept :json
              })

; Fails because the `:` characters aren't being included
(client/post "http://localhost:3000/apply-properties-file"
             {
              :body (str body-vars)
              :headers {}
              :content-type :json
              :accept :json
              })

; Succeeds with hard-coded json string
(client/post "http://localhost:3000/apply-properties-file"
             {
              :body body-vars-str
              :headers {}
              :content-type :json
              :accept :json
              })

; Succeeds as we successfully JSON encoded the map
(client/post "http://localhost:3000/apply-properties-file"
             {
              :body body-vars-jsonified
              :headers {}
              :content-type :json
              :accept :json
              })