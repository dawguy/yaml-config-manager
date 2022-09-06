(ns yaml-config-manager.requester
  (:require [clj-http.core :as http]
            [clj-http.client :as client]
            [cheshire.core :as json]))

(def a-response (atom {}))
(def response (deref a-response))

(defn dummy-json [_] "Returns a json string which looks kinda like the expected response"
  {"status" 200
   "data" {
           "data" {"dbUser" "DB-USER-SUCCESS", "dbPassword" "DB-PASSWORD-SUCCESS",
                   "the-port-in-use" "5432", "server.name" "www.localhost"}}})
(defn request-secrets [url token] "Requests the secrets from Vault"
  (let [response (client/get url {
                                  :body         (json/generate-string {})
                                  :headers      {:client-token token}
                                  :content-type :json
                                  :accept       :json})]
    (reset! a-response response)
    (get-in (json/parse-string (:body response)) ["body" "data" "data"]) ; TODO: Replace lookup string with correct one
))

;(client/head "http://example.com")
;
;(defn say-hi [status url] (prn (str "Hello! You got a " status " response from " url)))
;
;(say-hi (:status (client/head "http://example.com")) "http://example.com")
;
;; It appears that in order to send this it needs to be turned into a json string.
;(def body-vars {
;           "env" "development",
;           "fileName" "serviceA.yml",
;           "serviceName" "serviceA",
;           "properties" { "featureCFlag" true,
;                          "featureD.url" "updatedURLForSpringProperties"
;                        }
;           })
;
;(def body-vars-str "{
;                \"env\": \"development\",
;                \"fileName\": \"serviceA.yml\",
;                \"serviceName\": \"serviceA\",
;                \"properties\": { \"featureCFlag\": true,
;                              \"featureD.url\": \"updatedURLForSpringProperties\"
;                              }
;                }")
;
;(def body-vars-jsonified (json/generate-string body-vars))
;
;; Fails because we can't send a map
;(client/post "http://localhost:3000/apply-properties-file"
;             {
;              :body body-vars
;              :headers {}
;              :content-type :json
;              :accept :json
;              })
;
;; Fails because the `:` characters aren't being included
;(client/post "http://localhost:3000/apply-properties-file"
;             {
;              :body (str body-vars)
;              :headers {}
;              :content-type :json
;              :accept :json
;              })
;
;; Succeeds with hard-coded json string
;(client/post "http://localhost:3000/apply-properties-file"
;             {
;              :body body-vars-str
;              :headers {}
;              :content-type :json
;              :accept :json
;              })
;
;; Succeeds as we successfully JSON encoded the map
;(client/post "http://localhost:3000/apply-properties-file"
;             {
;              :body body-vars-jsonified
;              :headers {}
;              :content-type :json
;              :accept :json
;              })