(ns grey.sdk.domain.query
  "Grey SDK Query domain.
  
  Provides query operations for fetching data from arbitrary endpoints."
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.grpc.query :as grpc-query]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error]
            [clojure.string :as str]))

(defn- validate-endpoint
  "Validates query endpoint."
  [endpoint]
  (if (str/blank? endpoint)
    (r/err (error/validation-error "Endpoint cannot be empty"))
    (r/ok endpoint)))

(defn query
  "Executes a query to the specified endpoint.
  
  Arguments:
    grey-channel - The Grey channel
    endpoint     - Query endpoint path
    opts         - Optional parameters:
                   :params       - Query parameters (map)
                   :require-auth - Whether auth is required (default: true)
                   :headers      - Additional headers (map)
    
  Returns:
    Result with query data (:data, :metadata) or error"
  ([grey-channel endpoint]
   (query grey-channel endpoint {}))
  ([grey-channel endpoint opts]
   (let [require-auth (get opts :require-auth true)]
     (r/then (validate-endpoint endpoint)
             (fn [validated-endpoint]
               (if require-auth
                 (if (channel/authenticated? grey-channel)
                   (grpc-query/query grey-channel
                                     (channel/auth-metadata grey-channel)
                                     validated-endpoint
                                     opts)
                   (r/err (error/unauthorized "User not authenticated")))
                 (grpc-query/query grey-channel nil validated-endpoint opts)))))))
