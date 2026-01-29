(ns grey.sdk.domain.mutation
  "Grey SDK Mutation domain.
  
  Provides mutation operations for modifying data at arbitrary endpoints."
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.grpc.mutation :as grpc-mutation]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error]
            [clojure.string :as str]))

(def ^:private valid-methods
  #{:post :put :patch :delete})

(defn- validate-mutation
  "Validates mutation parameters."
  [endpoint opts]
  (let [method (get opts :method :post)]
    (cond
      (str/blank? endpoint)
      (r/err (error/validation-error "Endpoint cannot be empty"))
      
      (not (valid-methods method))
      (r/err (error/validation-error (str "Method must be one of: " (str/join ", " (map name valid-methods)))))
      
      :else
      (r/ok {:endpoint endpoint :opts opts}))))

(defn mutate
  "Executes a mutation to the specified endpoint.
  
  Arguments:
    grey-channel - The Grey channel
    endpoint     - Mutation endpoint path
    opts         - Optional parameters:
                   :method  - HTTP method (:post, :put, :patch, :delete) (default: :post)
                   :body    - Request body (any serializable value)
                   :headers - Additional headers (map)
    
  Returns:
    Result with mutation data (:success, :data, :metadata) or error"
  ([grey-channel endpoint]
   (mutate grey-channel endpoint {}))
  ([grey-channel endpoint opts]
   (if (channel/authenticated? grey-channel)
     (r/then (validate-mutation endpoint opts)
             (fn [{:keys [endpoint opts]}]
               (grpc-mutation/mutate grey-channel
                                     (channel/auth-metadata grey-channel)
                                     endpoint
                                     opts)))
     (r/err (error/unauthorized "User not authenticated")))))
