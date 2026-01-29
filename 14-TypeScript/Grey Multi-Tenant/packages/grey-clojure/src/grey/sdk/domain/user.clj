(ns grey.sdk.domain.user
  "Grey SDK User domain.
  
  Provides user operations for retrieving user information."
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.grpc.user :as grpc-user]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error]))

(defn get-user
  "Gets the currently authenticated user.
  
  Arguments:
    grey-channel - The Grey channel
    opts         - Optional parameters:
                   :force-refresh - Force refresh from server (boolean)
    
  Returns:
    Result with user data (:id, :email, :name, :avatar, :metadata) or error"
  ([grey-channel]
   (get-user grey-channel {}))
  ([grey-channel opts]
   (if (channel/authenticated? grey-channel)
     (grpc-user/get-user grey-channel (channel/auth-metadata grey-channel) opts)
     (r/err (error/unauthorized "User not authenticated")))))
