(ns grey.sdk.grpc.user
  "Grey SDK User gRPC service wrapper.
  
  This is a placeholder stub. In a real implementation, this would
  use generated protobuf client stubs from .proto files."
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error]))

(defn get-user
  "Gets the current user via gRPC.
  
  Arguments:
    grey-channel - The Grey channel
    metadata     - Auth metadata
    opts         - Optional parameters
    
  Returns:
    Result with user data or error"
  ([grey-channel metadata]
   (get-user grey-channel metadata {}))
  ([grey-channel metadata opts]
   (channel/with-channel grey-channel
     (fn [_managed-channel]
       ;; Stub implementation
       (Thread/sleep 1)
       (r/ok {:id "stub_user_id"
              :email "user@example.com"
              :name "Stub User"
              :avatar nil
              :metadata nil})))))
