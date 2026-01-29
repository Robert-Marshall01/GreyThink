(ns grey.sdk.grpc.channel
  "Grey SDK gRPC channel management.
  
  Manages the gRPC channel connection and authentication state."
  (:require [grey.sdk.options :as opts]
            [grey.sdk.error :as error]
            [grey.sdk.result :as r])
  (:import [io.grpc ManagedChannel ManagedChannelBuilder]
           [java.util.concurrent TimeUnit]))

(defprotocol IChannel
  "Protocol for gRPC channel operations."
  (get-channel [this] "Returns the underlying ManagedChannel")
  (get-access-token [this] "Returns the current access token")
  (set-access-token! [this token] "Sets the access token")
  (authenticated? [this] "Returns true if authenticated")
  (auth-metadata [this] "Returns metadata map with authorization header")
  (get-timeout [this] "Returns the timeout in milliseconds")
  (close! [this] "Closes the channel"))

(defn- create-managed-channel
  "Creates a ManagedChannel from options."
  [{:keys [host port use-tls]}]
  (let [builder (ManagedChannelBuilder/forAddress host port)]
    (if use-tls
      (.build builder)
      (.build (.usePlaintext builder)))))

(defrecord GreyChannel [options ^ManagedChannel channel access-token-atom]
  IChannel
  (get-channel [_]
    channel)
  
  (get-access-token [_]
    @access-token-atom)
  
  (set-access-token! [_ token]
    (reset! access-token-atom token))
  
  (authenticated? [_]
    (some? @access-token-atom))
  
  (auth-metadata [_]
    (if-let [token @access-token-atom]
      {"authorization" (str "Bearer " token)}
      {}))
  
  (get-timeout [_]
    (:timeout-ms options))
  
  (close! [_]
    (when channel
      (.shutdown channel)
      (try
        (.awaitTermination channel 5 TimeUnit/SECONDS)
        (catch InterruptedException _
          (.shutdownNow channel))))))

(defn create-channel
  "Creates a new Grey channel from options.
  
  Returns a result with the channel or an error."
  [options]
  (try
    (let [validated (opts/validate options)
          managed-channel (create-managed-channel validated)]
      (r/ok (->GreyChannel validated managed-channel (atom nil))))
    (catch Exception e
      (r/err (error/from-exception e)))))

(defn with-auth
  "Executes f with the channel's auth metadata.
  Returns the result of f or an unauthorized error."
  [channel f]
  (if (authenticated? channel)
    (f (auth-metadata channel))
    (r/err (error/unauthorized))))

(defn with-channel
  "Executes f with the underlying ManagedChannel.
  Returns the result of f or a network error if channel is closed."
  [grey-channel f]
  (let [^ManagedChannel ch (get-channel grey-channel)]
    (if (.isShutdown ch)
      (r/err (error/network-error "Channel is closed"))
      (try
        (f ch)
        (catch Exception e
          (r/err (error/from-exception e)))))))
