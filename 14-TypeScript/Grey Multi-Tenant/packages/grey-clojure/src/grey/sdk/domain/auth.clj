(ns grey.sdk.domain.auth
  "Grey SDK Auth domain.
  
  Provides authentication operations including login, logout, and token refresh."
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.grpc.auth :as grpc-auth]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error]
            [clojure.string :as str]))

(defn- validate-credentials
  "Validates email and password."
  [email password]
  (cond
    (str/blank? email)
    (r/err (error/validation-error "Email cannot be empty"))
    
    (str/blank? password)
    (r/err (error/validation-error "Password cannot be empty"))
    
    (not (str/includes? email "@"))
    (r/err (error/validation-error "Invalid email format"))
    
    :else
    (r/ok {:email email :password password})))

(defn login
  "Authenticates a user with email and password.
  
  On success, stores the access token in the channel state.
  
  Arguments:
    grey-channel - The Grey channel
    email        - User email
    password     - User password
    
  Returns:
    Result with auth data (:access-token, :refresh-token, :expires-in) or error"
  [grey-channel email password]
  (r/then (validate-credentials email password)
          (fn [_]
            (let [result (grpc-auth/login grey-channel email password)]
              (when (r/ok? result)
                (channel/set-access-token! grey-channel (:access-token (:data result))))
              result))))

(defn logout
  "Logs out the current user.
  
  Clears the access token from the channel state.
  
  Arguments:
    grey-channel - The Grey channel
    
  Returns:
    Result with nil or error"
  [grey-channel]
  (if (channel/authenticated? grey-channel)
    (let [result (grpc-auth/logout grey-channel (channel/auth-metadata grey-channel))]
      (when (r/ok? result)
        (channel/set-access-token! grey-channel nil))
      (r/map-data result (constantly nil)))
    (r/err (error/unauthorized "Not authenticated"))))

(defn refresh
  "Refreshes the authentication tokens using a refresh token.
  
  On success, updates the access token in the channel state.
  
  Arguments:
    grey-channel  - The Grey channel
    refresh-token - The refresh token from previous login
    
  Returns:
    Result with auth data or error"
  [grey-channel refresh-token]
  (if (str/blank? refresh-token)
    (r/err (error/validation-error "Refresh token cannot be empty"))
    (let [result (grpc-auth/refresh grey-channel refresh-token)]
      (when (r/ok? result)
        (channel/set-access-token! grey-channel (:access-token (:data result))))
      result)))
