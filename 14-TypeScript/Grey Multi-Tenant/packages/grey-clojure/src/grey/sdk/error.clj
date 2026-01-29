(ns grey.sdk.error
  "Grey SDK error handling.
  
  Provides normalized error representation and utilities for error handling.
  All errors are represented as maps with :code, :message, and :details keys."
  (:require [clojure.string :as str]))

;; Error codes
(def error-codes
  #{:unauthorized
    :forbidden
    :not-found
    :validation-error
    :network-error
    :timeout
    :server-error
    :unknown})

(defn grey-error
  "Creates a normalized error map.
  
  Arguments:
    code    - Keyword error code (e.g., :unauthorized, :not-found)
    message - Human-readable error message
    details - Optional additional details (map or nil)"
  ([code message]
   (grey-error code message nil))
  ([code message details]
   {:code (if (error-codes code) code :unknown)
    :message (str message)
    :details details}))

(defn unauthorized
  "Creates an unauthorized error."
  ([]
   (unauthorized "Authentication required"))
  ([message]
   (grey-error :unauthorized message)))

(defn forbidden
  "Creates a forbidden error."
  ([]
   (forbidden "Permission denied"))
  ([message]
   (grey-error :forbidden message)))

(defn not-found
  "Creates a not-found error."
  ([]
   (not-found "Resource not found"))
  ([message]
   (grey-error :not-found message)))

(defn validation-error
  "Creates a validation error."
  ([message]
   (grey-error :validation-error message))
  ([message details]
   (grey-error :validation-error message details)))

(defn network-error
  "Creates a network error."
  ([]
   (network-error "Network error occurred"))
  ([message]
   (grey-error :network-error message)))

(defn timeout-error
  "Creates a timeout error."
  ([]
   (timeout-error "Request timed out"))
  ([message]
   (grey-error :timeout message)))

(defn server-error
  "Creates a server error."
  ([]
   (server-error "Server error occurred"))
  ([message]
   (grey-error :server-error message)))

(defn unknown-error
  "Creates an unknown error."
  ([]
   (unknown-error "An unknown error occurred"))
  ([message]
   (grey-error :unknown message)))

;; gRPC status code mapping
(def ^:private grpc-status->code
  {0  :unknown        ; OK (shouldn't be an error)
   1  :unknown        ; CANCELLED
   2  :unknown        ; UNKNOWN
   3  :validation-error ; INVALID_ARGUMENT
   4  :timeout        ; DEADLINE_EXCEEDED
   5  :not-found      ; NOT_FOUND
   6  :validation-error ; ALREADY_EXISTS
   7  :forbidden      ; PERMISSION_DENIED
   8  :server-error   ; RESOURCE_EXHAUSTED
   9  :validation-error ; FAILED_PRECONDITION
   10 :server-error   ; ABORTED
   11 :validation-error ; OUT_OF_RANGE
   12 :server-error   ; UNIMPLEMENTED
   13 :server-error   ; INTERNAL
   14 :network-error  ; UNAVAILABLE
   15 :server-error   ; DATA_LOSS
   16 :unauthorized}) ; UNAUTHENTICATED

(defn from-grpc-status
  "Converts a gRPC status code to a Grey error."
  [status-code message]
  (let [code (get grpc-status->code status-code :unknown)]
    (grey-error code message {:grpc-status status-code})))

(defn from-exception
  "Converts an exception to a Grey error."
  [^Throwable ex]
  (cond
    (instance? java.net.ConnectException ex)
    (network-error (.getMessage ex))
    
    (instance? java.net.SocketTimeoutException ex)
    (timeout-error (.getMessage ex))
    
    (instance? java.util.concurrent.TimeoutException ex)
    (timeout-error (.getMessage ex))
    
    :else
    (unknown-error (or (.getMessage ex) "Unknown error"))))

(defn from-any
  "Converts any value to a Grey error.
  
  Handles:
    - Grey error maps (passthrough)
    - Keywords (as error codes)
    - Exceptions
    - Strings (as messages)
    - Other (unknown error)"
  [value]
  (cond
    (and (map? value) (:code value) (:message value))
    value
    
    (keyword? value)
    (grey-error value (name value))
    
    (instance? Throwable value)
    (from-exception value)
    
    (string? value)
    (unknown-error value)
    
    :else
    (unknown-error (str value))))

(defn error?
  "Returns true if value is a Grey error map."
  [value]
  (and (map? value)
       (contains? value :code)
       (contains? value :message)))
