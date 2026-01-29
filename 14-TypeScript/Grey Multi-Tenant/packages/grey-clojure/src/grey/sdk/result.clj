(ns grey.sdk.result
  "Result handling utilities for Grey SDK.
  
  Provides functional utilities for working with {:ok true/false :data/:error} result maps."
  (:require [grey.sdk.error :as error]))

(defn ok
  "Creates a success result."
  [data]
  {:ok true :data data})

(defn err
  "Creates an error result."
  [error]
  {:ok false :error (error/from-any error)})

(defn ok?
  "Returns true if result is successful."
  [result]
  (true? (:ok result)))

(defn err?
  "Returns true if result is an error."
  [result]
  (false? (:ok result)))

(defn then
  "Applies f to the data if result is ok, otherwise returns the error result.
  f should return a result map."
  [result f]
  (if (ok? result)
    (try
      (f (:data result))
      (catch Exception e
        (err e)))
    result))

(defn map-data
  "Applies f to the data if result is ok, wrapping the return value in ok.
  Unlike `then`, f returns a plain value, not a result."
  [result f]
  (if (ok? result)
    (try
      (ok (f (:data result)))
      (catch Exception e
        (err e)))
    result))

(defn catch-err
  "Applies f to the error if result is err, otherwise returns the ok result.
  f should return a result map."
  [result f]
  (if (err? result)
    (try
      (f (:error result))
      (catch Exception e
        (err e)))
    result))

(defn map-error
  "Applies f to the error if result is err, wrapping the return value in err.
  Unlike `catch-err`, f returns a plain error, not a result."
  [result f]
  (if (err? result)
    (try
      (err (f (:error result)))
      (catch Exception e
        (err e)))
    result))

(defn unwrap
  "Extracts the data from a result, throwing an exception if it's an error."
  [result]
  (if (ok? result)
    (:data result)
    (throw (ex-info "Unwrap failed on error result" {:error (:error result)}))))

(defn unwrap-or
  "Extracts the data from a result, or returns default if it's an error."
  [result default]
  (if (ok? result)
    (:data result)
    default))

(defn unwrap-error
  "Extracts the error from a result, throwing if it's ok."
  [result]
  (if (err? result)
    (:error result)
    (throw (ex-info "Unwrap-error failed on ok result" {:data (:data result)}))))

(defmacro try-result
  "Wraps body in try-catch, returning ok on success or err on exception."
  [& body]
  `(try
     (ok (do ~@body))
     (catch Exception e#
       (err e#))))

(defn combine
  "Combines multiple results into a single result.
  If all are ok, returns ok with a vector of all data.
  If any is err, returns the first error."
  [& results]
  (reduce
   (fn [acc result]
     (if (ok? acc)
       (if (ok? result)
         (ok (conj (:data acc) (:data result)))
         result)
       acc))
   (ok [])
   results))
