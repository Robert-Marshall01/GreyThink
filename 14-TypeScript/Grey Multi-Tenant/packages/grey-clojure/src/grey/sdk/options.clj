(ns grey.sdk.options
  "Grey SDK configuration options.
  
  Provides configuration management for the Grey SDK client."
  (:require [clojure.spec.alpha :as s]))

;; Specs for options
(s/def ::host string?)
(s/def ::port (s/and int? pos?))
(s/def ::use-tls boolean?)
(s/def ::timeout-ms (s/and int? pos?))
(s/def ::metadata (s/nilable map?))

(s/def ::options
  (s/keys :req-un [::host ::port ::use-tls ::timeout-ms]
          :opt-un [::metadata]))

(def default-timeout-ms
  "Default request timeout in milliseconds."
  30000)

(def default-port
  "Default port for gRPC."
  443)

(defn options
  "Creates Grey SDK options.
  
  Arguments:
    host       - Server hostname
    port       - Server port
    use-tls    - Whether to use TLS
    timeout-ms - Request timeout in milliseconds (optional, default 30000)
    metadata   - Additional metadata map (optional)"
  ([host port use-tls]
   (options host port use-tls default-timeout-ms))
  ([host port use-tls timeout-ms]
   (options host port use-tls timeout-ms nil))
  ([host port use-tls timeout-ms metadata]
   {:host host
    :port port
    :use-tls use-tls
    :timeout-ms timeout-ms
    :metadata metadata}))

(defn local
  "Creates options for local development.
  
  Arguments:
    port - Local server port (default: 8080)"
  ([]
   (local 8080))
  ([port]
   (options "localhost" port false default-timeout-ms)))

(defn production
  "Creates options for production with TLS.
  
  Arguments:
    host - Production server hostname"
  ([host]
   (production host default-port))
  ([host port]
   (options host port true default-timeout-ms)))

(defn endpoint
  "Returns the endpoint string for the options."
  [{:keys [host port]}]
  (str host ":" port))

(defn with-timeout
  "Returns options with a new timeout value."
  [opts timeout-ms]
  (assoc opts :timeout-ms timeout-ms))

(defn with-metadata
  "Returns options with additional metadata merged in."
  [opts metadata]
  (update opts :metadata merge metadata))

(defn valid?
  "Returns true if options are valid."
  [opts]
  (s/valid? ::options opts))

(defn validate
  "Validates options, throwing on invalid."
  [opts]
  (if (valid? opts)
    opts
    (throw (ex-info "Invalid options" {:explain (s/explain-str ::options opts)}))))
