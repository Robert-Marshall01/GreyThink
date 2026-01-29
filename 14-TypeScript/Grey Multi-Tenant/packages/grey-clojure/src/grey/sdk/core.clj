(ns grey.sdk.core
  "Grey SDK Core - main client façade.
  
  This namespace provides the primary interface for the Grey SDK.
  It aggregates all domain operations and manages the client lifecycle.
  
  ## Quick Start
  
  ```clojure
  (require '[grey.sdk.core :as grey]
           '[grey.sdk.options :as opts])
  
  ;; Create and use client
  (def client (grey/create-client (opts/local 8080)))
  
  (grey/login client \"user@example.com\" \"password\")
  (grey/get-user client)
  (grey/list-projects client)
  
  (grey/close client)
  ```"
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.domain.auth :as auth]
            [grey.sdk.domain.user :as user]
            [grey.sdk.domain.projects :as projects]
            [grey.sdk.domain.query :as query-domain]
            [grey.sdk.domain.mutation :as mutation]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error]
            [grey.sdk.options :as opts]))

;; =============================================================================
;; Client Creation
;; =============================================================================

(defn create-client
  "Creates a new Grey SDK client.
  
  Arguments:
    options - Client options from grey.sdk.options
    
  Returns:
    The Grey channel (client) or throws on error
    
  Example:
    (create-client (opts/local 8080))
    (create-client (opts/production \"api.grey.com\"))"
  [options]
  (let [result (channel/create-channel options)]
    (if (r/ok? result)
      (:data result)
      (throw (ex-info "Failed to create client" {:error (:error result)})))))

(defn create-client-safe
  "Creates a new Grey SDK client, returning a result.
  
  Arguments:
    options - Client options from grey.sdk.options
    
  Returns:
    Result with client or error"
  [options]
  (channel/create-channel options))

(defn close
  "Closes the client and releases resources.
  
  Arguments:
    client - The Grey client"
  [client]
  (channel/close! client))

;; =============================================================================
;; Client State
;; =============================================================================

(defn authenticated?
  "Returns true if the client is authenticated.
  
  Arguments:
    client - The Grey client"
  [client]
  (channel/authenticated? client))

(defn get-options
  "Returns the client's options.
  
  Arguments:
    client - The Grey client"
  [client]
  (:options client))

;; =============================================================================
;; Auth Operations
;; =============================================================================

(defn login
  "Authenticates with email and password.
  
  On success, stores the access token in the client state.
  
  Arguments:
    client   - The Grey client
    email    - User email
    password - User password
    
  Returns:
    Result with auth data (:access-token, :refresh-token, :expires-in) or error"
  [client email password]
  (auth/login client email password))

(defn logout
  "Logs out the current user.
  
  Clears the access token from the client state.
  
  Arguments:
    client - The Grey client
    
  Returns:
    Result with nil or error"
  [client]
  (auth/logout client))

(defn refresh
  "Refreshes authentication with a refresh token.
  
  Arguments:
    client        - The Grey client
    refresh-token - The refresh token from previous login
    
  Returns:
    Result with auth data or error"
  [client refresh-token]
  (auth/refresh client refresh-token))

;; =============================================================================
;; User Operations
;; =============================================================================

(defn get-user
  "Gets the currently authenticated user.
  
  Arguments:
    client - The Grey client
    opts   - Optional parameters (map)
    
  Returns:
    Result with user data (:id, :email, :name, :avatar, :metadata) or error"
  ([client]
   (user/get-user client))
  ([client opts]
   (user/get-user client opts)))

;; =============================================================================
;; Projects Operations
;; =============================================================================

(defn list-projects
  "Lists projects for the authenticated user.
  
  Arguments:
    client - The Grey client
    opts   - Optional parameters:
             :page      - Page number (default: 1)
             :page-size - Items per page (default: 20)
    
  Returns:
    Result with projects data (:projects, :total, :page, :page-size) or error"
  ([client]
   (projects/list-projects client))
  ([client opts]
   (projects/list-projects client opts)))

(defn create-project
  "Creates a new project.
  
  Arguments:
    client - The Grey client
    name   - Project name
    opts   - Optional parameters:
             :description - Project description
             :metadata    - Additional metadata
    
  Returns:
    Result with project data or error"
  ([client name]
   (projects/create-project client name))
  ([client name opts]
   (projects/create-project client name opts)))

;; =============================================================================
;; Query Operations
;; =============================================================================

(defn query
  "Executes a query to the specified endpoint.
  
  Arguments:
    client   - The Grey client
    endpoint - Query endpoint path
    opts     - Optional parameters:
               :params       - Query parameters (map)
               :require-auth - Whether auth is required (default: true)
    
  Returns:
    Result with query data (:data, :metadata) or error"
  ([client endpoint]
   (query-domain/query client endpoint))
  ([client endpoint opts]
   (query-domain/query client endpoint opts)))

;; =============================================================================
;; Mutation Operations
;; =============================================================================

(defn mutate
  "Executes a mutation to the specified endpoint.
  
  Arguments:
    client   - The Grey client
    endpoint - Mutation endpoint path
    opts     - Optional parameters:
               :method - HTTP method (:post, :put, :patch, :delete)
               :body   - Request body
    
  Returns:
    Result with mutation data (:success, :data, :metadata) or error"
  ([client endpoint]
   (mutation/mutate client endpoint))
  ([client endpoint opts]
   (mutation/mutate client endpoint opts)))

;; =============================================================================
;; Convenience Macros
;; =============================================================================

(defmacro with-client
  "Creates a client, executes body, and ensures cleanup.
  
  Example:
    (with-client [client (opts/local 8080)]
      (login client \"user@example.com\" \"password\")
      (get-user client))"
  [[client-sym options] & body]
  `(let [~client-sym (create-client ~options)]
     (try
       ~@body
       (finally
         (close ~client-sym)))))
