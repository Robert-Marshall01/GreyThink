# Grey Clojure SDK

Clojure client for the Grey Multi-Tenant API using gRPC transport.

## Installation

Add to your `deps.edn`:

```clojure
{:deps {grey/sdk {:local/root "../grey-clojure"}}}
```

Or with Maven coordinates (when published):

```clojure
{:deps {com.grey/sdk {:mvn/version "0.1.0"}}}
```

## Quick Start

```clojure
(require '[grey.sdk.core :as grey]
         '[grey.sdk.options :as opts])

;; Create client with options
(def client (grey/create-client (opts/local 8080)))

;; Or for production
(def client (grey/create-client (opts/production "api.grey.com")))

;; Login
(let [result (grey/login client "user@example.com" "password")]
  (if (:ok result)
    (println "Logged in:" (:data result))
    (println "Error:" (:error result))))

;; Get current user
(let [{:keys [ok data error]} (grey/get-user client)]
  (when ok
    (println "Hello," (:name data))))

;; List projects
(let [{:keys [ok data]} (grey/list-projects client {:page 1 :page-size 10})]
  (when ok
    (doseq [project (:projects data)]
      (println "Project:" (:name project)))))

;; Create project
(grey/create-project client "My Project" {:description "A new project"})

;; Query
(grey/query client "/api/data" {:params {:filter "active"}})

;; Mutate
(grey/mutate client "/api/update" {:method :put :body {:name "Updated"}})

;; Logout
(grey/logout client)

;; Close client
(grey/close client)
```

## Functional Style with Threading

```clojure
(require '[grey.sdk.core :as grey]
         '[grey.sdk.result :as r])

(-> (grey/create-client (opts/local 8080))
    (grey/login "user@example.com" "password")
    (r/then (fn [_] (grey/get-user client)))
    (r/then (fn [user] (println "User:" (:name user))))
    (r/catch (fn [err] (println "Error:" (:message err)))))
```

## Error Handling

All operations return a result map:

```clojure
;; Success
{:ok true :data {...}}

;; Error
{:ok false :error {:code :unauthorized
                   :message "..."
                   :details {...}}}
```

Error codes:
- `:unauthorized` - Authentication required
- `:forbidden` - Permission denied
- `:not-found` - Resource not found
- `:validation-error` - Input validation failed
- `:network-error` - Network failure
- `:timeout` - Request timeout
- `:server-error` - Server-side error
- `:unknown` - Unclassified error

## Testing

```bash
clj -M:test
```

## Building

```bash
clj -T:build jar
```

## License

MIT
