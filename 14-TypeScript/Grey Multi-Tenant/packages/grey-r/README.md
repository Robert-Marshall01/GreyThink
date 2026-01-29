# Grey SDK

R client SDK for the Grey Multi-Tenant API.

## Installation

```r
# Install from source
install.packages("greysdk", repos = NULL, type = "source")

# Or using devtools
devtools::install_github("grey/grey-r")
```
## Requirements

- R >= 4.0
- httr
- jsonlite
- R6

## Usage

```r
library(greysdk)

# Create client for local development
client <- GreyClient$new(GreyOptions$local(8080))

# Or for production
client <- GreyClient$new(GreyOptions$production("api.grey.com"))

# Authenticate
result <- client$auth$login(email = "user@example.com", password = "secret")

if (is_ok(result)) {
  tokens <- result$data
  print(paste("Access token:", tokens$access_token))
} else {
  error <- result$error
  print(paste("Error:", error$message))
}

# List projects
projects_result <- client$projects$list_projects()

if (is_ok(projects_result)) {
  for (project in projects_result$data$projects) {
    print(paste("Project:", project$name))
  }
}

# Create a project
create_result <- client$projects$create_project(
  name = "New Project",
  description = "A description"
)

# Execute a query
query_result <- client$query$query(
  query_name = "getUsers",
  variables = list(limit = 10)
)

# Execute a mutation
mutation_result <- client$mutation$mutate(
  mutation_name = "updateUser",
  input = list(id = "123", name = "New Name")
)

# Logout
client$auth$logout()
```

## Error Handling

All methods return a result object with `ok` and either `data` or `error`:

```r
result <- client$auth$login(email = email, password = password)

if (is_ok(result)) {
  data <- result$data
  # Handle success
} else {
  error <- result$error
  # error is a list with: code, message, details
  print(paste("Error code:", error$code))
  print(paste("Message:", error$message))
}
```

## Error Codes

- `unauthorized` - Authentication required (401)
- `forbidden` - Permission denied (403)
- `not_found` - Resource not found (404)
- `validation_error` - Invalid input (400, 422)
- `network_error` - Connection failed
- `timeout` - Request timed out
- `server_error` - Server error (5xx)
- `unknown` - Unknown error

## License

MIT
