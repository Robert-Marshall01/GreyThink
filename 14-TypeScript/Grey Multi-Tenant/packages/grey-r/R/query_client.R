#' @title Query Client
#' @description Query client for executing read operations
#' @name QueryClient
NULL

#' @importFrom R6 R6Class
#' @export
QueryClient <- R6::R6Class(
  "QueryClient",
  
  public = list(
    #' @description Create a new query client
    #' @param http_client HttpClient instance
    initialize = function(http_client) {
      private$http <- http_client
    },
    
    #' @description Execute a query operation
    #' @param query_name The name of the query
    #' @param variables Query variables (optional)
    #' @return grey_result with query result or error
    query = function(query_name, variables = NULL) {
      if (is.null(query_name) || nchar(trimws(query_name)) == 0) {
        return(result_err(error_validation("Query name is required")))
      }
      
      body <- list(query = query_name)
      if (!is.null(variables)) body$variables <- variables
      
      private$http$post("/api/v1/query", body)
    },
    
    #' @description Execute a batch of queries
    #' @param queries List of query objects with query_name and variables
    #' @return grey_result with array of results or error
    batch_query = function(queries) {
      if (is.null(queries) || !is.list(queries)) {
        return(result_err(error_validation("Queries list is required")))
      }
      
      if (length(queries) == 0) {
        return(result_err(error_validation("At least one query is required")))
      }
      
      formatted <- lapply(queries, function(q) {
        list(
          query = q$query_name,
          variables = q$variables
        )
      })
      
      private$http$post("/api/v1/query/batch", list(queries = formatted))
    },
    
    #' @description Execute a paginated query
    #' @param query_name The query name
    #' @param variables Query variables (optional)
    #' @param page Page number (default: 1)
    #' @param per_page Items per page (default: 20)
    #' @return grey_result with paginated result or error
    paginated_query = function(query_name, variables = NULL, page = 1, per_page = 20) {
      if (is.null(query_name) || nchar(trimws(query_name)) == 0) {
        return(result_err(error_validation("Query name is required")))
      }
      
      body <- list(
        query = query_name,
        variables = variables %||% list(),
        pagination = list(
          page = page,
          per_page = per_page
        )
      )
      
      private$http$post("/api/v1/query", body)
    }
  ),
  
  private = list(
    http = NULL
  )
)
