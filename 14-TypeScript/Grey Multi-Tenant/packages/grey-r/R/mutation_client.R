#' @title Mutation Client
#' @description Mutation client for executing write operations
#' @name MutationClient
NULL

#' @importFrom R6 R6Class
#' @export
MutationClient <- R6::R6Class(
  "MutationClient",
  
  public = list(
    #' @description Create a new mutation client
    #' @param http_client HttpClient instance
    initialize = function(http_client) {
      private$http <- http_client
    },
    
    #' @description Execute a mutation operation
    #' @param mutation_name The name of the mutation
    #' @param input Mutation input data
    #' @return grey_result with mutation result or error
    mutate = function(mutation_name, input) {
      if (is.null(mutation_name) || nchar(trimws(mutation_name)) == 0) {
        return(result_err(error_validation("Mutation name is required")))
      }
      
      if (is.null(input)) {
        return(result_err(error_validation("Input data is required")))
      }
      
      body <- list(
        mutation = mutation_name,
        input = input
      )
      
      private$http$post("/api/v1/mutate", body)
    },
    
    #' @description Execute a batch of mutations
    #' @param mutations List of mutation objects with mutation_name and input
    #' @return grey_result with array of results or error
    batch_mutate = function(mutations) {
      if (is.null(mutations) || !is.list(mutations)) {
        return(result_err(error_validation("Mutations list is required")))
      }
      
      if (length(mutations) == 0) {
        return(result_err(error_validation("At least one mutation is required")))
      }
      
      formatted <- lapply(mutations, function(m) {
        list(
          mutation = m$mutation_name,
          input = m$input
        )
      })
      
      private$http$post("/api/v1/mutate/batch", list(mutations = formatted))
    },
    
    #' @description Execute a mutation within a transaction
    #' @param transaction_id Transaction ID
    #' @param mutation_name Mutation name
    #' @param input Mutation input
    #' @return grey_result with mutation result or error
    transactional_mutate = function(transaction_id, mutation_name, input) {
      if (is.null(transaction_id) || nchar(trimws(transaction_id)) == 0) {
        return(result_err(error_validation("Transaction ID is required")))
      }
      
      if (is.null(mutation_name) || nchar(trimws(mutation_name)) == 0) {
        return(result_err(error_validation("Mutation name is required")))
      }
      
      if (is.null(input)) {
        return(result_err(error_validation("Input data is required")))
      }
      
      body <- list(
        transaction_id = transaction_id,
        mutation = mutation_name,
        input = input
      )
      
      private$http$post("/api/v1/mutate/transactional", body)
    }
  ),
  
  private = list(
    http = NULL
  )
)
