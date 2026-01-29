#' @title User Client
#' @description User client for user-related operations
#' @name UserClient
NULL

#' @importFrom R6 R6Class
#' @export
UserClient <- R6::R6Class(
  "UserClient",
  
  public = list(
    #' @description Create a new user client
    #' @param http_client HttpClient instance
    initialize = function(http_client) {
      private$http <- http_client
    },
    
    #' @description Get a user by ID
    #' @param user_id The user ID
    #' @return grey_result with user data or error
    get_user = function(user_id) {
      if (is.null(user_id) || nchar(trimws(as.character(user_id))) == 0) {
        return(result_err(error_validation("User ID is required")))
      }
      
      private$http$get(paste0("/api/v1/users/", user_id))
    },
    
    #' @description Get the current authenticated user
    #' @return grey_result with user data or error
    get_current_user = function() {
      private$http$get("/api/v1/users/me")
    },
    
    #' @description Update a user's profile
    #' @param user_id The user ID
    #' @param data Update data (list)
    #' @return grey_result with updated user or error
    update_user = function(user_id, data) {
      if (is.null(user_id) || nchar(trimws(as.character(user_id))) == 0) {
        return(result_err(error_validation("User ID is required")))
      }
      
      if (is.null(data) || length(data) == 0) {
        return(result_err(error_validation("Update data is required")))
      }
      
      private$http$patch(paste0("/api/v1/users/", user_id), data)
    }
  ),
  
  private = list(
    http = NULL
  )
)
