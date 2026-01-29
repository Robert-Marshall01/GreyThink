#' @title Auth Client
#' @description Authentication client for login, logout, and token refresh
#' @name AuthClient
NULL

#' @importFrom R6 R6Class
#' @export
AuthClient <- R6::R6Class(
  "AuthClient",
  
  public = list(
    #' @description Create a new auth client
    #' @param http_client HttpClient instance
    initialize = function(http_client) {
      private$http <- http_client
    },
    
    #' @description Login with email and password
    #' @param email User email
    #' @param password User password
    #' @return grey_result with tokens or error
    login = function(email, password) {
      # Validate inputs
      if (is.null(email) || nchar(trimws(email)) == 0) {
        return(result_err(error_validation("Email is required")))
      }
      
      if (is.null(password) || nchar(trimws(password)) == 0) {
        return(result_err(error_validation("Password is required")))
      }
      
      result <- private$http$post("/api/v1/auth/login", list(
        email = email,
        password = password
      ))
      
      # Store access token on success
      if (is_ok(result)) {
        data <- result$data
        if (!is.null(data$access_token)) {
          private$http$set_token(data$access_token)
        }
      }
      
      result
    },
    
    #' @description Logout the current user
    #' @return grey_result
    logout = function() {
      result <- private$http$post("/api/v1/auth/logout")
      
      # Clear token on success
      if (is_ok(result)) {
        private$http$clear_token()
      }
      
      result
    },
    
    #' @description Refresh the access token
    #' @param refresh_token The refresh token
    #' @return grey_result with new tokens or error
    refresh = function(refresh_token) {
      if (is.null(refresh_token) || nchar(trimws(refresh_token)) == 0) {
        return(result_err(error_validation("Refresh token is required")))
      }
      
      result <- private$http$post("/api/v1/auth/refresh", list(
        refresh_token = refresh_token
      ))
      
      # Update access token on success
      if (is_ok(result)) {
        data <- result$data
        if (!is.null(data$access_token)) {
          private$http$set_token(data$access_token)
        }
      }
      
      result
    },
    
    #' @description Manually set the access token
    #' @param token Access token string
    set_token = function(token) {
      private$http$set_token(token)
    }
  ),
  
  private = list(
    http = NULL
  )
)
