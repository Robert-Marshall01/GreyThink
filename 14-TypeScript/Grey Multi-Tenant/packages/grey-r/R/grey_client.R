#' @title Grey Client
#' @description Main client facade for the Grey SDK
#' @name GreyClient
NULL

#' @importFrom R6 R6Class
#' @export
GreyClient <- R6::R6Class(
  "GreyClient",
  
  public = list(
    #' @field auth AuthClient instance
    auth = NULL,
    
    #' @field user UserClient instance
    user = NULL,
    
    #' @field projects ProjectsClient instance
    projects = NULL,
    
    #' @field query QueryClient instance
    query = NULL,
    
    #' @field mutation MutationClient instance
    mutation = NULL,
    
    #' @description Create a new Grey client
    #' @param options GreyOptions instance
    initialize = function(options) {
      private$options <- options
      private$http_client <- HttpClient$new(options)
      
      # Initialize domain clients
      self$auth <- AuthClient$new(private$http_client)
      self$user <- UserClient$new(private$http_client)
      self$projects <- ProjectsClient$new(private$http_client)
      self$query <- QueryClient$new(private$http_client)
      self$mutation <- MutationClient$new(private$http_client)
    },
    
    #' @description Get the current options
    #' @return GreyOptions
    get_options = function() {
      private$options
    },
    
    #' @description Check if authenticated (has access token)
    #' @return TRUE if authenticated
    is_authenticated = function() {
      !is.null(private$http_client$access_token)
    },
    
    #' @description Clear the current authentication
    clear_auth = function() {
      private$http_client$clear_token()
    }
  ),
  
  private = list(
    options = NULL,
    http_client = NULL
  )
)

#' Create a client for local development
#'
#' @param port Local server port (default: 8080)
#' @return GreyClient
#' @export
GreyClient$local <- function(port = 8080) {
  options <- GreyOptions$local(port)
  GreyClient$new(options)
}

#' Create a client for production
#'
#' @param host Production server hostname
#' @param port Server port (default: 443)
#' @return GreyClient
#' @export
GreyClient$production <- function(host, port = 443) {
  options <- GreyOptions$production(host, port)
  GreyClient$new(options)
}
