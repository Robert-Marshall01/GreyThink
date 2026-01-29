#' @title Grey Options
#' @description Configuration options for the Grey SDK
#' @name GreyOptions
NULL

#' @importFrom R6 R6Class
#' @export
GreyOptions <- R6::R6Class(
  "GreyOptions",
  
  public = list(
    #' @field host Server hostname
    host = NULL,
    
    #' @field port Server port
    port = NULL,
    
    #' @field use_tls Whether to use HTTPS
    use_tls = NULL,
    
    #' @field timeout Request timeout in seconds
    timeout = NULL,
    
    #' @field headers Additional headers
    headers = NULL,
    
    #' @description Create new options
    #' @param host Server hostname
    #' @param port Server port (default: 443)
    #' @param use_tls Use HTTPS (default: TRUE)
    #' @param timeout Timeout in seconds (default: 30)
    #' @param headers Additional headers (default: list())
    initialize = function(host, port = 443, use_tls = TRUE, timeout = 30, headers = list()) {
      self$host <- host
      self$port <- port
      self$use_tls <- use_tls
      self$timeout <- timeout
      self$headers <- headers
    },
    
    #' @description Get the base URL
    #' @return Base URL string
    base_url = function() {
      scheme <- if (self$use_tls) "https" else "http"
      paste0(scheme, "://", self$host, ":", self$port)
    },
    
    #' @description Create new options with different timeout
    #' @param timeout New timeout value
    #' @return New GreyOptions
    with_timeout = function(timeout) {
      GreyOptions$new(
        host = self$host,
        port = self$port,
        use_tls = self$use_tls,
        timeout = timeout,
        headers = self$headers
      )
    },
    
    #' @description Create new options with additional headers
    #' @param new_headers Headers to merge
    #' @return New GreyOptions
    with_headers = function(new_headers) {
      merged <- c(self$headers, new_headers)
      GreyOptions$new(
        host = self$host,
        port = self$port,
        use_tls = self$use_tls,
        timeout = self$timeout,
        headers = merged
      )
    }
  )
)

#' Create options for local development
#'
#' @param port Local server port (default: 8080)
#' @return GreyOptions
#' @export
GreyOptions$local <- function(port = 8080) {
  GreyOptions$new(
    host = "localhost",
    port = port,
    use_tls = FALSE,
    timeout = 30
  )
}

#' Create options for production
#'
#' @param host Production server hostname
#' @param port Server port (default: 443)
#' @return GreyOptions
#' @export
GreyOptions$production <- function(host, port = 443) {
  GreyOptions$new(
    host = host,
    port = port,
    use_tls = TRUE,
    timeout = 30
  )
}
