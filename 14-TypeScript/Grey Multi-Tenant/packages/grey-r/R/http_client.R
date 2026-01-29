#' @title HTTP Client
#' @description Shared HTTP client for the Grey SDK using httr
#' @name HttpClient
NULL

#' @importFrom httr GET POST PUT PATCH DELETE content status_code add_headers timeout
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom R6 R6Class
#' @export
HttpClient <- R6::R6Class(
  "HttpClient",
  
  public = list(
    #' @field options GreyOptions instance
    options = NULL,
    
    #' @field access_token Current access token
    access_token = NULL,
    
    #' @description Create a new HTTP client
    #' @param options GreyOptions instance
    initialize = function(options) {
      self$options <- options
      self$access_token <- NULL
    },
    
    #' @description Set the access token
    #' @param token Access token string
    set_token = function(token) {
      self$access_token <- token
    },
    
    #' @description Clear the access token
    clear_token = function() {
      self$access_token <- NULL
    },
    
    #' @description Make a GET request
    #' @param path Request path
    #' @param params Query parameters (optional)
    #' @return grey_result
    get = function(path, params = NULL) {
      url <- private$build_url(path)
      private$execute("GET", url, params = params)
    },
    
    #' @description Make a POST request
    #' @param path Request path
    #' @param body Request body (list)
    #' @return grey_result
    post = function(path, body = list()) {
      url <- private$build_url(path)
      private$execute("POST", url, body = body)
    },
    
    #' @description Make a PUT request
    #' @param path Request path
    #' @param body Request body (list)
    #' @return grey_result
    put = function(path, body = list()) {
      url <- private$build_url(path)
      private$execute("PUT", url, body = body)
    },
    
    #' @description Make a PATCH request
    #' @param path Request path
    #' @param body Request body (list)
    #' @return grey_result
    patch = function(path, body = list()) {
      url <- private$build_url(path)
      private$execute("PATCH", url, body = body)
    },
    
    #' @description Make a DELETE request
    #' @param path Request path
    #' @param params Query parameters (optional)
    #' @return grey_result
    delete = function(path, params = NULL) {
      url <- private$build_url(path)
      private$execute("DELETE", url, params = params)
    }
  ),
  
  private = list(
    build_url = function(path) {
      paste0(self$options$base_url(), path)
    },
    
    build_headers = function() {
      headers <- c(
        "Content-Type" = "application/json",
        "Accept" = "application/json"
      )
      
      # Add custom headers from options
      if (length(self$options$headers) > 0) {
        headers <- c(headers, self$options$headers)
      }
      
      # Add authorization header if token exists
      if (!is.null(self$access_token)) {
        headers <- c(headers, "Authorization" = paste("Bearer", self$access_token))
      }
      
      do.call(httr::add_headers, as.list(headers))
    },
    
    execute = function(method, url, body = NULL, params = NULL) {
      tryCatch({
        headers <- private$build_headers()
        timeout_val <- httr::timeout(self$options$timeout)
        
        response <- switch(method,
          "GET" = httr::GET(url, headers, timeout_val, query = params),
          "POST" = httr::POST(url, headers, timeout_val, 
                              body = jsonlite::toJSON(body, auto_unbox = TRUE),
                              encode = "raw"),
          "PUT" = httr::PUT(url, headers, timeout_val,
                            body = jsonlite::toJSON(body, auto_unbox = TRUE),
                            encode = "raw"),
          "PATCH" = httr::PATCH(url, headers, timeout_val,
                                body = jsonlite::toJSON(body, auto_unbox = TRUE),
                                encode = "raw"),
          "DELETE" = httr::DELETE(url, headers, timeout_val, query = params)
        )
        
        private$handle_response(response)
        
      }, error = function(e) {
        msg <- e$message
        
        if (grepl("timeout", msg, ignore.case = TRUE)) {
          result_err(error_timeout(msg))
        } else if (grepl("connection|connect|network", msg, ignore.case = TRUE)) {
          result_err(error_network(msg))
        } else {
          result_err(error_unknown(msg))
        }
      })
    },
    
    handle_response = function(response) {
      status <- httr::status_code(response)
      body <- private$parse_body(response)
      
      if (status >= 200 && status < 300) {
        result_ok(body)
      } else {
        error <- error_from_response(status, body)
        result_err(error)
      }
    },
    
    parse_body = function(response) {
      content_text <- httr::content(response, as = "text", encoding = "UTF-8")
      
      if (is.null(content_text) || nchar(content_text) == 0) {
        return(list())
      }
      
      tryCatch({
        jsonlite::fromJSON(content_text, simplifyVector = FALSE)
      }, error = function(e) {
        list(raw = content_text)
      })
    }
  )
)
