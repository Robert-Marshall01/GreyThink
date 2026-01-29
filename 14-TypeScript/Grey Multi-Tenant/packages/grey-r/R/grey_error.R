#' @title Grey Error
#' @description Normalized error structure for the Grey SDK
#' @name grey_error
NULL

#' Create a Grey error object
#'
#' @param code Error code
#' @param message Error message
#' @param details Additional error details (optional)
#' @return A grey_error object (list)
#' @export
grey_error <- function(code, message, details = NULL) {
  err <- list(
    code = normalize_error_code(code),
    message = as.character(message),
    details = details
  )
  class(err) <- c("grey_error", "list")
  err
}

#' Check if an object is a grey_error
#'
#' @param x Object to check
#' @return TRUE if x is a grey_error
#' @export
is_grey_error <- function(x) {
  inherits(x, "grey_error")
}

#' Create an unauthorized error
#'
#' @param message Custom message (optional)
#' @return grey_error
#' @export
error_unauthorized <- function(message = NULL) {
  grey_error(
    ERROR_CODES$UNAUTHORIZED,
    message %||% "Authentication required"
  )
}

#' Create a forbidden error
#'
#' @param message Custom message (optional)
#' @return grey_error
#' @export
error_forbidden <- function(message = NULL) {
  grey_error(
    ERROR_CODES$FORBIDDEN,
    message %||% "Permission denied"
  )
}

#' Create a not found error
#'
#' @param message Custom message (optional)
#' @return grey_error
#' @export
error_not_found <- function(message = NULL) {
  grey_error(
    ERROR_CODES$NOT_FOUND,
    message %||% "Resource not found"
  )
}

#' Create a validation error
#'
#' @param message Error message
#' @param details Validation details (optional)
#' @return grey_error
#' @export
error_validation <- function(message, details = NULL) {
  grey_error(
    ERROR_CODES$VALIDATION_ERROR,
    message,
    details
  )
}

#' Create a network error
#'
#' @param message Custom message (optional)
#' @return grey_error
#' @export
error_network <- function(message = NULL) {
  grey_error(
    ERROR_CODES$NETWORK_ERROR,
    message %||% "Network error occurred"
  )
}

#' Create a timeout error
#'
#' @param message Custom message (optional)
#' @return grey_error
#' @export
error_timeout <- function(message = NULL) {
  grey_error(
    ERROR_CODES$TIMEOUT,
    message %||% "Request timed out"
  )
}

#' Create a server error
#'
#' @param message Custom message (optional)
#' @return grey_error
#' @export
error_server <- function(message = NULL) {
  grey_error(
    ERROR_CODES$SERVER_ERROR,
    message %||% "Server error occurred"
  )
}

#' Create an unknown error
#'
#' @param message Custom message (optional)
#' @return grey_error
#' @export
error_unknown <- function(message = NULL) {
  grey_error(
    ERROR_CODES$UNKNOWN,
    message %||% "An unknown error occurred"
  )
}

#' Create error from HTTP response
#'
#' @param status HTTP status code
#' @param body Response body (list)
#' @return grey_error
#' @export
error_from_response <- function(status, body = NULL) {
  code <- error_code_from_status(status)
  message <- body$message %||% body$error %||% paste("HTTP error", status)
  details <- body$details
  
  grey_error(code, message, details)
}

#' Null-coalescing operator
#' @param a First value
#' @param b Default value if a is NULL
#' @return a if not NULL, otherwise b
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
