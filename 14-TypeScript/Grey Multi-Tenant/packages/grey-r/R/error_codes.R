#' @title Error Codes
#' @description Error code constants for the Grey SDK
#' @name error_codes
NULL

#' Error code constants
#' @export
ERROR_CODES <- list(
  UNAUTHORIZED = "unauthorized",
  FORBIDDEN = "forbidden",
  NOT_FOUND = "not_found",
  VALIDATION_ERROR = "validation_error",
  NETWORK_ERROR = "network_error",
  TIMEOUT = "timeout",
  SERVER_ERROR = "server_error",
  UNKNOWN = "unknown"
)

#' Convert HTTP status code to error code
#'
#' @param status HTTP status code
#' @return Error code string
#' @export
error_code_from_status <- function(status) {
  if (status == 401) return(ERROR_CODES$UNAUTHORIZED)
  if (status == 403) return(ERROR_CODES$FORBIDDEN)
  if (status == 404) return(ERROR_CODES$NOT_FOUND)
  if (status %in% c(400, 422)) return(ERROR_CODES$VALIDATION_ERROR)
  if (status %in% c(408, 504)) return(ERROR_CODES$TIMEOUT)
  if (status >= 500 && status < 600) return(ERROR_CODES$SERVER_ERROR)
  return(ERROR_CODES$UNKNOWN)
}

#' Check if an error code is valid
#'
#' @param code Error code to check
#' @return TRUE if valid, FALSE otherwise
#' @export
is_valid_error_code <- function(code) {
  code %in% unlist(ERROR_CODES)
}

#' Normalize an error code
#'
#' @param code Error code to normalize
#' @return Valid error code
#' @export
normalize_error_code <- function(code) {
  if (is_valid_error_code(code)) code else ERROR_CODES$UNKNOWN
}
