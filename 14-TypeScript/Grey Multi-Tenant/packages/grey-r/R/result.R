#' @title Grey Result
#' @description Result type for Grey SDK operations
#' @name grey_result
NULL

#' Create a successful result
#'
#' @param data The success data
#' @return A grey_result object
#' @export
grey_result <- function(data) {
  result <- list(
    ok = TRUE,
    data = data,
    error = NULL
  )
  class(result) <- c("grey_result", "list")
  result
}

#' Create a successful result (alias)
#'
#' @param data The success data
#' @return A grey_result object
#' @export
result_ok <- function(data) {
 grey_result(data)
}

#' Create a failed result
#'
#' @param error The error (grey_error or will be converted)
#' @return A grey_result object
#' @export
result_err <- function(error) {
  if (!is_grey_error(error)) {
    error <- grey_error(ERROR_CODES$UNKNOWN, as.character(error))
  }
  
  result <- list(
    ok = FALSE,
    data = NULL,
    error = error
  )
  class(result) <- c("grey_result", "list")
  result
}

#' Check if a result is successful
#'
#' @param result A grey_result object
#' @return TRUE if successful
#' @export
is_ok <- function(result) {
  if (!inherits(result, "grey_result")) {
    stop("Expected a grey_result object")
  }
  isTRUE(result$ok)
}

#' Check if a result is an error
#'
#' @param result A grey_result object
#' @return TRUE if error
#' @export
is_err <- function(result) {
  if (!inherits(result, "grey_result")) {
    stop("Expected a grey_result object")
  }
  !isTRUE(result$ok)
}

#' Unwrap a result or stop with error
#'
#' @param result A grey_result object
#' @return The data if successful
#' @export
unwrap <- function(result) {
  if (!inherits(result, "grey_result")) {
    stop("Expected a grey_result object")
  }
  
  if (is_err(result)) {
    stop(paste("Unwrap failed:", result$error$message))
  }
  
  result$data
}

#' Unwrap a result or return default
#'
#' @param result A grey_result object
#' @param default Default value if error
#' @return The data or default
#' @export
unwrap_or <- function(result, default) {
  if (!inherits(result, "grey_result")) {
    stop("Expected a grey_result object")
  }
  
  if (is_ok(result)) result$data else default
}

#' Map the data if successful
#'
#' @param result A grey_result object
#' @param fn Function to apply to data
#' @return New grey_result
#' @export
result_map <- function(result, fn) {
  if (!inherits(result, "grey_result")) {
    stop("Expected a grey_result object")
  }
  
  if (is_err(result)) return(result)
  
  tryCatch({
    result_ok(fn(result$data))
  }, error = function(e) {
    result_err(error_unknown(e$message))
  })
}

#' Chain another operation if successful
#'
#' @param result A grey_result object
#' @param fn Function that returns a grey_result
#' @return grey_result from fn or original error
#' @export
result_then <- function(result, fn) {
  if (!inherits(result, "grey_result")) {
    stop("Expected a grey_result object")
  }
  
  if (is_err(result)) return(result)
  
  tryCatch({
    res <- fn(result$data)
    if (!inherits(res, "grey_result")) {
      result_ok(res)
    } else {
      res
    }
  }, error = function(e) {
    result_err(error_unknown(e$message))
  })
}

#' Handle error if failed
#'
#' @param result A grey_result object
#' @param fn Function that returns a grey_result
#' @return grey_result from fn or original success
#' @export
result_catch <- function(result, fn) {
  if (!inherits(result, "grey_result")) {
    stop("Expected a grey_result object")
  }
  
  if (is_ok(result)) return(result)
  
  tryCatch({
    res <- fn(result$error)
    if (!inherits(res, "grey_result")) {
      result_ok(res)
    } else {
      res
    }
  }, error = function(e) {
    result_err(error_unknown(e$message))
  })
}

#' Print method for grey_result
#'
#' @param x A grey_result object
#' @param ... Additional arguments (ignored)
#' @export
print.grey_result <- function(x, ...) {
  if (is_ok(x)) {
    cat("Result::Ok\n")
    print(x$data)
  } else {
    cat("Result::Err\n")
    cat(paste("  Code:", x$error$code, "\n"))
    cat(paste("  Message:", x$error$message, "\n"))
  }
  invisible(x)
}
