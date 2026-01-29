test_that("grey_error creates correct structure", {
  err <- grey_error("unauthorized", "Access denied", list(user_id = 123))
  
  expect_s3_class(err, "grey_error")
  expect_equal(err$code, "unauthorized")
  expect_equal(err$message, "Access denied")
  expect_equal(err$details$user_id, 123)
})

test_that("grey_error normalizes invalid codes", {
  err <- grey_error("invalid_code", "Some error")
  
  expect_equal(err$code, "unknown")
})

test_that("is_grey_error works", {
  err <- grey_error("not_found", "Missing")
  
  expect_true(is_grey_error(err))
  expect_false(is_grey_error(list(code = "error")))
})

test_that("error factory functions work", {
  expect_equal(error_unauthorized()$code, "unauthorized")
  expect_equal(error_forbidden()$code, "forbidden")
  expect_equal(error_not_found()$code, "not_found")
  expect_equal(error_validation("Bad input")$code, "validation_error")
  expect_equal(error_network()$code, "network_error")
  expect_equal(error_timeout()$code, "timeout")
  expect_equal(error_server()$code, "server_error")
  expect_equal(error_unknown()$code, "unknown")
})

test_that("error_from_response works", {
  err <- error_from_response(404, list(message = "User not found"))
  
  expect_equal(err$code, "not_found")
  expect_equal(err$message, "User not found")
})
