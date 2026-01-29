test_that("error_code_from_status returns correct codes", {
  expect_equal(error_code_from_status(401), "unauthorized")
  expect_equal(error_code_from_status(403), "forbidden")
  expect_equal(error_code_from_status(404), "not_found")
  expect_equal(error_code_from_status(400), "validation_error")
  expect_equal(error_code_from_status(422), "validation_error")
  expect_equal(error_code_from_status(500), "server_error")
  expect_equal(error_code_from_status(418), "unknown")
})

test_that("is_valid_error_code works", {
  expect_true(is_valid_error_code("unauthorized"))
  expect_true(is_valid_error_code("forbidden"))
  expect_false(is_valid_error_code("invalid_code"))
})

test_that("normalize_error_code normalizes correctly", {
  expect_equal(normalize_error_code("forbidden"), "forbidden")
  expect_equal(normalize_error_code("invalid"), "unknown")
})
