test_that("result_ok creates successful result", {
  result <- result_ok(list(id = 1, name = "Test"))
  
  expect_s3_class(result, "grey_result")
  expect_true(is_ok(result))
  expect_false(is_err(result))
  expect_equal(result$data$id, 1)
  expect_null(result$error)
})

test_that("result_err creates error result", {
  err <- grey_error("unauthorized", "Not logged in")
  result <- result_err(err)
  
  expect_s3_class(result, "grey_result")
  expect_false(is_ok(result))
  expect_true(is_err(result))
  expect_null(result$data)
  expect_equal(result$error$code, "unauthorized")
})

test_that("result_err wraps non-grey_error", {
  result <- result_err("Something went wrong")
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "unknown")
  expect_equal(result$error$message, "Something went wrong")
})

test_that("unwrap_or returns data or default", {
  ok <- result_ok(10)
  err <- result_err(error_not_found())
  
  expect_equal(unwrap_or(ok, 0), 10)
  expect_equal(unwrap_or(err, 0), 0)
})

test_that("result_map transforms data", {
  result <- result_ok(5)
  mapped <- result_map(result, function(x) x * 2)
  
  expect_true(is_ok(mapped))
  expect_equal(mapped$data, 10)
})

test_that("result_map passes through error", {
  err <- result_err(error_not_found())
  mapped <- result_map(err, function(x) x * 2)
  
  expect_true(is_err(mapped))
  expect_equal(mapped$error$code, "not_found")
})

test_that("result_then chains operations", {
  result <- result_ok(5)
  chained <- result_then(result, function(x) result_ok(x * 2))
  
  expect_true(is_ok(chained))
  expect_equal(chained$data, 10)
})

test_that("result_catch handles errors", {
  err <- result_err(error_not_found())
  caught <- result_catch(err, function(e) result_ok("recovered"))
  
  expect_true(is_ok(caught))
  expect_equal(caught$data, "recovered")
})

test_that("result_catch passes through success", {
  result <- result_ok("value")
  caught <- result_catch(result, function(e) result_ok("default"))
  
  expect_true(is_ok(caught))
  expect_equal(caught$data, "value")
})
