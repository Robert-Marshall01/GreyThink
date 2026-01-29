test_that("AuthClient login validates email", {
  client <- GreyClient$local()
  result <- client$auth$login(email = "", password = "secret")
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
})

test_that("AuthClient login validates password", {
  client <- GreyClient$local()
  result <- client$auth$login(email = "user@example.com", password = "")
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
})

test_that("AuthClient refresh validates token", {
  client <- GreyClient$local()
  result <- client$auth$refresh(refresh_token = "")
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
})
