test_that("ProjectsClient create_project validates name", {
  client <- GreyClient$local()
  result <- client$projects$create_project(name = "")
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
  expect_match(result$error$message, "name", ignore.case = TRUE)
})

test_that("ProjectsClient get_project validates id", {
  client <- GreyClient$local()
  result <- client$projects$get_project(project_id = "")
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
})

test_that("ProjectsClient update_project validates id", {
  client <- GreyClient$local()
  result <- client$projects$update_project(project_id = "", data = list(name = "New"))
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
})

test_that("ProjectsClient update_project validates data", {
  client <- GreyClient$local()
  result <- client$projects$update_project(project_id = "123", data = list())
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
})

test_that("ProjectsClient delete_project validates id", {
  client <- GreyClient$local()
  result <- client$projects$delete_project(project_id = "")
  
  expect_true(is_err(result))
  expect_equal(result$error$code, "validation_error")
})
