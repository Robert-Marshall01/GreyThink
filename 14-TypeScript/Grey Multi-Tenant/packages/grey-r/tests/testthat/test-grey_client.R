test_that("GreyClient creates with options", {
  client <- GreyClient$new(GreyOptions$local(8080))
  
  expect_s3_class(client, "GreyClient")
  expect_s3_class(client$auth, "AuthClient")
  expect_s3_class(client$user, "UserClient")
  expect_s3_class(client$projects, "ProjectsClient")
  expect_s3_class(client$query, "QueryClient")
  expect_s3_class(client$mutation, "MutationClient")
})

test_that("GreyClient$local creates local client", {
  client <- GreyClient$local(3000)
  
  expect_s3_class(client, "GreyClient")
  expect_equal(client$get_options()$host, "localhost")
  expect_equal(client$get_options()$port, 3000)
})

test_that("GreyClient$production creates production client", {
  client <- GreyClient$production("api.grey.com")
  
  expect_s3_class(client, "GreyClient")
  expect_equal(client$get_options()$host, "api.grey.com")
  expect_true(client$get_options()$use_tls)
})

test_that("is_authenticated returns false initially", {
  client <- GreyClient$local()
  
  expect_false(client$is_authenticated())
})
