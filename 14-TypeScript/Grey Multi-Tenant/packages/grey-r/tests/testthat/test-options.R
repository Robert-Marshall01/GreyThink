test_that("GreyOptions$new creates options correctly", {
  opts <- GreyOptions$new(host = "api.example.com")
  
  expect_equal(opts$host, "api.example.com")
  expect_equal(opts$port, 443)
  expect_true(opts$use_tls)
  expect_equal(opts$timeout, 30)
  expect_equal(opts$headers, list())
})

test_that("GreyOptions$local creates local options", {
  opts <- GreyOptions$local(3000)
  
  expect_equal(opts$host, "localhost")
  expect_equal(opts$port, 3000)
  expect_false(opts$use_tls)
})

test_that("GreyOptions$production creates production options", {
  opts <- GreyOptions$production("api.grey.com")
  
  expect_equal(opts$host, "api.grey.com")
  expect_equal(opts$port, 443)
  expect_true(opts$use_tls)
})

test_that("base_url returns correct URL", {
  local <- GreyOptions$local(8080)
  prod <- GreyOptions$production("api.grey.com")
  
  expect_equal(local$base_url(), "http://localhost:8080")
  expect_equal(prod$base_url(), "https://api.grey.com:443")
})

test_that("with_timeout creates new options", {
  original <- GreyOptions$local()
  modified <- original$with_timeout(120)
  
  expect_equal(original$timeout, 30)
  expect_equal(modified$timeout, 120)
  expect_equal(modified$host, original$host)
})

test_that("with_headers merges headers", {
  original <- GreyOptions$new(host = "api.grey.com", headers = list("X-A" = "a"))
  modified <- original$with_headers(list("X-B" = "b"))
  
  expect_equal(length(original$headers), 1)
  expect_equal(length(modified$headers), 2)
  expect_equal(modified$headers$`X-A`, "a")
  expect_equal(modified$headers$`X-B`, "b")
})
