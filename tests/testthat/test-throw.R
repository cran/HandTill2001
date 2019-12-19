testthat::context("Testing HandTill2001:::throw()")
testthat::test_that("throw the HandTill2001 exception", {
  error_message <- "hello, testthat"
  string <- "hello, testthat"
  testthat::expect_error(
    HandTill2001:::throw(string),
    regexp = error_message,
    class = c("error", "HandTill2001", "condition")
  )
})
