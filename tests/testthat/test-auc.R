testthat::context("Testing HandTill2001::auc()")
testthat::test_that("binary", {
  data(ht01.twoclass, package = "HandTill2001")
  object <- HandTill2001::bincap(response = as.factor(ht01.twoclass$observed),
                                 predicted = ht01.twoclass$predicted,
                                 true = "1") 
  result <- HandTill2001::auc(object)
  expectation <- 0.802607561929596
  testthat:::expect_equal(result, expectation)
})

testthat::test_that("multiple", {
  data(ht01.multipleclass, package = "HandTill2001")
  object <- HandTill2001::multcap(response = ht01.multipleclass$observed, 
                                  predicted = as.matrix(ht01.multipleclass[, 
                                    levels(ht01.multipleclass$observed)]))
  result <- HandTill2001::auc(object)
  expectation <- 0.947211779448622
  testthat:::expect_equal(result, expectation)
})
