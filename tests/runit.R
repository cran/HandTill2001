#!/usr/bin/Rscript --vanilla
is_failure <- function(result) {
  res <- RUnit::getErrors(result)
  names(res) <- tolower(names(res)) # soothe lintr
  sum_of_exceptions <- res[["nerr"]] + res[["nfail"]]
  fail <- as.logical(sum_of_exceptions)
  return(fail)
}

if (interactive()) {
  pkgload::load_all(path = ".") # needed to use pkgload's shim version of
  # base's system.file
  unit_dir <- system.file("inst", "runit_tests", package = "HandTill2001")
} else {
  require("HandTill2001", quietly = TRUE, character.only = TRUE) ||
    pkgload::load_all(path = ".") # needed to use pkgload's shim version of
  r_call <- commandArgs(trailingOnly = FALSE)
  if (any(grepl("--file", r_call))) {
    unit_dir <- file.path("inst", "runit_tests")
  } else {
    unit_dir <- system.file("runit_tests", package = "HandTill2001")
  }
}
if (!dir.exists(unit_dir)) {
  stop(
    "Can not find RUnit test directory ", unit_dir,
    ". Try to (re)install the package first."
  )
}
package_suite <- RUnit::defineTestSuite("HandTill2001_unit_test",
  dirs = unit_dir,
  testFileRegexp = "^.*\\.[rR]",
  testFuncRegexp = "^test_+"
)
test_result <- RUnit::runTestSuite(package_suite)

root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package),
  error = function(e) {
    return(NULL)
  }
)
if (!is.null(root)) {
  log_dir <- file.path(root, "log")
  dir.create(log_dir, showWarnings = FALSE)
  file_name <- file.path(log_dir, "runit.log")
  html_file <- file.path(log_dir, "runit.html")
  RUnit::printHTMLProtocol(test_result, fileName = html_file)
  if (interactive()) {
    browseURL(paste0("file:", html_file))
  }
} else {
  file_name <- ""
}
RUnit::printTextProtocol(test_result, showDetails = TRUE, fileName = file_name)
if (is_failure(test_result)) {
  RUnit::printTextProtocol(test_result, showDetails = TRUE)
  stop("RUnit failed.")
}
