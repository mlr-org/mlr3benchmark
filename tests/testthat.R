if (requireNamespace("testthat", quietly = TRUE)) {
  library("testthat")
  library("checkmate") # for more expect_*() functions
  library("mlr3benchmark")
  test_check("mlr3benchmark")
}
