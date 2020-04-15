if (requireNamespace("testthat", quietly = TRUE)) {
  library(checkmate)
  library(testthat)
  library(mlr3benchmark)

  test_check("mlr3benchmark")
}
