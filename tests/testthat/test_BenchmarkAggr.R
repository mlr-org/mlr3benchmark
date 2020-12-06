test_that("construction", {
  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = runif(10))
  expect_silent(BenchmarkAggr$new(df))
  expect_warning(BenchmarkAggr$new(df, independent = FALSE), "independent datasets")

  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("regr.", 1:5),
                  regr.rmse = runif(10), stringsAsFactors = FALSE)
  expect_equal(BenchmarkAggr$new(df)$learners, as.character(1:5))
  expect_equal(BenchmarkAggr$new(df)$measures, "rmse")

  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("regr.", 1:5),
                  regr.rmse = runif(10), stringsAsFactors = FALSE)
  expect_equal(BenchmarkAggr$new(df, strip_prefix = FALSE)$learners, unique(df$learner_id))
  expect_equal(BenchmarkAggr$new(df, strip_prefix = FALSE)$measures, "regr.rmse")

  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5))
  expect_error(BenchmarkAggr$new(df), "At least one")

  df = data.frame(task_id = rep(c("A"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = runif(5))
  expect_warning(as.BenchmarkAggr(df), "multiple tasks")

  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = runif(20))
  expect_error(as.BenchmarkAggr(df), "combination")
})

test_that("public methods", {
  set.seed(1)
  rmse = round(runif(10, 1, 5))
  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = rmse)
  ba = BenchmarkAggr$new(df)
  expect_output(print(ba), "10 rows with 2 tasks, 5 learners and 1 measure")
  expect_output(ba$summary(), "10 rows with 2 tasks, 5 learners and 1 measure")
  expect_equal(as.numeric(ba$rank_data(ties.method = "first")), c(1, 2, 4, 5, 3, 4, 5, 2, 3, 1))
  expect_silent(ba$friedman_test())

  skip_if_not_installed("PMCMR")
  expect_warning(ba$friedman_posthoc(), "Cannot reject")
  expect_silent(ba$friedman_posthoc(p.value = 0.9))
})


test_that("active bindings", {
  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = round(runif(10, 1, 5)))
  ba = BenchmarkAggr$new(df)
  expect_equal(class(ba$data)[1], "data.table")
  expect_equal(ba$learners, paste0("L", 1:5))
  expect_equal(ba$tasks, c("A", "B"))
  expect_equal(ba$measures, "RMSE")
  expect_equal(ba$nlrns, 5)
  expect_equal(ba$nmeas, 1)
  expect_equal(ba$nrow, 10)
  expect_equal(ba$ntasks, 2)
})

test_that("mlr3 coercions", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("rpart")

  library(mlr3)
  task = tsks(c("boston_housing", "mtcars"))
  learns = lrns(c("regr.featureless", "regr.rpart"))
  bm = benchmark(benchmark_grid(task, learns, rsmp("holdout")))
  expect_equal(class(as.BenchmarkAggr(bm))[1], "BenchmarkAggr")
  expect_equal(class(as.BenchmarkAggr(bm, meas = msr("regr.mae")))[1], "BenchmarkAggr")
  aggr = bm$aggregate(msrs(c("regr.rmse", "regr.mae")))
  ba = BenchmarkAggr$new(aggr)
  expect_equal(class(as.BenchmarkAggr(bm))[1], "BenchmarkAggr")
  expect_equal(ba$measures, c("rmse", "mae"))
})
