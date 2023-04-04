test_that("construction", {
  df = data.frame(task_id = factor(rep(c("A", "B"), each = 5)),
                  learner_id = factor(paste0("L", 1:5)),
                  RMSE = runif(10))
  expect_silent(BenchmarkAggr$new(df))
  expect_warning(BenchmarkAggr$new(df, independent = FALSE), "independent datasets")

  df = data.frame(tasks = rep(c("A", "B"), each = 5),
                  learners = paste0("regr.", 1:5),
                  regr.rmse = runif(10), stringsAsFactors = TRUE)
  expect_equal(BenchmarkAggr$new(df, task_id = "tasks", learner_id = "learners")$learners,
               as.character(1:5))
  expect_equal(BenchmarkAggr$new(df, task_id = "tasks", learner_id = "learners")$measures, "rmse")

  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("regr.", 1:5),
                  regr.rmse = runif(10), stringsAsFactors = TRUE)
  expect_equal(BenchmarkAggr$new(df, strip_prefix = FALSE)$learners, as.character(unique(df$learner_id)))
  expect_equal(BenchmarkAggr$new(df, strip_prefix = FALSE)$measures, "regr.rmse")

  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  stringsAsFactors = TRUE)
  expect_error(BenchmarkAggr$new(df), "At least one")

  df = data.frame(task_id = rep(c("A"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = runif(5),
                  stringsAsFactors = TRUE)
  expect_warning(as_benchmark_aggr(df), "multiple tasks")

  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = runif(20),
                  stringsAsFactors = TRUE)
  expect_error(as_benchmark_aggr(df), "combination")
})

test_that("public methods", {
  set.seed(1)
  rmse = round(runif(10, 1, 5))
  mse = rmse^2
  df = data.frame(tasks = rep(c("A", "B"), each = 5),
                  learners = paste0("L", 1:5),
                  RMSE = rmse, MSE = mse,
                  stringsAsFactors = TRUE)
  ba = BenchmarkAggr$new(df, task_id = "tasks", learner_id = "learners")
  expect_output(print(ba), "10 rows with 2 tasks, 5 learners and 2 measures")
  expect_output(ba$summary(), "10 rows with 2 tasks, 5 learners and 2 measures")
  expect_error(as.numeric(ba$rank_data(ties.method = "first")), "Multiple")
  expect_equal(as.numeric(ba$rank_data(ties.method = "first", meas = "RMSE")),
               c(1, 2, 4, 5, 3, 4, 5, 2, 3, 1))
  expect_equal(as.numeric(ba$rank_data(task = "B", ties.method = "first", meas = "RMSE")),
               c(4, 5, 2, 3, 1))
  expect_equal(as.numeric(ba$rank_data(task = "B", ties.method = "first", minimize = FALSE,
                                       meas = "RMSE")),
               c(1, 2, 3, 4, 5))
  expect_is(ba$friedman_test(), "data.frame")
  expect_is(ba$friedman_test(meas = "RMSE"), "htest")
  expect_equal(ba$subset(task = "A", learner = "L1"),
               data.table(tasks = factor("A"), learners = factor("L1"), RMSE = 2, MSE = 4))
  expect_equal(ba$subset(learner = "L3"),
               data.table(tasks = factor(c("A", "B")), learners = factor(c("L3", "L3")),
                          RMSE = c(3, 4), MSE = c(9, 16)))

  skip_if_not_installed("PMCMRplus")
  expect_error(ba$friedman_posthoc(), "measures")
  expect_warning(ba$friedman_posthoc(meas = "RMSE"), "Returning overall")
  expect_equal(
    class(expect_warning(ba$friedman_posthoc(meas = "RMSE",
                                            friedman_global = FALSE),
                        "unreliable")), "PMCMR"
  )
  expect_silent(ba$friedman_posthoc(p.value = 0.9, meas = "RMSE"))
})


test_that("active bindings", {
  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = round(runif(10, 1, 5)),
                  stringsAsFactors = TRUE)
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
  expect_equal(class(as_benchmark_aggr(bm))[1], "BenchmarkAggr")
  expect_equal(class(as_benchmark_aggr(bm, meas = msr("regr.mae")))[1], "BenchmarkAggr")
  aggr = bm$aggregate(msrs(c("regr.rmse", "regr.mae")))
  aggr$task_id = factor(aggr$task_id)
  aggr$learner_id = factor(aggr$learner_id)
  ba = BenchmarkAggr$new(aggr)
  expect_equal(class(as_benchmark_aggr(bm))[1], "BenchmarkAggr")
  expect_equal(ba$measures, c("rmse", "mae"))
})
