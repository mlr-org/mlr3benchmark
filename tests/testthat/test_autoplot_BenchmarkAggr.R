test_that("autoplot.BenchmarkAggr", {
  set.seed(1)
  df = data.frame(task_id = factor(rep(c("A", "B"), each = 100)),
                  learner_id = factor(paste0("L", 1:100)),
                  RMSE = runif(100, 0, 1:100))
  ba = BenchmarkAggr$new(df)

  expect_true(is.ggplot(autoplot(ba, type = "mean")))
  expect_true(is.ggplot(autoplot(ba, type = "box")))

  skip_if_not_installed("PMCMRplus")
  expect_true(is.ggplot(autoplot(ba, type = "fn")))
  expect_true(is.ggplot(autoplot(ba, type = "cd")))
})

test_that("autoplot.BenchmarkAggr cd", {
  skip_if_not_installed("PMCMRplus")

  set.seed(1)
  df = data.frame(task_id = factor(rep(c("A", "B"), each = 5)),
                  learner_id = factor(paste0("L", 1:5)),
                  RMSE = runif(5))
  ba = BenchmarkAggr$new(df)

  expect_error(is.ggplot(autoplot(ba, type = "fn")))
  expect_true(is.ggplot(expect_warning(autoplot(ba, type = "fn", friedman_global = FALSE))))
  expect_silent(is.ggplot(autoplot(ba, type = "fn", p.value = 1, test = "bd")))
  expect_error(is.ggplot(autoplot(ba, type = "cd")))
  expect_true(is.ggplot(expect_warning(autoplot(ba, type = "cd", friedman_global = FALSE))))
  expect_silent(is.ggplot(autoplot(ba, type = "cd", p.value = 1, test = "bd")))
  expect_silent(is.ggplot(autoplot(ba, type = "cd", p.value = 1, test = "bd", style = 2)))
})

test_that("autoplot with BenchmarkAggr from mlr3::benchmark()", {
  skip_if_not_installed("PMCMRplus")
  skip_if_not_installed("mlr3")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("rpart")
  set.seed(1)

  library("mlr3")

  task = tsks(c("iris", "sonar"))
  learns = c(lrns(c("classif.featureless", "classif.rpart")),
    lrn("classif.rpart", id = "rpart2")
  )
  bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 3)))
  ba = as_benchmark_aggr(bm)
  expect_warning(expect_true(ggplot2::is.ggplot(autoplot(ba,
    type = "cd",
    friedman_global = FALSE
  ))))
  expect_true(ggplot2::is.ggplot(autoplot(ba, type = "cd", p.value = 0.2)))
})
