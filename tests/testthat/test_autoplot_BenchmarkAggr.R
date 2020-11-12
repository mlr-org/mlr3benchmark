test_that("autoplot.BenchmarkAggr", {
  set.seed(1)
  df = data.frame(task_id = rep(c("A", "B"), each = 100),
                  learner_id = paste0("L", 1:100),
                  RMSE = runif(100, 0, 1:100))
  ba = BenchmarkAggr$new(df)

  expect_true(is.ggplot(autoplot(ba, type = "mean")))
  expect_true(is.ggplot(autoplot(ba, type = "box")))
  expect_true(is.ggplot(autoplot(ba, type = "fn")))
  expect_true(is.ggplot(autoplot(ba, type = "cd")))
})

test_that("autoplot.BenchmarkAggr cd", {
  set.seed(1)
  df = data.frame(task_id = rep(c("A", "B"), each = 5),
                  learner_id = paste0("L", 1:5),
                  RMSE = runif(5))
  ba = BenchmarkAggr$new(df)

  expect_error(is.ggplot(autoplot(ba, type = "fn")))
  expect_silent(is.ggplot(autoplot(ba, type = "fn", p.value = 1)))
  expect_error(is.ggplot(autoplot(ba, type = "cd")))
  expect_silent(is.ggplot(autoplot(ba, type = "cd", p.value = 1)))
})
