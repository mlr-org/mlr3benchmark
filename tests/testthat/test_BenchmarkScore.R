test_that("construction", {
  df = data.frame(
    task_id    = factor(rep(c("A", "B"), each = 25)),
    learner_id = factor(rep(paste0("L", 1:5), each = 5)),
    iteration  = 1:5,
    RMSE       = runif(50)
  )
  expect_silent(BenchmarkScore$new(df))

  # check that at least one measure is included
  df2 = subset(df, select = c("task_id", "learner_id", "iteration"))
  expect_error(BenchmarkScore$new(df2), "At least one measure")

  df = data.frame(
    tasks     = rep(c("A", "B"), each = 25),
    learners  = rep(paste0("regr.", 1:5), each = 5),
    iters     = 1:5,
    regr.mae  = runif(50),
    regr.mse  = runif(50),
    stringsAsFactors = TRUE
  )

  # check different column names
  bms = BenchmarkScore$new(df, task_id = "tasks", learner_id = "learners",
    iteration = "iters")
  expect_equal(bms$learners, as.character(1:5))
  expect_equal(bms$measures, c("mae", "mse"))

  # check striping of mlr3 prefixes
  bms = BenchmarkScore$new(df, task_id = "tasks", learner_id = "learners",
    iteration = "iters", strip_prefix = FALSE)
  expect_equal(bms$learners, paste0("regr.", 1:5))
  expect_equal(bms$measures, c("regr.mae", "regr.mse"))

  # Same task-learner-iteration combination is not allowed
  df = data.frame(
    task_id    = c("A", "A", "B", "B", "B"),
    learner_id = rep("L1", each = 5),
    iteration  = c(1,2,1,2,2),
    RMSE       = runif(5),
    stringsAsFactors = TRUE
  )
  expect_error(BenchmarkScore$new(df), "combination")

  # Should have at least 2 iterations
  df = data.frame(
    task_id    = rep(c("A", "B"), each = 2),
    learner_id = rep(c("L1", "L2"), times = 2),
    iteration  = 1,
    RMSE       = runif(4),
    stringsAsFactors = TRUE
  )
  expect_error(BenchmarkScore$new(df), "Less than two resamplings")
})

test_that("public methods", {
  set.seed(1)
  rmse = round(runif(18, 1, 9))
  mse = rmse^2
  df = data.frame(
    tasks     = rep(c("A", "B"), each = 9),
    learners  = rep(paste0("L", 1:3), each = 3),
    iters     = 1:3,
    RMSE      = rmse,
    MSE       = mse,
    stringsAsFactors = TRUE
  )
  bms = BenchmarkScore$new(df, task_id = "tasks", learner_id = "learners",
    iteration = 'iters')

  # print()
  expect_output(print(bms),
    "18 rows with 2 tasks, 3 learners, 3 resampling iterations and 2 measures")
  # summary()
  expect_output(bms$summary(),
    "18 rows with 2 tasks, 3 learners, 3 resampling iterations and 2 measures")
  # subset()
  empty_dt = data.table(tasks = factor(), learners = factor(), iters = numeric(),
    RMSE = numeric(), MSE = numeric())
  expect_equal(bms$subset(), bms$data) # no subsetting
  expect_equal(bms$subset(tasks = c("A", "B")), bms$data)
  expect_equal(bms$subset(tasks = "C42"), empty_dt)
  expect_equal(bms$subset(learners = "L42"), empty_dt)
  expect_equal(bms$subset(iterations = 0), empty_dt)
  expect_equal(
    bms$subset(tasks = "A", learners = "L1", iterations = 1:2),
    data.table(tasks = factor("A"), learners = factor("L1"), iters = 1:2,
               RMSE = c(3,4), MSE = c(9,16))
  )
  expect_equal(
    bms$subset(learners = "L3", iterations = 2),
    data.table(tasks = factor(c("A", "B")), learners = factor(c("L3", "L3")),
               iters = c(2, 2), RMSE = c(6, 7), MSE = c(36, 49))
  )
  expect_equal(
    bms$subset(tasks = c("B", "C42"), learners = c("L1", "L42", "L3"), iterations = 3),
    data.table(tasks = factor("B"), learners = factor(c("L1", "L3")), iters = c(3,3),
               RMSE = c(2, 9), MSE = c(4, 81))
  )
})

test_that("active bindings", {
  df = data.frame(
    tasks     = factor(rep(c("A", "B"), each = 25)),
    learners  = factor(rep(paste0("L", 1:5), each = 5)),
    iteration = 1:5,
    RMSE      = runif(50),
    MSE       = runif(50)
  )
  bms = BenchmarkScore$new(df, task_id = "tasks", learner_id = "learners")

  expect_equal(class(bms$data)[1], "data.table")
  expect_equal(colnames(bms$data), colnames(df))
  expect_equal(bms$learners, paste0("L", 1:5))
  expect_equal(bms$tasks, c("A", "B"))
  expect_equal(bms$iterations, 1:5)
  expect_equal(bms$measures, c("RMSE", "MSE"))
  expect_equal(bms$col_roles$task_id, "tasks")
  expect_equal(bms$col_roles$learner_id, "learners")
  expect_equal(bms$col_roles$iteration, "iteration")
  expect_equal(bms$nlrns, 5)
  expect_equal(bms$ntasks, 2)
  expect_equal(bms$niters, 5)
  expect_equal(bms$nmeas, 2)
  expect_equal(bms$nrow, 50)
})

test_that("mlr3 coercions", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("rpart")

  library(mlr3)
  tasks = tsks(c("boston_housing", "mtcars"))
  learners = lrns(c("regr.featureless", "regr.rpart"))
  bm = benchmark(benchmark_grid(tasks, learners, rsmp("cv", folds = 2)))
  expect_equal(class(as_benchmark_score(bm))[1], "BenchmarkScore")
  expect_equal(class(as_benchmark_score(bm, measures = msr("regr.mae")))[1], "BenchmarkScore")
  tab = bm$score(measures = msrs(c("regr.rmse", "regr.mae")))
  tab$task_id = factor(tab$task_id)
  tab$learner_id = factor(tab$learner_id)
  bms = BenchmarkScore$new(tab)
  expect_equal(class(bms)[1], "BenchmarkScore")
  expect_equal(bms$measures, c("rmse", "mae"))
})
