#' @title Benchmark Score Result Object
#'
#' @description An R6 class for non-aggregated, resampling-based benchmark results.
#'
#' @details This class is used as a container of benchmarking results where
#' multiple learners (models) have been tested against multiple tasks (datasets)
#' using a resampling scheme. The results stored are the per-resampling
#' performance scores, so multiple values per learner-task combination.
#'
#' The class can either be constructed using \CRANpkg{mlr3} objects, for example
#' the result of [mlr3::BenchmarkResult]`$score` or via [as_benchmark_score], or
#' by passing in a custom dataset of results. Custom datasets must include at
#' the very least, a factor column for learner ids, a factor column for task ids,
#' a numeric column for the resampling iterations (e.g. 1,2,3,...) and numeric
#' columns for one or more measures.
#'
#' @examples
#' # Not restricted to mlr3 objects
#' df = data.frame(
#'   tasks     = factor(rep(c("A", "B"), each = 25)),
#'   learners  = factor(rep(paste0("L", 1:5), each = 5)),
#'   iters     = 1:5,
#'   RMSE      = runif(50),
#'   MAE       = runif(50)
#' )
#' BenchmarkScore$new(df, task_id = "tasks", learner_id = "learners", iteration = "iters")
#' # equivalently
#' as_benchmark_score(df, task_id = "tasks", learner_id = "learners", iteration = "iters")
#'
#' if (requireNamespaces(c("mlr3", "rpart"))) {
#'   library(mlr3)
#'   tasks = tsks(c("boston_housing", "mtcars"))
#'   learners = lrns(c("regr.featureless", "regr.rpart"))
#'   bm = benchmark(benchmark_grid(tasks, learners, rsmp("cv", folds = 3)))
#'
#'   # coercion from mlr3 BenchmarkResult object
#'   as_benchmark_score(bm)
#' }
#' @export
BenchmarkScore = R6Class("BenchmarkScore",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param dt `(matrix(1))`\cr
    #' A `matrix` like object coercable to [data.table::data.table][data.table],
    #' should include column names "task_id", "learner_id", "iteration" and at
    #' least one measure (numeric).
    #' @param task_id (`factor(1)`) \cr
    #' String specifying name of task id column.
    #' @param learner_id (`factor(1)`)\cr
    #' String specifying name of learner id column.
    #' @param iteration (`character(1)`)\cr
    #' String specifying name of iteration id column.
    #' @param strip_prefix (`logical(1)`) \cr
    #' If `TRUE` (default) then mlr prefixes, e.g. `regr.`, `classif.`, are automatically
    #' stripped from the `learner_id` and other columns (i.e. representing measure ids).
    #' @param ... `ANY` \cr
    #' Additional arguments, currently unused.
    initialize = function(dt, task_id = "task_id", learner_id = "learner_id",
      iteration = "iteration", strip_prefix = TRUE, ...) {

      if (!is.data.table(dt)) {
        dt = as.data.table(dt)
      }

      assert_flag(strip_prefix)
      assert_subset(c(task_id, learner_id, iteration), colnames(dt))
      assert_factor(unlist(subset(dt, select = task_id)))
      assert_factor(unlist(subset(dt, select = learner_id)))
      assert_numeric(unlist(subset(dt, select = iteration)))

      private$.col_roles = list(
        task_id = task_id,
        learner_id = learner_id,
        iteration = iteration
      )
      # exclude column names that are part of `BenchmarkResult$score()`
      measure_ids = setdiff(colnames(dt), c(task_id, learner_id, iteration,
        "uhash", "nr", "task", "learner", "resampling", "resampling_id",
        "prediction"))

      if (length(measure_ids) == 0L) {
        stop("At least one measure must be included in `dt`.")
      }

      dt = dt[, c(task_id, learner_id, iteration, measure_ids), with = FALSE]
      # confirm all measures numeric
      assert_data_frame(dt[, measure_ids, with = FALSE], types = "numeric")
      # confirm at least 2 iterations
      niters = length(unique(dt[[iteration]]))
      if (niters < 2) {
        stop("Less than two resamplings - better use BenchmarkAggr()")
      }

      # data checks
      if (anyDuplicated(dt, by = c(task_id, learner_id, iteration))) {
        stop("Multiple results for a learner-task-iteration combination detected. There should be exactly one row for each learner-task-iteration combination.") # nolint
      }

      count_dt = dt[, .(ucount = uniqueN(.SD)), by = .(
        task    = get(private$.col_roles$task_id),
        learner = get(private$.col_roles$learner_id))]

      if (!all(count_dt[["ucount"]] == niters)) {
        stop("For every task and learner combination there should be exactly the same number of iterations")
      }

      if (strip_prefix) {
        if (isNamespaceLoaded("mlr3")) {
          types = mlr3::mlr_reflections$task_types$type
        } else {
          types = c("regr", "classif", "surv", "dens", "clust")
        }
        pattern = sprintf("^(%s)\\.", paste0(types, collapse = "|"))
        levels(dt[[learner_id]]) = gsub(pattern, "", levels(dt[[learner_id]]))
        colnames(dt) = gsub(pattern, "", colnames(dt))
      }

      private$.dt = dt

      invisible(self)
    },

    #' @description
    #' Prints the internal data via [data.table::print.data.table].
    #' @param ... `ANY` \cr Passed to [data.table::print.data.table].
    print = function(...) {
      catf("<BenchmarkScore> of %i %s with %i %s, %i %s, %i resampling iterations and %i %s",
        self$nrow, ifelse(self$nrow == 1, "row", "rows"),
        self$ntasks, ifelse(self$ntasks == 1, "task", "tasks"),
        self$nlrns, ifelse(self$nlrns == 1, "learner", "learners"),
        self$niters,
        self$nmeas, ifelse(self$nmeas == 1, "measure", "measures"))
      print(private$.dt, ...)
    },

    #' @description
    #' Prints the internal data via [data.table::print.data.table].
    #' @param ... `ANY` \cr Passed to [data.table::print.data.table].
    summary = function(...) {
      self$print(...)
    },

    #' @description Subsets the data by given tasks, learners and iterations.
    #' Returns data as [data.table::data.table].
    #' @param tasks (`character()`) \cr
    #' Task(s) to subset the data by.
    #' @param learners (`character()`) \cr
    #' Learner(s) to subset the data by.
    #' @param iterations (`numeric()`) \cr
    #' Resampling iteration(s) to subset the data by.
    subset = function(tasks = NULL, learners = NULL, iterations = NULL) {
      dt = private$.dt

      if (!is.null(tasks)) {
        rows = dt[[self$col_roles$task_id]] %in% tasks
        dt = dt[rows]
      }
      if (!is.null(learners)) {
        rows = dt[[self$col_roles$learner_id]] %in% learners
        dt = dt[rows]
      }
      if (!is.null(iterations)) {
        rows = dt[[self$col_roles$iteration]] %in% iterations
        dt = dt[rows]
      }

      dt
    }
  ),

  active = list(
    #' @field data ([data.table::data.table]) \cr Resampled scores data.
    data = function() private$.dt,
    #' @field learners `(character())` \cr Unique learner names.
    learners = function() levels(private$.dt[[self$col_roles$learner_id]]),
    #' @field tasks `(character())` \cr Unique task names.
    tasks = function() levels(private$.dt[[self$col_roles$task_id]]),
    #' @field iterations `(numeric())` \cr Unique resampling iterations.
    iterations = function() unique(private$.dt[[self$col_roles$iteration]]),
    #' @field measures `(character())` \cr Unique measure names.
    measures = function() setdiff(colnames(private$.dt), unlist(self$col_roles)),
    #' @field nlrns `(integer())` \cr Number of learners.
    nlrns = function() length(self$learners),
    #' @field ntasks `(integer())` \cr Number of tasks.
    ntasks = function() length(self$tasks),
    #' @field niters `(integer()` \cr Number of resampling iterations.
    niters = function() length(self$iterations),
    #' @field nmeas `(integer())` \cr Number of measures.
    nmeas = function() length(self$measures),
    #' @field nrow `(integer())` \cr Number of rows.
    nrow = function() nrow(self$data),
    #' @field col_roles (`character()`) \cr
    #' Column roles, currently cannot be changed after construction.
    col_roles = function() {
      private$.col_roles
    }
  ),

  private = list(
    .col_roles = character(0),
    .dt = data.table()
  )
)

#' @title Coercions to BenchmarkScore
#'
#' @description Coercion methods to [BenchmarkScore].
#' For [mlr3::BenchmarkResult] this is a simple wrapper around the
#' [BenchmarkScore] constructor called with [mlr3::BenchmarkResult]`$score()`.
#'
#' @param obj ([mlr3::BenchmarkResult]|`matrix(1)`) \cr Passed to [BenchmarkScore]`$new()`.
#' @param task_id,learner_id,iteration,strip_prefix See [BenchmarkScore]`$initialize()`.
#' @param ... `ANY` \cr Passed to [mlr3::BenchmarkResult]`$score()`.
#' @examples
#' # From a data.frame object
#' df = data.frame(
#'   tasks     = factor(rep(c("A", "B"), each = 25)),
#'   learners  = factor(rep(paste0("L", 1:5), each = 5)),
#'   iters     = 1:5,
#'   RMSE      = runif(50),
#'   MAE       = runif(50)
#' )
#' bms = as_benchmark_score(df, task_id = "tasks", learner_id = "learners", iteration = "iters")
#'
#' if (requireNamespaces(c("mlr3", "rpart"))) {
#'   library(mlr3)
#'   tasks = tsks(c("boston_housing", "mtcars"))
#'   learners = lrns(c("regr.featureless", "regr.rpart"))
#'   bm = benchmark(benchmark_grid(tasks, learners, rsmp("cv", folds = 3)))
#'
#'   # default measure
#'   as_benchmark_score(bm)
#'
#'   # change measure
#'   as_benchmark_score(bm, measures = msrs(c("regr.rmse", "regr.mae")))
#' }
#'
#' @export
as_benchmark_score = function(obj, task_id = "task_id", learner_id = "learner_id",
  iteration = "iteration", strip_prefix = TRUE, ...) {
  UseMethod("as_benchmark_score", obj)
}

#' @export
as_benchmark_score.default = function(obj, task_id = "task_id",
  learner_id = "learner_id", iteration = "iteration", strip_prefix = TRUE, ...) {
  BenchmarkScore$new(obj, task_id = task_id, learner_id = learner_id,
    iteration = iteration, strip_prefix = strip_prefix)
}

#' @export
as_benchmark_score.BenchmarkResult = function(obj, task_id = "task_id",
  learner_id = "learner_id", iteration = "iteration", strip_prefix = TRUE,
  measures = NULL, ...) {
  requireNamespaces("mlr3")
  measures = mlr3::as_measures(measures, task_type = obj$task_type)
  tab = obj$score(measures = measures)
  cols = c("task_id", "learner_id", "iteration", map_chr(measures, "id"))
  tab$task_id = factor(tab$task_id, levels = unique(tab$task_id))
  tab$learner_id = factor(tab$learner_id, levels = unique(tab$learner_id))
  BenchmarkScore$new(tab[, cols, with = FALSE], strip_prefix = strip_prefix)
}
