#' @title Aggregated Benchmark Result Object
#'
#' @description An R6 class for aggregated benchmark results.
#' @details This class is used to easily carry out and guide analysis of models after aggregating
#' the results after resampling. This can either be constructed using \CRANpkg{mlr3} objects,
#' for example the result of [mlr3::BenchmarkResult]`$aggregate` or via [as.BenchmarkAggr],
#' or by passing in a custom dataset of results. Custom datasets must include at the very least,
#' column names `learner_id` (for models) and `task_id` (for datasets).
#'
#' Currently supported for multiple independent datasets only.
#'
#' @references
#' `r format_bib("demsar_2006")
#'
#' @examples
#' # Not restricted to mlr3 objects
#' df = data.frame(task_id = rep(c("A", "B"), each = 5),
#'                 learner_id = paste0("L", 1:5),
#'                 RMSE = runif(10), MAE = runif(10))
#' BenchmarkAggr$new(df)
#'
#' if (requireNamespaces(c("mlr3", "rpart"))) {
#'   library(mlr3)
#'   task = tsks(c("boston_housing", "mtcars"))
#'   learns = lrns(c("regr.featureless", "regr.rpart"))
#'   bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 2)))
#'
#'   # coercion
#'   as.BenchmarkAggr(bm)
#'
#'   # initialize
#'   BenchmarkAggr$new(bm$aggregate())
#' }
#' @export
BenchmarkAggr = R6Class("BenchmarkAggr",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param dt `(matrix(1))` \cr
    #' `matrix` like object coercable to [data.table::data.table][data.table], should
    #' include column names "task_id" and "learner_id", and at least one measure (numeric).
    #' If ids are not already factors then coerced internally.
    #' @param independent `(logical(1))` \cr
    #' Are tasks independent of one another? Affects which tests can be used for analysis.
    #' @param strip_prefix (`logical(1)`) \cr
    #' If `TRUE` (default) then mlr prefixes, e.g. `regr.`, `classif.`, are automatically
    #' stripped from the `learner_id`.
    #' @param ... `ANY` \cr Additional arguments, currently unused.
    initialize = function(dt, independent = TRUE, strip_prefix = TRUE, ...) {
      dt = as.data.table(dt)

      private$.independent = assert_logical(independent)
      if (!independent) {
        warning("Currently only methods for independent datasets are supported.")
      }

      # at the very least should include task_id, learner_id, and one measure
      assert(all(c("task_id", "learner_id") %in% colnames(dt)))


      if (any(duplicated(dt[, c("task_id", "learner_id")]))) {
        stop("Multiple results for a learner-task combination detected. There should be exactly one row for each learner-task combination.") # nolint
      }

      # TODO - The line below could be removed if there is a use for this extra data,
      # for now there isn't in this object.
      dt = subset(dt, select = setdiff(colnames(dt),
                            c("nr", "task", "learner", "resampling", "resampling_id",
                              "iteration", "prediction", "resample_result", "iters")))

      if (ncol(dt) < 3) {
        stop("At least one measure must be included in `dt`.")
      }

      # check measures are numeric
      sapply(dt[, setdiff(colnames(dt), c("task_id", "learner_id")), with = FALSE], assert_numeric)

      if (strip_prefix) {
        if (!test_factor(dt$learner_id)) {
          dt$learner_id = gsub("regr\\.|classif\\.|surv\\.|dens\\.|clust\\.", "", dt$learner_id)
        }
        colnames(dt) = gsub("regr\\.|classif\\.|surv\\.|dens\\.|clust\\.", "", colnames(dt))
      }

      if (!test_factor(dt$task_id)) {
        dt$task_id = factor(assert_character(dt$task_id), levels = unique(dt$task_id))
      }

      if (!test_factor(dt$learner_id)) {
        dt$learner_id = factor(assert_character(dt$learner_id), levels = unique(dt$learner_id))
      }

      private$.dt = dt

      if (self$ntasks < 2) {
        warning("Currently only supported for multiple tasks.")
      }

      invisible(self)
    },

    #' @description
    #' Prints the internal data via [data.table::print.data.table].
    #' @param ... `ANY` \cr Passed to [data.table::print.data.table].
    print = function(...) {
      catf("<BenchmarkAggr> of %i %s with %i %s, %i %s and %i %s",
           self$nrow, ifelse(self$nrow == 1, "row", "rows"),
           self$ntasks, ifelse(self$ntasks == 1, "task", "tasks"),
           self$nlrns, ifelse(self$nlrns == 1, "learner", "learners"),
           self$nmeas, ifelse(self$nmeas == 1, "measure", "measures"))
      print(private$.dt, ...)
    },

    #' @description
    #' Prints the internal data via [data.table::print.data.table].
    #' @param ... `ANY` \cr Passed to [data.table::print.data.table].
    summary = function(...) {
      self$print(...)
    },

    #' @description Ranks the aggregated data given some measure.
    #' @param meas `(character(1))` \cr
    #' Measure to rank the data against, should be in `$measures`. Can be `NULL` if only one measure
    #' in data.
    #' @param minimize `(logical(1))` \cr
    #' Should the measure be minimized? Default is `TRUE`.
    #' @param task `(character(1))` \cr
    #' If `NULL` then returns a matrix of ranks where columns are tasks and rows are
    #' learners, otherwise returns a one-column matrix of a specified task, should
    #' be in `$tasks`.
    #' @param ... `ANY` `ANY` \cr Passed to [data.table::frank()].
    rank_data = function(meas = NULL, minimize = TRUE, task = NULL, ...) {
      meas = .check_meas(self, meas)
      df = subset(private$.dt, select = c("task_id", meas))
      lrns = self$learners
      nr = self$nlrns

      if (!minimize) {
        df[[meas]] = -df[[meas]]
      }

      if (!is.null(task)) {
        df = subset(df, task_id == task)
        rdf = matrix(data.table::frank(subset(df, select = meas), ...), ncol = 1)
        colnames(rdf) = task
      } else {
        tasks = self$tasks
        rdf = matrix(nrow = nr, ncol = self$ntasks)
        for (i in seq_along(tasks)) {
          rdf[, i] = data.table::frank(subset(df, task_id == tasks[[i]], select = meas), ...)
        }
        colnames(rdf) = tasks
      }

      rownames(rdf) = lrns
      rdf
    },

    #' @description Computes Friedman test over all tasks, assumes datasets are independent.
    #' @param meas `(character(1))` \cr
    #' Measure to rank the data against, should be in `$measures`. If no measure is provided
    #' then returns a matrix of tests for all measures.
    #' @param p.adjust.method `(character(1))` \cr
    #' Passed to [p.adjust] if `meas = NULL` for multiple testing correction. If `NULL`
    #' then no correction applied.
    friedman_test = function(meas = NULL, p.adjust.method = NULL) { # nolint

      if (self$nlrns < 2) {
        stop("At least two learners are required.")
      }

      if (self$ntasks < 2) {
        stop("At least two tasks are required")
      }

      if (!is.null(meas)) {
        return(friedman.test(as.formula(paste0(meas, " ~ learner_id | task_id", sep = "")),
                      data = private$.dt))
      } else {
        x = sapply(self$measures, function(x)
          friedman.test(as.formula(paste0(x, " ~ learner_id | task_id", sep = "")),
                       data = private$.dt))
        x = data.frame(t(x[1:3, ]))
        colnames(x) = c("X2", "df", "p.value")
        rownames(x) = self$measures

        if (!is.null(p.adjust.method)) {
          x$p.adj.value = p.adjust(x$p.value, p.adjust.method) # nolint
          x$p.signif = ifelse(x$p.adj.value <= 0.001, "***", # nolint
                              ifelse(x$p.adj.value <= 0.01, "**",
                                     ifelse(x$p.adj.value <= 0.05, "*",
                                            ifelse(x$p.adj.value <= 0.1, ".", " "))))
        } else {
          x$p.signif = ifelse(x$p.value <= 0.001, "***", # nolint
                              ifelse(x$p.value <= 0.01, "**",
                                     ifelse(x$p.value <= 0.05, "*",
                                            ifelse(x$p.value <= 0.1, ".", " "))))
        }
        return(x)
      }

    },

    #' @description Posthoc Friedman Nemenyi tests. Computed with
    #' [PMCMR::posthoc.friedman.nemenyi.test]. If global `$friedman_test` is non-significant then
    #' this is returned and no post-hocs computed. Also returns critical difference
    #' @param meas `(character(1))` \cr
    #' Measure to rank the data against, should be in `$measures`. Can be `NULL` if only one measure
    #' in data.
    #' @param p.value `(numeric(1))` \cr
    #' p.value for which the global test will be considered significant.
    friedman_posthoc = function(meas = NULL, p.value = 0.05) { # nolint

      if (!requireNamespace("PMCMR", quietly = TRUE)) {
        stop("Package PMCMR required for post-hoc Friedman tests.")
      }

      if (self$nlrns == 2) {
        warning("Only two learners available, returning global test.")
        self$friedman_test(meas)
      }

      meas = .check_meas(self, meas)
      assertNumeric(p.value, lower = 0, upper = 1, len = 1)
      f.test = self$friedman_test(meas) # nolint

      if (!is.na(f.test$p.value)) {
        f.rejnull = f.test$p.value < p.value # nolint
        if (!f.rejnull) {
          warning("Cannot reject null hypothesis of overall Friedman test, returning overall Friedman test.") # nolint
        }
      } else {
        f.rejnull = FALSE # nolint
        warning("P-value not computable. Learner performances might be exactly equal.")
      }

      if (f.rejnull) {
        form = as.formula(paste0(meas, " ~ learner_id | task_id", sep = ""))
        nem_test = PMCMR::posthoc.friedman.nemenyi.test(form, data = private$.dt) # nolint
        nem_test$f.rejnull = f.rejnull # nolint
        return(nem_test)
      } else {
        f.test$f.rejnull = f.rejnull # nolint
        return(f.test)
      }
    }
  ),

  active = list(
    #' @field data ([data.table::data.table]) \cr Aggregated data.
    data = function() private$.dt,
    #' @field learners `(character())` \cr Unique learner names.
    learners = function() as.character(unique(private$.dt$learner_id)),
    #' @field tasks `(character())` \cr Unique task names.
    tasks = function() as.character(unique(private$.dt$task_id)),
    #' @field measures `(character())` \cr Unique measure names.
    measures = function() {
      setdiff(colnames(private$.dt), c("task_id", "learner_id"))
    },
    #' @field nlrns `(integers())` \cr Number of learners.
    nlrns = function() length(self$learners),
    #' @field ntasks `(integers())` \cr Number of tasks.
    ntasks = function() length(self$tasks),
    #' @field nmeas `(integers())` \cr Number of measures.
    nmeas = function() length(self$measures),
    #' @field nrow `(integers())` \cr Number of rows.
    nrow = function() nrow(self$data)
  ),

  private = list(
    .dt = data.table(),
    .independent = logical(0),
    .crit_differences = function(meas = NULL, minimize = TRUE, p.value = 0.05, baseline = NULL, # nolint
                                test = c("bd", "nemenyi")) {

      meas = .check_meas(self, meas)
      test = match.arg(test)
      assertNumeric(p.value, lower = 0, upper = 1, len = 1)

      # Get Rankmatrix, transpose and get mean ranks
      mean_rank = rowMeans(self$rank_data(meas, minimize = minimize))
      # Gather Info for plotting the descriptive part.
      df = data.frame(mean_rank,
                      learner_id = names(mean_rank),
                      rank = rank(mean_rank, ties.method = "average"))
      # Orientation of descriptive lines yend(=y-value of horizontal line)
      right = df$rank > median(df$rank)
      # Better learners are ranked ascending
      df$yend[!right] = rank(df$rank[!right], ties.method = "first") + 1
      # Worse learners ranked descending
      df$yend[right] = rank(-df$rank[right], ties.method = "first") + 1
      # Better half of learner have lines to left / others right.
      df$xend = ifelse(!right, 0L, max(df$rank) + 1L)
      # Save orientation, can be used for vjust of text later on
      df$right = as.numeric(right)

      # Perform nemenyi test
      nem_test = tryCatch(self$friedman_posthoc(meas, p.value),
                   warning = function(w)
                     stopf("Global Friedman test non-significant (p > %s), try type = 'mean' instead.", p.value)) # nolint

      # calculate critical difference(s)
      if (test == "nemenyi") {
        cd = (qtukey(1 - p.value, self$nlrns, 1e+06) / sqrt(2L)) *
          sqrt(self$nlrns * (self$nlrns + 1L) / (6L * self$ntasks))
      } else {
        cd = (qtukey(1L - (p.value / (self$nlrns - 1L)), 2L, 1e+06) / sqrt(2L)) *
          sqrt(self$nlrns * (self$nlrns + 1L) / (6L * self$ntasks))
      }

      out = list(data = df, test = test, cd = cd)

      # Create data for connecting bars (only nemenyi test)
      if (test == "nemenyi") {
        sub = sort(df$mean_rank)
        # Compute a matrix of all possible bars
        mat = apply(t(outer(sub, sub, `-`)), c(1, 2),
                    FUN = function(x) ifelse(x > 0 && x < cd, x, 0))
        # Get start and end point of all possible bars
        xstart = round(apply(mat + sub, 1, min), 3)
        xend = round(apply(mat + sub, 1, max), 3)
        nem_df = data.table(xstart, xend, "diff" = xend - xstart) # nolint
        # For each unique endpoint of a bar keep only the longest bar
        nem_df = nem_df[, .SD[which.max(.SD$diff)], by = "xend"] # nolint
        # Take only bars with length > 0
        nem_df = nem_df[nem_df$diff > 0, ] # nolint
        # Descriptive lines for learners start at 0.5, 1.5, ...
        nem_df$y = c(seq(from = 0.5, by = 0.5,
                         length.out = sum(nem_df$xstart <= median(df$mean_rank))),
                     seq(from = 0.5, by = 0.5,
                         length.out = sum(nem_df$xstart > median(df$mean_rank))))

        out$nemenyi_data = as.data.frame(nem_df) # nolint
      } else {
        # Get a baseline
        if (is.null(baseline)) {
          baseline = as.character(df$learner_id[which.min(df$rank)])
        } else {
          assert_choice(baseline, self$learners)
        }

        out$data$baseline = as.numeric(baseline == df$learner_id)
      }

      out
    }
  )
)

#' @title Coercions to BenchmarkAggr
#' @description Coercion methods to [BenchmarkAggr]. For [mlr3::BenchmarkResult] this is a simple
#' wrapper around the [BenchmarkAggr] constructor called with [mlr3::BenchmarkResult]`$aggregate()`.
#' @param obj ([mlr3::BenchmarkResult]|`matrix(1)`) \cr Passed to [BenchmarkAggr]`$new()`.
#' @param independent,strip_prefix See [BenchmarkAggr]`$initialize()`.
#' @param ... `ANY` \cr Passed to [mlr3::BenchmarkResult]`$aggregate()`.
#' @examples
#' df = data.frame(task_id = rep(c("A", "B"), each = 5),
#'                 learner_id = paste0("L", 1:5),
#'                 RMSE = runif(10), MAE = runif(10))
#'
#' as.BenchmarkAggr(df)
#'
#'
#' if (requireNamespaces(c("mlr3", "rpart"))) {
#'   library(mlr3)
#'   task = tsks(c("boston_housing", "mtcars"))
#'   learns = lrns(c("regr.featureless", "regr.rpart"))
#'   bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 2)))
#'
#'   # default measure
#'   as.BenchmarkAggr(bm)
#'
#'   # change measure
#'   as.BenchmarkAggr(bm, measure = msr("regr.rmse"))
#' }
#'
#' @export
as.BenchmarkAggr = function(obj, independent = TRUE, strip_prefix = TRUE, ...) { # nolint
  UseMethod("as.BenchmarkAggr", obj)
}
#' @export
as.BenchmarkAggr.default = function(obj, independent = TRUE, strip_prefix = TRUE, ...) { # nolint
  BenchmarkAggr$new(obj, independent, strip_prefix)
}
#' @export
as.BenchmarkAggr.BenchmarkResult = function(obj, independent = TRUE, strip_prefix = TRUE, ...) { # nolint
  BenchmarkAggr$new(obj$aggregate(...), independent, strip_prefix)
}