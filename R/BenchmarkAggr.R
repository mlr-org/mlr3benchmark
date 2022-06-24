#' @title Aggregated Benchmark Result Object
#'
#' @description An R6 class for aggregated benchmark results.
#' @details This class is used to easily carry out and guide analysis of models after aggregating
#' the results after resampling. This can either be constructed using \CRANpkg{mlr3} objects,
#' for example the result of [mlr3::BenchmarkResult]`$aggregate` or via [as.BenchmarkAggr],
#' or by passing in a custom dataset of results. Custom datasets must include at the very least,
#' a character column for learner ids, a character column for task ids, and numeric columns for
#' one or more measures.
#'
#' Currently supported for multiple independent datasets only.
#'
#' @references
#' `r format_bib("demsar_2006")
#'
#' @examples
#' # Not restricted to mlr3 objects
#' df = data.frame(tasks = factor(rep(c("A", "B"), each = 5),
#'                                levels = c("A", "B")),
#'                 learners = factor(paste0("L", 1:5)),
#'                 RMSE = runif(10), MAE = runif(10))
#' as.BenchmarkAggr(df, task_id = "tasks", learner_id = "learners")
#'
#' if (requireNamespaces(c("mlr3", "rpart"))) {
#'   library(mlr3)
#'   task = tsks(c("boston_housing", "mtcars"))
#'   learns = lrns(c("regr.featureless", "regr.rpart"))
#'   bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 2)))
#'
#'   # coercion
#'   as.BenchmarkAggr(bm)
#' }
#' @export
BenchmarkAggr = R6Class("BenchmarkAggr",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param dt `(matrix(1))` \cr'
    #' `matrix` like object coercable to [data.table::data.table][data.table], should
    #' include column names "task_id" and "learner_id", and at least one measure (numeric).
    #' If ids are not already factors then coerced internally.
    #' @param task_id (`character(1)`) \cr
    #' String specifying name of task id column.
    #' @param learner_id (`character(1)`)\cr
    #' String specifying name of learner id column.
    #' @param independent `(logical(1))` \cr
    #' Are tasks independent of one another? Affects which tests can be used for analysis.
    #' @param strip_prefix (`logical(1)`) \cr
    #' If `TRUE` (default) then mlr prefixes, e.g. `regr.`, `classif.`, are automatically
    #' stripped from the `learner_id`.
    #' @param ... `ANY` \cr
    #' Additional arguments, currently unused.
    initialize = function(dt, task_id = "task_id", learner_id = "learner_id",
                          independent = TRUE, strip_prefix = TRUE, ...) {

       if (!is.data.table(dt)) {
        dt = as.data.table(dt)
      }

      private$.independent = assert_flag(independent)
      assert_flag(strip_prefix)
      assert_subset(c(task_id, learner_id), colnames(dt))
      assert_factor(unlist(subset(dt, select = task_id)))
      assert_factor(unlist(subset(dt, select = learner_id)))

      private$.col_roles = list(task_id = task_id, learner_id = learner_id)
      measure_ids = setdiff(colnames(dt), c(task_id, learner_id, "nr", "resample_result",
                                            "resampling_id", "iters"))

      if (length(measure_ids) == 0L) {
        stop("At least one measure must be included in `dt`.")
      }

      dt = dt[, c(task_id, learner_id, measure_ids), with = FALSE]
      # confirm all measures numeric
      assert_data_frame(dt[, measure_ids, with = FALSE], types = "numeric")

      if (!independent) {
        warning("Currently only methods for independent datasets are supported.")
      }

      if (anyDuplicated(dt, by = c(task_id, learner_id))) {
        stop("Multiple results for a learner-task combination detected. There should be exactly one row for each learner-task combination.") # nolint
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
      df = private$.dt[ , c(self$col_roles$task_id, meas), with = FALSE]

      if (!minimize) {
        df[[meas]] = -df[[meas]]
      }

      if (!is.null(task)) {
        df = df[get(self$col_roles$task_id) == task]
        rdf = matrix(data.table::frank(subset(df, select = meas), ...), ncol = 1)
        colnames(rdf) = task
      } else {
        rdf = matrix(nrow = self$nlrns, ncol = self$ntasks)
        for (i in seq_along(self$tasks)) {
          rdf[, i] = data.table::frank(subset(df, get(self$col_roles$task_id) == self$tasks[[i]],
                                              select = meas), ...)
        }
        colnames(rdf) = self$tasks
      }

      rownames(rdf) = self$learners
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

      if (!is.null(meas) || (is.null(meas) && self$nmeas == 1)) {
        if (is.null(meas)) meas = self$measures
        return(stats::friedman.test(as.formula(paste0(meas, " ~ ", self$col_roles$learner_id, " | ",
                                                      self$col_roles$task_id)),
                      data = private$.dt))
      } else {
        x = sapply(self$measures, function(x)
          stats::friedman.test(as.formula(paste0(x, " ~ ", self$col_roles$learner_id, " | ",
                                                 self$col_roles$task_id)),
                       data = private$.dt))
        x = data.frame(t(x[1:3, ]))
        colnames(x) = c("X2", "df", "p.value")
        rownames(x) = self$measures

        if (!is.null(p.adjust.method)) {
          x$p.adj.value = stats::p.adjust(x$p.value, p.adjust.method) # nolint
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
    #' [PMCMRplus::frdAllPairsNemenyiTest]. If global `$friedman_test` is non-significant then
    #' this is returned and no post-hocs computed. Also returns critical difference
    #' @param meas `(character(1))` \cr
    #' Measure to rank the data against, should be in `$measures`. Can be `NULL` if only one measure
    #' in data.
    #' @param p.value `(numeric(1))` \cr
    #' p.value for which the global test will be considered significant.
    #' @param friedman_global (`logical(1)`)\cr
    #' Should a friedman global test be performed before conducting the posthoc
    #' test? If `FALSE`, a warning is issued in case the corresponding friedman
    #' global test fails instead of an error. Default is `TRUE` (raises an
    #' error if global test fails).
    friedman_posthoc = function(meas = NULL, p.value = 0.05, friedman_global = TRUE) { # nolint

      if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
        stop("Package PMCMRplus required for post-hoc Friedman tests.")
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
          if (friedman_global) {
            warning(sprintf(
              "Global Friedman test non-significant (p = %s > %s). Returning overall Friedman test.",
              round(f.test$p.value, 3), p.value
            )) # nolint
          } else {
            warning(sprintf(
          "Global Friedman test non-significant (p = %s > %s), posthoc results unreliable.",
          round(f.test$p.value, 3), p.value
            ))
          }
        }
      } else {
        f.rejnull = FALSE # nolint
        warning("P-value not computable. Learner performances might be exactly equal.")
      }

      if (f.rejnull || !friedman_global) {
        form = as.formula(paste0(meas, " ~ ", self$col_roles$learner_id, " | ",
                                 self$col_roles$task_id))
        nem_test = PMCMRplus::frdAllPairsNemenyiTest(form, data = private$.dt) # nolint
        nem_test$f.rejnull = f.rejnull # nolint
        return(nem_test)
      } else {
        f.test$f.rejnull = f.rejnull # nolint
        return(f.test)
      }
    },

    #' @description Subsets the data by given tasks or learners.
    #' Returns data as [data.table::data.table].
    #' @param task (`character()`) \cr
    #' Task(s) to subset the data by.
    #' @param learner (`character()`) \cr
    #' Learner(s) to subset the data by.
    subset = function(task = NULL, learner = NULL) {
      dt = private$.dt

      if (!is.null(task))
        dt = subset(dt, get(self$col_roles$task_id) == task)
      if (!is.null(learner))
        dt = dt[get(self$col_roles$learner_id) == learner]

      dt
    }
  ),

  active = list(
    #' @field data ([data.table::data.table]) \cr Aggregated data.
    data = function() private$.dt,
    #' @field learners `(character())` \cr Unique learner names.
    learners = function() levels(private$.dt[[self$col_roles$learner_id]]),
    #' @field tasks `(character())` \cr Unique task names.
    tasks = function() levels(private$.dt[[self$col_roles$task_id]]),
    #' @field measures `(character())` \cr Unique measure names.
    measures = function() setdiff(colnames(private$.dt), unlist(self$col_roles)),
    #' @field nlrns `(integer())` \cr Number of learners.
    nlrns = function() nlevels(private$.dt[[self$col_roles$learner_id]]),
    #' @field ntasks `(integer())` \cr Number of tasks.
    ntasks = function() nlevels(private$.dt[[self$col_roles$task_id]]),
    #' @field nmeas `(integer())` \cr Number of measures.
    nmeas = function() ncol(private$.dt) - 2L,
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
    .dt = data.table(),
    .independent = logical(0),
    .crit_differences = function(meas = NULL, minimize = TRUE, p.value = 0.05, baseline = NULL, # nolint
                                test = c("bd", "nemenyi"), friedman_global = TRUE) {

      meas = .check_meas(self, meas)
      test = match.arg(test)
      assertNumeric(p.value, lower = 0, upper = 1, len = 1)

      # Get Rankmatrix, transpose and get mean ranks
      mean_rank = rowMeans(self$rank_data(meas, minimize = minimize))
      # Gather Info for plotting the descriptive part.
      df = data.frame(mean_rank,
                      learner_id = names(mean_rank),
                      rank = rank(mean_rank, ties.method = "average"))
      colnames(df)[2] = self$col_roles$learner_id
      # Orientation of descriptive lines yend(=y-value of horizontal line)
      right = df$rank > stats::median(df$rank)
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
                 warning = function(w) {
                   if (friedman_global)
                     stopf("Global Friedman test non-significant (p > %s), try type = 'mean' instead.", p.value) # nolint
                   else
                     warning(sprintf("Global Friedman test non-significant (p > %s), try type = 'mean' instead.", p.value)) # nolint))
                 })

      # calculate critical difference(s)
      if (test == "nemenyi") {
        cd = (stats::qtukey(1 - p.value, self$nlrns, 1e+06) / sqrt(2L)) *
          sqrt(self$nlrns * (self$nlrns + 1L) / (6L * self$ntasks))
      } else {
        cd = (stats::qtukey(1L - (p.value / (self$nlrns - 1L)), 2L, 1e+06) / sqrt(2L)) *
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
        rank_y = rank(c(seq.int(length.out = sum(nem_df$xstart <= stats::median(df$mean_rank))),
                     seq.int(length.out = sum(nem_df$xstart > stats::median(df$mean_rank)))),
                     ties.method = "first")
        nem_df$y = seq.int(from = 0.5, by = 0.5, length.out = nrow(nem_df))[rank_y]

        out$nemenyi_data = as.data.frame(nem_df) # nolint
      } else {
        # Get a baseline
        if (is.null(baseline)) {
          baseline = as.character(df[[self$col_roles$learner_id]][which.min(df$rank)])
        } else {
          assert_choice(baseline, self$learners)
        }

        out$data$baseline = as.numeric(baseline == df[[self$col_roles$learner_id]])
      }

      out
    }
  )
)

#' @title Coercions to BenchmarkAggr
#'
#' @description Coercion methods to [BenchmarkAggr]. For [mlr3::BenchmarkResult] this is a simple
#' wrapper around the [BenchmarkAggr] constructor called with [mlr3::BenchmarkResult]`$aggregate()`.
#'
#' @param obj ([mlr3::BenchmarkResult]|`matrix(1)`) \cr Passed to [BenchmarkAggr]`$new()`.
#' @param task_id,learner_id,independent,strip_prefix See [BenchmarkAggr]`$initialize()`.
#' @param ... `ANY` \cr Passed to [mlr3::BenchmarkResult]`$aggregate()`.
#' @examples
#' df = data.frame(tasks = factor(rep(c("A", "B"), each = 5),
#'                                levels = c("A", "B")),
#'                 learners = factor(paste0("L", 1:5)),
#'                 RMSE = runif(10), MAE = runif(10))
#'
#' as.BenchmarkAggr(df, task_id = "tasks", learner_id = "learners")
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
#'   as.BenchmarkAggr(bm, measures = msr("regr.rmse"))
#' }
#'
#' @export
as.BenchmarkAggr = function(obj, task_id = "task_id", learner_id = "learner_id",
                            independent = TRUE, strip_prefix = TRUE, ...) { # nolint
  UseMethod("as.BenchmarkAggr", obj)
}

#' @export
as.BenchmarkAggr.default = function(obj, task_id = "task_id", learner_id = "learner_id",
                                    independent = TRUE, strip_prefix = TRUE, ...) { # nolint
  BenchmarkAggr$new(as.data.table(obj), task_id = task_id, learner_id = learner_id,
                    independent = independent, strip_prefix = strip_prefix)
}

#' @export
as.BenchmarkAggr.BenchmarkResult = function(obj, task_id = "task_id", learner_id = "learner_id",
                                            independent = TRUE, strip_prefix = TRUE,
                                            measures = NULL, ...) { # nolint
  requireNamespaces("mlr3")
  measures = mlr3::as_measures(measures, task_type = obj$task_type)
  tab = obj$aggregate(measures = measures)
  cols = c("task_id", "learner_id", map_chr(measures, "id"))
  tab$task_id = factor(tab$task_id, levels = unique(tab$task_id))
  tab$learner_id = factor(tab$learner_id, levels = unique(tab$learner_id))
  BenchmarkAggr$new(tab[, cols, with = FALSE], independent = independent,
                    strip_prefix = strip_prefix)
}
