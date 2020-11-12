#' @title Plots for BenchmarkAggr
#'
#' @description
#' Generates plots for [BenchmarkAggr], all assume that there are multiple, independent, tasks.
#' Choices depending on the argument `type`:
#'
#' * `"mean"` (default): Assumes there are at least two independent tasks. Plots the sample mean
#' of the measure for all learners with error bars computed with the standard error of the mean.
#' * `"box"`: Boxplots for each learners calculated over all tasks for a given measure.
#' * `"fn"`: Plots post-hoc Friedman-Nemenyi by first calling `[BenchmarkAggr]$friedman_posthoc`
#' and plotting significant pairs in red squares and leaving non-significant pairs blank,
#' useful for simply visualising pair-wise comparisons.
#' * `"cd"`: Critical difference plots (Demsar, 2006). Learners are drawn on the x-axis according
#' to their average rank with the best performing on the left and decreasing performance going
#' right. Any learners not connected by a horizontal bar are significantly different in performance.
#' Critical differences are calculated as:
#' \deqn{CD = q_{\alpha} \sqrt{\left(\frac{k(k+1)}{6N}\right)}}{CD = q_alpha sqrt(k(k+1)/(6N))}
#' Where \eqn{q_\alpha} is based on the studentized range statistic.
#' See references for further details. It's recommended to use [magick::image_trim()] to crop
#' the white space around the image.
#'
#' @param obj [BenchmarkAggr]
#' @param type `(character(1))` \cr Type of plot, see description.
#' @param meas `(character(1))` \cr Measure to plot, should be in `obj$measures`, can be `NULL` if
#' only one measure is in `obj`.
#' @param level `(numeric(1))` \cr Confidence level for error bars for `type = "mean"`
#' @param p.value `(numeric(1))` \cr What value should be considered significant for
#' `type = "cd"` and `type = "fn"`.
#' @param minimize `(logical(1))` \cr
#' For `type = "cd"`, indicates if the measure is optimally minimized. Default is `TRUE`.
#' @param test (`character(1))`) \cr
#' For `type = "cd"`, critical differences are either computed between all learners
#' (`test = "nemenyi"`), or to a baseline (`test = "bd"`). Bonferroni-Dunn usually yields higher
#' power than Nemenyi as it only compares algorithms to one baseline. Default is Nemenyi.
#' @param baseline `(character(1))` \cr
#' For `type = "cd"` and `test = "bd"` a baseline learner to compare the other learners to,
#' should be in `$learners`, if `NULL` then differences are compared to the best performing
#' learner.
#' @param style `(integer(1))` \cr
#' For `type = "cd"` two ggplot styles are shipped with the package (`style = 1` or `style = 2`),
#' otherwise the data can be accessed via the returned ggplot.
#' @param ratio (`numeric(1)`) \cr
#' For `type = "cd"` and `style = 1`, passed to [ggplot2::coord_fixed()], useful for quickly
#' specifying the aspect ratio of the plot, best used with [ggsave()].
#' @param col (`character(1)`)\cr
#' For `type = "fn"`, specifies color to fill significant tiles, default is `"red"`.
#' @param ... `ANY` \cr Additional arguments, currently unused.
#'
#' @references Janez Demsar, Statistical Comparisons of Classifiers over
#' Multiple Data Sets, JMLR, 2006
#'
#' @examples
#' if (requireNamespaces(c("mlr3learners", "mlr3", "rpart", "xgboost"))) {
#' library(mlr3)
#' library(mlr3learners)
#' library(ggplot2)
#'
#' set.seed(1)
#' task = tsks(c("iris", "sonar", "wine", "zoo"))
#' learns = lrns(c("classif.featureless", "classif.rpart", "classif.xgboost"))
#' bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 3)))
#' obj = as.BenchmarkAggr(bm)
#'
#' # mean and error bars
#' autoplot(obj, type = "mean")
#'
#' if (requireNamespace("PMCMR", quietly = TRUE)) {
#'   # critical differences
#'   autoplot(obj, type = "cd")
#'
#'   # post-hoc friedman-nemenyi
#'   autoplot(obj, type = "fn")
#' }
#'
#' }
#'
#' @export
autoplot.BenchmarkAggr = function(obj, type = c("mean", "box", "fn", "cd"), meas = NULL, # nolint
                                  level = 0.95, p.value = 0.05, minimize = TRUE, # nolint
                                  test = "nem", baseline = NULL, style = 1L,
                                  ratio = 1/7, col = "red", ...) { # nolint

  # fix no visible binding
  lower = upper = Var1 = Var2 = value = NULL

  type = match.arg(type)

  meas = .check_meas(obj, meas)

  if (type == "cd") {
    if (style == 1L) .plot_critdiff_1(obj, meas, p.value, minimize, test, baseline, ratio)
    else .plot_critdiff_2(obj, meas, p.value, minimize, test, baseline)
  } else if (type == "mean") {
    if (obj$ntasks < 2) {
      stop("At least two tasks required.")
    }
    loss = stats::aggregate(as.formula(paste0(meas, " ~ learner_id")), obj$data, mean)
    se = stats::aggregate(as.formula(paste0(meas, " ~ learner_id")), obj$data, stats::sd)[, 2] / sqrt(obj$ntasks)
    loss$lower = loss[, meas] - se * stats::qnorm(1 - (1 - level) / 2)
    loss$upper = loss[, meas] + se * stats::qnorm(1 - (1 - level) / 2)
    ggplot(data = loss, aes_string(x = "learner_id", y = meas)) +
      geom_errorbar(aes(ymin = lower, ymax = upper),
                    width = .5) +
      geom_point()
  } else if (type == "fn") {

    p = tryCatch(obj$friedman_posthoc(meas, p.value)$p.value,
                 warning = function(w)
                   stopf("Global Friedman test non-significant (p > %s), try type = 'mean' instead.", p.value)) # nolint

    p = p[rev(seq_len(nrow(p))), ]
    p = t(p)

    p = cbind(expand.grid(rownames(p), colnames(p)), value = as.numeric(p))

    p$value = factor(ifelse(p$value < p.value, "0", "1"))
    ggplot(data = p, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(size = 0.5, color = !is.na(p$value)) +
      scale_fill_manual(name = "p-value",
                        values = c("0" = col, "1" = "white"),
                        breaks = c("0", "1"),
                        labels = c(paste0("<= ", p.value), paste0("> ", p.value))) +
      theme(axis.title = element_blank(),
            axis.text.y = element_text(angle = 45),
            axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.7),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            legend.background = element_rect(color = "black"),
            legend.key = element_rect(color = "black"),
            legend.position = c(1, 0.9),
            legend.justification = "right")

  } else if (type == "box") {
    ggplot(data = obj$data,
           aes_string(x = "learner_id", y = meas)) +
      geom_boxplot()
  }
}
