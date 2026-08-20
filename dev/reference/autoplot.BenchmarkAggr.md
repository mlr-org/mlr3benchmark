# Plots for BenchmarkAggr

Generates plots for
[BenchmarkAggr](https://mlr3benchmark.mlr-org.com/dev/reference/BenchmarkAggr.md),
all assume that there are multiple, independent, tasks. Choices
depending on the argument `type`:

- `"mean"` (default): Assumes there are at least two independent tasks.
  Plots the sample mean of the measure for all learners with error bars
  computed with the standard error of the mean.

- `"box"`: Boxplots for each learner calculated over all tasks for a
  given measure.

- `"fn"`: Plots post-hoc Friedman-Nemenyi by first calling
  [BenchmarkAggr](https://mlr3benchmark.mlr-org.com/dev/reference/BenchmarkAggr.md)`$friedman_posthoc`
  and plotting significant pairs in coloured squares and leaving
  non-significant pairs blank, useful for simply visualising pair-wise
  comparisons.

- `"cd"`: Critical difference plots (Demsar, 2006). Learners are drawn
  on the x-axis according to their average rank with the best performing
  on the left and decreasing performance going right. Any learners not
  connected by a horizontal bar are significantly different in
  performance. Critical differences are calculated as: \$\$CD =
  q\_{\alpha} \sqrt{\left(\frac{k(k+1)}{6N}\right)}\$\$ Where
  \\q\_\alpha\\ is based on the studentized range statistic. See
  references for further details. It's recommended to crop white space
  using external tools, or function `image_trim()` from package
  [magick](https://CRAN.R-project.org/package=magick).

## Usage

``` r
# S3 method for class 'BenchmarkAggr'
autoplot(
  object,
  type = c("mean", "box", "fn", "cd"),
  meas = NULL,
  level = 0.95,
  p.value = 0.05,
  minimize = TRUE,
  test = "nem",
  baseline = NULL,
  style = 1L,
  ratio = 1/7,
  col = "red",
  friedman_global = TRUE,
  ...
)
```

## Arguments

- object:

  ([BenchmarkAggr](https://mlr3benchmark.mlr-org.com/dev/reference/BenchmarkAggr.md))  
  The benchmark aggregation object.

- type:

  `(character(1))`  
  Type of plot, see description.

- meas:

  `(character(1))`  
  Measure to plot, should be in `obj$measures`, can be `NULL` if only
  one measure is in `obj`.

- level:

  `(numeric(1))`  
  Confidence level for error bars for `type = "mean"`

- p.value:

  `(numeric(1))`  
  What value should be considered significant for `type = "cd"` and
  `type = "fn"`.

- minimize:

  `(logical(1))`  
  For `type = "cd"`, indicates if the measure is optimally minimized.
  Default is `TRUE`.

- test:

  (`character(1))`)  
  For `type = "cd"`, critical differences are either computed between
  all learners (`test = "nemenyi"`), or to a baseline (`test = "bd"`).
  Bonferroni-Dunn usually yields higher power than Nemenyi as it only
  compares algorithms to one baseline. Default is `"nemenyi"`.

- baseline:

  `(character(1))`  
  For `type = "cd"` and `test = "bd"` a baseline learner to compare the
  other learners to, should be in `$learners`, if `NULL` then
  differences are compared to the best performing learner.

- style:

  `(integer(1))`  
  For `type = "cd"` two ggplot styles are shipped with the package
  (`style = 1` or `style = 2`), otherwise the data can be accessed via
  the returned ggplot.

- ratio:

  (`numeric(1)`)  
  For `type = "cd"` and `style = 1`, passed to
  [`ggplot2::coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html),
  useful for quickly specifying the aspect ratio of the plot, best used
  with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- col:

  (`character(1)`)  
  For `type = "fn"`, specifies color to fill significant tiles, default
  is `"red"`.

- friedman_global:

  (`logical(1)`)  
  Should a friedman global test be performed for`type = "cd"` and
  `type = "fn"`? If `FALSE`, a warning is issued in case the
  corresponding friedman posthoc test fails instead of an error. Default
  is `TRUE` (raises an error if global test fails).

- ...:

  `ANY`  
  Additional arguments, currently unused.

## Value

The generated plot.

## References

Demšar J (2006). “Statistical Comparisons of Classifiers over Multiple
Data Sets.” *Journal of Machine Learning Research*, **7**(1), 1-30.
<https://jmlr.org/papers/v7/demsar06a.html>.

## Examples

``` r
if (requireNamespaces(c("mlr3learners", "mlr3", "rpart", "xgboost"))) {
library(mlr3)
library(mlr3learners)
library(ggplot2)

set.seed(1)
task = tsks(c("iris", "sonar", "wine", "zoo"))
learns = lrns(c("classif.featureless", "classif.rpart", "classif.xgboost"))
learns$classif.xgboost$param_set$values$nrounds = 50
bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 3)))
obj = as_benchmark_aggr(bm)

# mean and error bars
autoplot(obj, type = "mean", level = 0.95)

if (requireNamespace("PMCMRplus", quietly = TRUE)) {
  # critical differences
  autoplot(obj, type = "cd",style = 1)
  autoplot(obj, type = "cd",style = 2)

  # post-hoc friedman-nemenyi
  autoplot(obj, type = "fn")
}

}

```
