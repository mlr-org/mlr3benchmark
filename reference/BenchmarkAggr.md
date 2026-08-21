# Aggregated Benchmark Result Object

An R6 class for aggregated benchmark results.

## Details

This class is used to easily carry out and guide analysis of models
after aggregating the results after resampling. This can either be
constructed using [mlr3](https://CRAN.R-project.org/package=mlr3)
objects, for example the result of
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)`$aggregate`
or via
[as_benchmark_aggr](https://mlr3benchmark.mlr-org.com/reference/as_benchmark_aggr.md),
or by passing in a custom dataset of results. Custom datasets must
include at the very least, a character column for learner ids, a
character column for task ids, and numeric columns for one or more
measures.

Currently supported for multiple independent datasets only.

## References

Demšar J (2006). “Statistical Comparisons of Classifiers over Multiple
Data Sets.” *Journal of Machine Learning Research*, **7**(1), 1-30.
<https://jmlr.org/papers/v7/demsar06a.html>.

## Active bindings

- `data`:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Aggregated data.

- `learners`:

  `(character())`  
  Unique learner names.

- `tasks`:

  `(character())`  
  Unique task names.

- `measures`:

  `(character())`  
  Unique measure names.

- `nlrns`:

  `(integer())`  
  Number of learners.

- `ntasks`:

  `(integer())`  
  Number of tasks.

- `nmeas`:

  `(integer())`  
  Number of measures.

- `nrow`:

  `(integer())`  
  Number of rows.

- `col_roles`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Column roles, currently cannot be changed after construction.

## Methods

### Public methods

- [`BenchmarkAggr$new()`](#method-BenchmarkAggr-initialize)

- [`BenchmarkAggr$print()`](#method-BenchmarkAggr-print)

- [`BenchmarkAggr$summary()`](#method-BenchmarkAggr-summary)

- [`BenchmarkAggr$rank_data()`](#method-BenchmarkAggr-rank_data)

- [`BenchmarkAggr$friedman_test()`](#method-BenchmarkAggr-friedman_test)

- [`BenchmarkAggr$friedman_posthoc()`](#method-BenchmarkAggr-friedman_posthoc)

- [`BenchmarkAggr$subset()`](#method-BenchmarkAggr-subset)

- [`BenchmarkAggr$clone()`](#method-BenchmarkAggr-clone)

------------------------------------------------------------------------

### `BenchmarkAggr$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    BenchmarkAggr$new(
      dt,
      task_id = "task_id",
      learner_id = "learner_id",
      independent = TRUE,
      strip_prefix = TRUE,
      ...
    )

#### Arguments

- `dt`:

  `(matrix(1))`  
  A `matrix` like object coercable to
  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html),
  should include column names "task_id" and "learner_id", and at least
  one measure (numeric). If ids are not already factors then coerced
  internally.

- `task_id`:

  (`character(1)`)  
  String specifying name of task id column.

- `learner_id`:

  (`character(1)`)  
  String specifying name of learner id column.

- `independent`:

  `(logical(1))`  
  Are tasks independent of one another? Affects which tests can be used
  for analysis.

- `strip_prefix`:

  (`logical(1)`)  
  If `TRUE` (default) then mlr prefixes, e.g. `regr.`, `classif.`, are
  automatically stripped from the `learner_id`.

- `...`:

  `ANY`  
  Additional arguments, currently unused.

------------------------------------------------------------------------

### `BenchmarkAggr$print()`

Prints the internal data via
[data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html).

#### Usage

    BenchmarkAggr$print(...)

#### Arguments

- `...`:

  `ANY`  
  Passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html).

------------------------------------------------------------------------

### `BenchmarkAggr$summary()`

Prints the internal data via
[data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html).

#### Usage

    BenchmarkAggr$summary(...)

#### Arguments

- `...`:

  `ANY`  
  Passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html).

------------------------------------------------------------------------

### `BenchmarkAggr$rank_data()`

Ranks the aggregated data given some measure.

#### Usage

    BenchmarkAggr$rank_data(meas = NULL, minimize = TRUE, task = NULL, ...)

#### Arguments

- `meas`:

  `(character(1))`  
  Measure to rank the data against, should be in `$measures`. Can be
  `NULL` if only one measure in data.

- `minimize`:

  `(logical(1))`  
  Should the measure be minimized? Default is `TRUE`.

- `task`:

  `(character(1))`  
  If `NULL` then returns a matrix of ranks where columns are tasks and
  rows are learners, otherwise returns a one-column matrix of a
  specified task, should be in `$tasks`.

- `...`:

  `ANY` `ANY`  
  Passed to
  [`data.table::frank()`](https://rdrr.io/pkg/data.table/man/frank.html).

------------------------------------------------------------------------

### `BenchmarkAggr$friedman_test()`

Computes Friedman test over all tasks, assumes datasets are independent.

#### Usage

    BenchmarkAggr$friedman_test(meas = NULL, p.adjust.method = NULL)

#### Arguments

- `meas`:

  `(character(1))`  
  Measure to rank the data against, should be in `$measures`. If no
  measure is provided then returns a matrix of tests for all measures.

- `p.adjust.method`:

  `(character(1))`  
  Passed to [p.adjust](https://rdrr.io/r/stats/p.adjust.html) if
  `meas = NULL` for multiple testing correction. If `NULL` then no
  correction applied.

------------------------------------------------------------------------

### `BenchmarkAggr$friedman_posthoc()`

Posthoc Friedman Nemenyi tests. Computed with
[PMCMRplus::frdAllPairsNemenyiTest](https://rdrr.io/pkg/PMCMRplus/man/frdAllPairsNemenyiTest.html).
If global `$friedman_test` is non-significant then this is returned and
no post-hocs computed. Also returns critical difference

#### Usage

    BenchmarkAggr$friedman_posthoc(
      meas = NULL,
      p.value = 0.05,
      friedman_global = TRUE
    )

#### Arguments

- `meas`:

  `(character(1))`  
  Measure to rank the data against, should be in `$measures`. Can be
  `NULL` if only one measure in data.

- `p.value`:

  `(numeric(1))`  
  p.value for which the global test will be considered significant.

- `friedman_global`:

  (`logical(1)`)  
  Should a friedman global test be performed before conducting the
  posthoc test? If `FALSE`, a warning is issued in case the
  corresponding friedman global test fails instead of an error. Default
  is `TRUE` (raises an error if global test fails).

------------------------------------------------------------------------

### `BenchmarkAggr$subset()`

Subsets the data by given tasks or learners. Returns data as
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

#### Usage

    BenchmarkAggr$subset(task = NULL, learner = NULL)

#### Arguments

- `task`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Task(s) to subset the data by.

- `learner`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Learner(s) to subset the data by.

------------------------------------------------------------------------

### `BenchmarkAggr$clone()`

The objects of this class are cloneable with this method.

#### Usage

    BenchmarkAggr$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Not restricted to mlr3 objects
df = data.frame(tasks = factor(rep(c("A", "B"), each = 5),
                               levels = c("A", "B")),
                learners = factor(paste0("L", 1:5)),
                RMSE = runif(10), MAE = runif(10))
as_benchmark_aggr(df, task_id = "tasks", learner_id = "learners")
#> <BenchmarkAggr> of 10 rows with 2 tasks, 5 learners and 2 measures
#>      tasks learners        RMSE        MAE
#>     <fctr>   <fctr>       <num>      <num>
#>  1:      A       L1 0.080750138 0.87460066
#>  2:      A       L2 0.834333037 0.17494063
#>  3:      A       L3 0.600760886 0.03424133
#>  4:      A       L4 0.157208442 0.32038573
#>  5:      A       L5 0.007399441 0.40232824
#>  6:      B       L1 0.466393497 0.19566983
#>  7:      B       L2 0.497777389 0.40353812
#>  8:      B       L3 0.289767245 0.06366146
#>  9:      B       L4 0.732881987 0.38870131
#> 10:      B       L5 0.772521511 0.97554784

if (requireNamespaces(c("mlr3", "rpart"))) {
  library(mlr3)
  task = tsks(c("sonar", "spam"))
  learns = lrns(c("classif.featureless", "classif.rpart"))
  bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 2)))

  # coercion
  as_benchmark_aggr(bm)
}
#> <BenchmarkAggr> of 4 rows with 2 tasks, 2 learners and 1 measure
#>    task_id  learner_id        ce
#>     <fctr>      <fctr>     <num>
#> 1:   sonar featureless 0.5432692
#> 2:   sonar       rpart 0.3076923
#> 3:    spam featureless 0.3940461
#> 4:    spam       rpart 0.1056295
```
