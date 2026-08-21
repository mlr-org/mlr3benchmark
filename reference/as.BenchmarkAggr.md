# Coercions to BenchmarkAggr

This function is deprecated, use
[`as_benchmark_aggr()`](https://mlr3benchmark.mlr-org.com/reference/as_benchmark_aggr.md)
instead.

Coercion methods to
[BenchmarkAggr](https://mlr3benchmark.mlr-org.com/reference/BenchmarkAggr.md).
For
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
this is a simple wrapper around the
[BenchmarkAggr](https://mlr3benchmark.mlr-org.com/reference/BenchmarkAggr.md)
constructor called with
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)`$aggregate()`.

## Usage

``` r
as.BenchmarkAggr(
  obj,
  task_id = "task_id",
  learner_id = "learner_id",
  independent = TRUE,
  strip_prefix = TRUE,
  ...
)
```

## Arguments

- obj:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)\|`matrix(1)`)  
  Passed to
  [BenchmarkAggr](https://mlr3benchmark.mlr-org.com/reference/BenchmarkAggr.md)`$new()`.

- task_id, learner_id, independent, strip_prefix:

  See
  [BenchmarkAggr](https://mlr3benchmark.mlr-org.com/reference/BenchmarkAggr.md)`$initialize()`.

- ...:

  `ANY`  
  Passed to
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)`$aggregate()`.

## Examples

``` r
df = data.frame(tasks = factor(rep(c("A", "B"), each = 5),
                               levels = c("A", "B")),
                learners = factor(paste0("L", 1:5)),
                RMSE = runif(10), MAE = runif(10))

as_benchmark_aggr(df, task_id = "tasks", learner_id = "learners")
#> <BenchmarkAggr> of 10 rows with 2 tasks, 5 learners and 2 measures
#>      tasks learners        RMSE        MAE
#>     <fctr>   <fctr>       <num>      <num>
#>  1:      A       L1 0.958143702 0.11680080
#>  2:      A       L2 0.230981117 0.57059911
#>  3:      A       L3 0.002217805 0.04269875
#>  4:      A       L4 0.493922241 0.57782042
#>  5:      A       L5 0.848674139 0.16699995
#>  6:      B       L1 0.387798765 0.51995640
#>  7:      B       L2 0.354754367 0.56203728
#>  8:      B       L3 0.220384292 0.14794807
#>  9:      B       L4 0.824803983 0.37385347
#> 10:      B       L5 0.027968708 0.44564092


if (requireNamespaces(c("mlr3", "rpart"))) {
  library(mlr3)
  task = tsks(c("sonar", "spam"))
  learns = lrns(c("classif.featureless", "classif.rpart"))
  bm = benchmark(benchmark_grid(task, learns, rsmp("cv", folds = 2)))

  # default measure
  as_benchmark_aggr(bm)

  # change measure
  as_benchmark_aggr(bm, measures = msr("classif.acc"))
}
#> <BenchmarkAggr> of 4 rows with 2 tasks, 2 learners and 1 measure
#>    task_id  learner_id       acc
#>     <fctr>      <fctr>     <num>
#> 1:   sonar featureless 0.5336538
#> 2:   sonar       rpart 0.6442308
#> 3:    spam featureless 0.6059579
#> 4:    spam       rpart 0.9024123
```
