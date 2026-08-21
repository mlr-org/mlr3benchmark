# Coercions to BenchmarkAggr

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
as_benchmark_aggr(
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
#>  1:      A       L1 0.705157773 0.94656867
#>  2:      A       L2 0.643531289 0.39483338
#>  3:      A       L3 0.178971994 0.08425591
#>  4:      A       L4 0.535854182 0.07194882
#>  5:      A       L5 0.189668759 0.12406365
#>  6:      B       L1 0.002376202 0.08919474
#>  7:      B       L2 0.209276534 0.03956739
#>  8:      B       L3 0.077628147 0.25982758
#>  9:      B       L4 0.612152911 0.97113995
#> 10:      B       L5 0.170132384 0.54270485


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
#> 2:   sonar       rpart 0.7836538
#> 3:    spam featureless 0.6059529
#> 4:    spam       rpart 0.8954580
```
