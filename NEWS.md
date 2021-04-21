# mlr3benchmark 0.1.2.9000

* Fix README

# mlr3benchmark 0.1.2

* Critical patch for bug in creating BenchmarkAggr objects.
* Task and learner columns must now be provided as factors to BenchmarkAggr objects, no internal coercion is made.
* Bug fix in CD plots

# mlr3benchmark 0.1.1

* BenchmarkAggr$friedman_test now returns the full test object if only a single measure exists in the object
* Fixed plotting in `autoplot.BenchmarkAggr` for CD-plots, previously bars were overlapping and giving misleading results.
* `BenchmarkAggr` is now more flexible in construction. Instead of being forced to name columns `task_id` and `learner_id`, instead any name can be used if passed to the respective arguments in the constructor.
* Adds `$subset` public method to `BenchmarkAggr` as a thin wrapper around `subset` for `data.table`. Returns subsetted data.table.

# mlr3benchmark 0.1.0

* Initial CRAN release
