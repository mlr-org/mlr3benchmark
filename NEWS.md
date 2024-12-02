# mlr3benchmark 0.1.7

* Changed examples to not use `tsk("boston_housing")` anymore
* Remove use of deprecated `ggplot2` functions

# mlr3benchmark 0.1.6

* Renamed `as.BenchmarkAggr` to `as_benchmark_aggr()` for consistency. The former still works but is deprecated.

# mlr3benchmark 0.1.5

* Rename argument of autoplot function to comply with new CRAN check
* Fix bug in `rank_data` caused by learner name mismatch

# mlr3benchmark 0.1.4

* Add `friedman_global` argument to posthoc tests and to autoplots to allow methods and plots to run even if the global Friedman test fails (i.e. don't reject null)
* New maintainer: Sebastian Fischer
* Fix documentation

# mlr3benchmark 0.1.3

* Fix README
* Fix for PMCMRplus

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
