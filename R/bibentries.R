# nocov start
format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  demsar_2006 = bibentry("article",
    author  = "Janez Dem\u0161ar",
    title   = "Statistical Comparisons of Classifiers over Multiple Data Sets",
    journal = "Journal of Machine Learning Research",
    year    = "2006",
    volume  = "7",
    number  = "1",
    pages   = "1-30",
    url     = "https://jmlr.org/papers/v7/demsar06a.html"
  ),
  benavoli_2017 = bibentry("article",
    author  = "Alessio Benavoli, Giorgio Corani, Janez Dem\u0161ar and Marco Zaffalon",
    title   = "Time for a Change: a Tutorial for Comparing Multiple Classifiers Through Bayesian Analysis",
    journal = "Journal of Machine Learning Research",
    year    = "2017",
    volume  = "18",
    number  = "77",
    pages   = "1-36",
    url     = "http://jmlr.org/papers/v18/16-305.html"
  )
)
# nocov end
