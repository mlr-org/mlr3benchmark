.check_meas = function(obj, meas) {
  if (is.null(meas)) {
    if (obj$nmeas > 1) {
      stop("Multiple measures available but `meas` is NULL. Please specify a measure.")
    } else {
      meas = obj$measures
    }
  } else {
    assert_choice(meas, obj$measures)
  }

  return(meas)
}

#' @title Helper Vectorizing requireNamespace
#' @description Internal helper function for documentation.
#' @param x Packages to check.
#' @return
#' A `logical(1)`, indicating wether all required packages are available.
#' @export
requireNamespaces = function(x) { # nolint
  if (all(map_lgl(x, requireNamespace, quietly = TRUE))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
