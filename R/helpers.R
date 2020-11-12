.check_meas = function(obj, meas) {
  if (is.null(meas)) {
    if (obj$nmeas > 1) {
      stop("Multiple measures available but `meas` is NULL. Please specify a measure.")
    } else {
      meas = obj$measures
    }
  }

  return(meas)
}
