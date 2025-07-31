library(refineR)

run_refiner <- function(values, nbootstrap_value) {
  model <- refineR::findRI(Data = values, NBootstrap = nbootstrap_value)
  return(model)
}

extract_intervals <- function(model) {
  ri <- refineR::getPercentileCI(model, percentiles = c(2.5, 97.5))
  return(ri)
}