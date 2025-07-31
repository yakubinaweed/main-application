library(refineR)

run_refiner <- function(values) {
  model <- RefineR(values, modelSelection = TRUE, nBootstraps = 1)
  return(model)
}

extract_intervals <- function(model) {
  ri <- getPercentileCI(model, percentiles = c(2.5, 97.5))
  return(ri)
}