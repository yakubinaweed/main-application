z_transform <- function(vec) {
  return((vec - mean(vec)) / sd(vec))
}
