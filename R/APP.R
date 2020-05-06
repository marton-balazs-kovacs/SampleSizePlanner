APP <- function(confidence, closeness) {
  ceiling ((qnorm((sqrt(confidence)+1)/2)/closeness)^2)
}