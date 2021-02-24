ssp_app <- function(confidence, closeness) {
  n1 <- ceiling((qnorm((sqrt(confidence) + 1) / 2) / closeness)^2)
  n1
}
