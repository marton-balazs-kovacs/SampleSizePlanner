#' Determine sample size with A Priori Precision (APP) method
#' 
#' The approach aims to plan a sample size based on how close
#' the researcher wishes both sample means to be to their
#' respective population parameter, and how confident the researcher
#' wants to be in this.
#' 
#' @param confidence Numeric. The desired probability of obtaining the sample mean with the desired closeness to the population mean.
#' @param closeness Numeric. The desired closeness of the sample mean to the population mean defined in standard deviation.
#' 
#' @return The function returns a list of one named element. The determined
#' sample size for group one and two called `n1`.
#' @export
#' @examples
#' \dontrun{
#' SampleSizePlanner::ssp_app(closeness = 0.2, confidence = 0.95)
#' }
ssp_app <- function(confidence, closeness) {
  n1 <- ceiling((qnorm((sqrt(confidence) + 1) / 2) / closeness)^2)
  list(n1 = n1)
}
