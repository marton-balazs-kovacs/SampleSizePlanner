#' Determine sample size with Accuracy In Parameter Estimation (AIPE) method
#' 
#' A sample size estimation method used for parameter estimation.
#' The approach aims to find the required sample size, such that
#' the confidence interval has a certain expected width. The
#' function is a simple wrapper function around the \code{\link[MBESS]{ss.aipe.smd}}
#' function.
#' 
#' @param delta Numeric. The expected population effect size.
#' @param confidence_levell Numeric. The desired level of confidence.
#' @param width Numeric. The desired width of the confidence interval, given Delta.
#' @param which_width Character. The desired width of interest. Either `Full`, `Lower` or `Upper`.
#' @param certainity Numeric. The desired certainty of the confidence interval width.
#' @param ... Other arguments to be passed to the \code{\link[MBESS]{ss.aipe.smd}} function.
#' 
#' @return The function returns a list of one named element. The determined
#' sample size for group one and two called `n1`.
#' @export
#' @examples 
#' \dontrun{
#' SampleSizePlanner::ssp_aipe(delta = 0.5, width = 0.2, confidence_level = 0.8)
#' }
ssp_aipe <- function(delta, confidence_level, width, which_width = "Full", certainty = NULL, ...) {
  n1 <- MBESS::ss.aipe.smd(delta = delta, conf.level = confidence_level, width = width, which.width = which_width, certainty = certainty, ...)
  list(n1 = n1)
}
