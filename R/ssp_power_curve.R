#' Calculating a power curve
#' 
#' lorem ipsum
#' 
#' @param delta Numeric. the range of hypothetical population effect sizes
#' @param tpr Numeric. the desired long run probability of obtaining a significant result with TOST, given Delta
#' @param animated Logical. if TRUE the output plot is animated
#' 
#' @return The function returns a ggplot2 object.
#' @export
ssp_power_curve <- function(delta, tpr, max_n = 5000) {
  n1 <- NULL
  npower <- NULL
  # delta <- seq(delta1, delta2, 0.01)
  for (i in 1:length(delta)) {
    res = power_optim(fun = traditional, range = c(5, max_n), delta[i], tpr = tpr)
    n1[i] = res$n1
    npower[i] = res$npower
  }
  
  list(
    delta = delta,
    n1 = n1,
    npower = npower
  )
}
