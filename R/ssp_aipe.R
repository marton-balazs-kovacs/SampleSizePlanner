ssp_aipe <- function(n_min, n_max, delta, confidence, alpha = 0.05) {
  result <- power_optim(fun = traditional, range = c(n_min, n_max), delta = delta, opt = confidence, alpha =alpha)
  if(!all(is.na(result))) {
    result
    } else {
      stop("Your chosen confidence level cannot be achieved within this range of sample sizes!")
      }
}
