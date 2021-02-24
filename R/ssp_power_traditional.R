traditional <- function(n1, delta, alpha = .05) {
  1 - pt(qt((1-alpha), n1*2-2), n1*2-2, delta/sqrt(2/n1))
}

ssp_power_traditional <- function(n_min, n_max, delta, opt, alpha = 0.05) {
  result <- power_optim(fun = traditional, range = c(n_min, n_max), delta = delta, opt = opt, alpha = alpha)
  result
}
