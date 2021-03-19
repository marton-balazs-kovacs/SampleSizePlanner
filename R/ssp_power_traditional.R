ssp_power_traditional <- function(max_n, delta, tpr, alpha = 0.05) {
  power_optim(fun = traditional, range = c(4, max_n), delta = delta, tpr = tpr, alpha = alpha)
}

traditional <- function(n1, delta, alpha = .05) {
  1 - pt(qt((1-alpha), n1*2-2), n1*2-2, delta/sqrt(2/n1))
}

