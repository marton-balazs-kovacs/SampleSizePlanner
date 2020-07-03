traditional <- function(n1, delta, alpha = .05) {
  1 - pt(qt((1-alpha), n1*2-2), n1*2-2, delta/sqrt(2/n1))
}

ssp_power_traditional <- function(n_min, n_max, delta, opt, report_text = FALSE) {
  result <- power_optim(fun = traditional, range = c(n_min, n_max), delta = delta, opt = opt)
  if (report_text) {
    glue::glue("In order to calculate the sample size we choose the traditional power (REF) method. We choose the power to be {opt} because ... The expected delta was {delta} as ... Because of limits in resources the minimum sample size of interest was {n_min} and the maximum sample size of interest was {n_max}. The estimated sample size was {purrr::pluck(result, \"n1\")} with {purrr::pluck(result, \"npower\")} estimated power.")
  } else {
    result
  }
}
