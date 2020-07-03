ssp_aipe <- function(n_min, n_max, delta, opt, report_text = FALSE) {
  result <- power_optim(fun = traditional, range = c(n_min, n_max), delta = delta, opt = opt)
  if(!all(is.na(result))) {
    if (report_text) {
      glue::glue("In order to calculate the sample size we choose the accuracy in parameter estimation (AIPE; REF) method. We choose the expected effect size to be {delta}, because ... The desired power was {opt} as ... Because of limits in resources the minimum sample size of interest was {n_min} and the maximum sample size of interest was {n_max}. The resulting confidence of the sample size estimation method was {purrr::pluck(result, \"n1\")} with the estimated sample size of {purrr::pluck(result, \"npower\")}.")
      } else {
        result
        }
    } else {
    stop("Your chosen confidence level cannot be achieved within this range of sample sizes!")
  }
}
