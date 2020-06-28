ssp_aipe <- function(n_min, n_max, delta, opt) {
  result <- power_optim(fun = traditional, range = c(n_min, n_max), delta = delta, opt = opt)
  if(!is.null(result)) {
    return(
      list(n1 = result[1] %>% purrr::flatten_dbl(),
           npower = result[2] %>% purrr::flatten_dbl())
    )
  } else {
    stop("Your chosen confidence level cannot be achieved within this range of sample sizes!")
  }
}
