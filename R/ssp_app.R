ssp_app <- function(confidence, closeness, report_text = FALSE) {
  n1 <- ceiling((qnorm((sqrt(confidence) + 1) / 2) / closeness)^2)
  
  if (report_text) {
    glue::glue("In order to calculate the sample size we choose the METHOD NAME (APP; REF) method. We choose the closeness to be {closeness}, because ... The desired confidence was {confidence} as ... The estimated sample size was {n1}.")
  } else {
    n1
  }
}
