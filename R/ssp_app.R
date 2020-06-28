ssp_app <- function(confidence, closeness) {
  n1 <- ceiling((qnorm((sqrt(confidence) + 1) / 2) / closeness)^2)
  
  return(
    list(
      n1 = n1
    )
  )
}