ssp_bfda <- function(delta, thresh = 10, power = 0.8, n_rep = 1000, report_text = FALSE) {
  ns = NULL
  bfs = NULL
  for (i in 1:n_rep) {
    n1 = n2 = 4
    plac = rnorm(n1, 0, 1)
    treat = rnorm(n2, delta, 1)
    bf = as.vector(ttestBF(x = treat, y = plac, nullInterval = c(0, Inf), rscale = 1/sqrt(2)))[1]
    
    while (bf < thresh & bf>(1/thresh) & n1<1500) {
      n1 = n2 = n1 + 1
      Plac = c(plac, rnorm(1, 0, 1))
      Treat = c(treat, rnorm(1, delta, 1))
      bf = as.vector(ttestBF(x = treat, y = plac, nullInterval = c(0, Inf), rscale = 1/sqrt(2)))[1]
    }
    ns[i] = n1
    bfs[i] = bf
  }
  nopt = sort(ns)[round(length(ns) * power)]
  bfha = mean(bfs > thresh)
  
  if (report_text) {
    glue::glue("In order to calculate the sample size we choose the Bayes factor design analysis (BFDA; REF) method. We choose the power to be {power} because ... The expected delta was {delta} as ... We run the sequential analysis {n_rep} times. The estimated sample size was {nopt} with {bfha)}.")
  } else {
    list(
      nopt = nopt,
      bfha = bfha
    )
  }
}