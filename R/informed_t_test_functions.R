#' Informed t-test functions
#' 
#' Functions created to calculate Bayes factor with informed priors
#' for t-tests. The functions were created for the
#' Gronau, Q. F., Ly, A., & Wagenmakers, E. J. (2019).
#' Informed Bayesian t-tests. \emph{The American Statistician.} paper.
#' The functions are retrieved from \href{https://osf.io/bsp6z/}{OSF}.
#' @importFrom stats integrate dt
#' @name informed_t_test_functions
NULL


#' @rdname informed_t_test_functions
cdf_normal <- function(x,
                       t,
                       n1,
                       n2 = NULL,
                       independentSamples = FALSE,
                       prior.mean,
                       prior.variance,
                       rel.tol = .Machine$double.eps^0.25) {
  
  out <- stats::integrate(posterior_normal, lower = -Inf, upper = x,
                   t = t, n1 = n1, n2 = n2,
                   independentSamples = independentSamples,
                   prior.mean = prior.mean,
                   prior.variance = prior.variance,
                   rel.tol = rel.tol)$value
  
  # catch numerical errors
  if (out > 1 & out < 1.001) {
    out <- 1
    warning(
      "Numerical integration yields a CDF value slightly larger than 1. The CDF value has been replaced by 1.",
      call. = FALSE
    )
  }
  
  return(out)
  
}

#' @rdname informed_t_test_functions
cdf_t <- function(x,
                  t,
                  n1,
                  n2 = NULL,
                  independentSamples = FALSE,
                  prior.location,
                  prior.scale,
                  prior.df,
                  rel.tol = .Machine$double.eps^0.25) {
  
  out <- stats::integrate(posterior_t,
                          lower = -Inf, upper = x,
                          t = t, n1 = n1, n2 = n2,
                          independentSamples = independentSamples,
                          prior.location = prior.location,
                          prior.scale = prior.scale,
                          prior.df = prior.df,
                          rel.tol = rel.tol)$value
  
  # catch numerical errors
  if (out > 1 & out < 1.001) {
    out <- 1
    warning(
      "Numerical integration yields a CDF value slightly larger than 1. The CDF value has been replaced by 1.",
      call. = FALSE
    )
  }
  
  return(out)
  
}

#' @rdname informed_t_test_functions
integrand_t <- function(delta, t, n, nu, mu.delta, gamma, kappa) {
  
  suppressWarnings(
    stats::dt(x = t, df = nu, ncp = sqrt(n) * delta) *
      1 / gamma * stats::dt( (delta - mu.delta) / gamma, df = kappa)
  )
  
}

#' @rdname informed_t_test_functions
posterior_t <- function(delta,
                        t,
                        n1,
                        n2 = NULL,
                        independentSamples = FALSE,
                        prior.location,
                        prior.scale,
                        prior.df,
                        rel.tol = .Machine$double.eps^0.25) {
  
  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  
  mu.delta <- prior.location
  gamma <- prior.scale
  kappa <- prior.df
  
  numerator <- suppressWarnings(
    stats::dt(x = t, df = nu, ncp = sqrt(neff) * delta) *
      1 / gamma * stats::dt( (delta - mu.delta) / gamma, df = kappa)
  )
  
  denominator <- stats::integrate(integrand_t,
                           lower = -Inf, upper = Inf,
                           t = t, n = neff, nu = nu,
                           mu.delta = mu.delta,
                           gamma = gamma,
                           kappa = kappa,
                           rel.tol = rel.tol)$value
  
  out <- numerator / denominator
  out[is.na(out)] <- 0
  
  return(out)
  
}

#' @rdname informed_t_test_functions
posterior_normal <- function(delta,
                             t,
                             n1,
                             n2 = NULL,
                             independentSamples = FALSE,
                             prior.mean,
                             prior.variance) {
  
  neff <- ifelse(independentSamples, n1 * n2 / (n1 + n2), n1)
  nu <- ifelse(independentSamples, n1 + n2 - 2, n1 - 1)
  
  mu.delta <- prior.mean
  g <- prior.variance
  
  numerator <- stats::dt(x = t, df = nu, ncp = sqrt(neff) * delta) *
    dnorm(x = delta, mean = mu.delta, sd = sqrt(g))
  
  denominator <- 1 / sqrt(1 + neff * g) *
    stats::dt(x = t / sqrt(1 + neff * g),
       df = nu,
       ncp = sqrt(neff / (1 + neff * g)) * mu.delta)
  
  out <- numerator / denominator
  out[is.na(out)] <- 0
  
  return(out)
  
}