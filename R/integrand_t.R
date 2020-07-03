integrand_t <- function(delta, t, n, nu, mu.delta, gamma, kappa) {
  
  suppressWarnings(
    dt(x = t, df = nu, ncp = sqrt(n) * delta) *
      1 / gamma * dt( (delta - mu.delta) / gamma, df = kappa)
  )
  
}