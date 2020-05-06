Tpow <- function (n1, Band = NA, delta, alpha = .05) {
  1 - pt (qt ((1-alpha), n1*2-2), n1*2-2, delta/sqrt(2/n1))
}