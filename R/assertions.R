is_not_null <- function(x) {
  !is.null(x)
}

assertthat::on_failure(is_not_null) <- function(call, env) {
  paste(deparse(substitute(x, env)), "argument must be defined")
}

is_positive_number <- function(x) {
  assertthat::assert_that(is_not_null(x))
  assertthat::assert_that(is.numeric(x))
  x > 0
}

assertthat::on_failure(is_positive_number) <- function(call, env) {
  paste(deparse(call$x), "must be a postive number")
}

# if (any(c(opt, band,delta) <= 0)) {
#   rlang::abort("opt, band and delta must be positive numbers")
# }

# if (any(is.null(opt),
#         is.null(band),
#         is.null(delta))) {
#   rlang::abort("opt, band, and delta must be defined")
# }