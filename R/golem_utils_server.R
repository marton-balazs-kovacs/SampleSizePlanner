# Inverted versions of in, is.null and is.na
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

# Removes the null from a vector
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

# If x is null, return y, otherwise return x
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}
# If x is NA, return y, otherwise return x
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

# typing reactiveValues is too long
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

# info logo in name html wrapper
name_with_info <- function(name, description) {
  HTML(
    '<div title="',
    description,
    '">',
    name,
    '<i class="fas fa-info"></i>',
    '</div>')
}

# f2 function
get_f2 <- function(mu, sigma) {
  
  # arrange into 2by2
  mu_mat <- matrix(mu, nrow = 2, byrow = TRUE,
                   dimnames = list(A = c("A1","A2"),
                                   B = c("B1","B2")))
  
  # margins & grand mean
  mu_i_dot <- rowMeans(mu_mat)
  mu_dot_j <- colMeans(mu_mat)
  mu_dot   <- mean(mu_mat)        
  
  # main‐effect f
  fA  <- sqrt(sum((mu_i_dot-mu_dot)^2) / 2) / sigma
  fB  <- sqrt(sum((mu_dot_j-mu_dot)^2) / 2) / sigma
  
  # interaction deviations d_ij
  d_mat <- mu_mat -
    outer(mu_i_dot, rep(1,2)) -  # subtract A 
    outer(rep(1,2), mu_dot_j) +  # subtract B 
    mu_dot                      # add back grand mu
  
  # interaction f
  fAB <- sqrt(mean(d_mat^2)) / (2 * sigma)
  
  c(fA = fA, fB = fB, fAB = fAB)
}
