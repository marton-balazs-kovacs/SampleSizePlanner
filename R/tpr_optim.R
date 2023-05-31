#' Optimize output TPR
#' 
#' The function optimizes the true positive rate for a range of sample sizes
#' for several methods in the package.
#' 
#' @param fun Function. The sample size determining method to use.
#' @param range Integer. Range of sample sizes of interest.
#' @param delta Numeric. The expected population effect size.
#' @param tpr Numeric. The desired long-run probability.
#' @param ... Additional arguments to pass to `fun`.
#' 
#' @return The function returns a list of two. `n1` the resulting sample size and `tpr_out`
#' the associated true positive rate with that sample size.
tpr_optim <- function(fun, range, tpr, ...) {
  Ns = range
  Res = c(fun(Ns[1], ...), 
          fun(Ns[2], ...))
  
  if (tpr < min(Res)) {
    stop(paste0("Your chosen true positive rate (", tpr, ") level is already achieved for n = ", Ns[1], " with ", round(min(Res), 2), " true positive rate!"))
    } else if (tpr > max(Res)) {
      stop(paste0("Your chosen true positive rate (", tpr, ") level cannot be achieved for n < ", Ns[2], "!"))
      }
  
  while ((min(Ns[Res>tpr]) - max(Ns[Res < tpr]))>1) {
    ResL = Res[which(Ns == max(Ns[Res < tpr]))]
    ResH = Res[which(Ns == min(Ns[Res > tpr]))]
    NL = max(Ns[Res < tpr])
    NH = min(Ns[Res > tpr])
    NewN = round((NH - NL) * ((tpr - ResL) / (ResH - ResL)) + NL)
    while (NewN %in% Ns) {
      NewN = ifelse(Res[length(Res)] > tpr, NewN - 1, NewN + 1)
      }
    Ns = c(Ns, NewN)
    Res = c(Res, fun(Ns[length(Ns)], ...))
    }
  
  return(
    list(
      n1 = min(Ns[Res > tpr]),
      tpr_out = Res[which(Ns == min(Ns[Res > tpr]))]
      )
    )
}
