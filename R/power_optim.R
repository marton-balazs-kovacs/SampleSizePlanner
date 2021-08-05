power_optim <- function(fun, range, delta, tpr, ...) {
  Ns = range
  Res = c(fun(Ns[1], delta, ...), 
          fun(Ns[2], delta, ...))
  
  if (tpr < min(Res)) {
    stop(paste0("Your chosen true positive rate (", tpr, ") level is already achieved for n = ", Ns[1], "with", min(Res), "!"))
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
    Res = c(Res, fun(Ns[length(Ns)], delta, ...))
    }
  
  return(
    list(
      n1 = min(Ns[Res > tpr]),
      npower = Res[which(Ns == min(Ns[Res > tpr]))]
      )
    )
}
