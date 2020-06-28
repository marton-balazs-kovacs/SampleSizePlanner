power_optim <- function(fun, range, delta, opt, ...) {
  Ns = range
  Res = c(fun(Ns[1], delta, ...), 
          fun(Ns[2], delta, ...))
  if (!(opt > min(Res) & opt < max(Res))) {
    return(
      list(
        n1 = NA_real_,
        npower = NA_real_
        )
      )
    }
  
  while ((min(Ns[Res>opt])-max(Ns[Res<opt]))>1) {
    ResL = Res[which(Ns==max(Ns[Res<opt]))]
    ResH = Res[which(Ns==min(Ns[Res>opt]))]
    NL = max(Ns[Res<opt])
    NH = min(Ns[Res>opt])
    NewN = round((NH-NL)*((opt-ResL)/(ResH-ResL))+NL)
    while (NewN%in%Ns) {
      NewN = ifelse(Res[length(Res)]>opt, NewN - 1, NewN + 1)
      }
    Ns = c(Ns, NewN)
    Res = c(Res, fun(Ns[length(Ns)], delta, ...))
    }
  
  return(
    list(
      n1 = min(Ns[Res>opt]),
      npower = Res[which(Ns==min(Ns[Res>opt]))]
      )
    )
}
