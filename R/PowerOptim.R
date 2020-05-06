PowerOptim <- function (Fun, Range, Arguments, Opt) {
  Ns = Range
  Res = c(Fun(Ns[1], Arguments[1], Arguments[2]), 
          Fun(Ns[2], Arguments[1], Arguments[2]))
  if (!(Opt>min(Res)&Opt<max(Res))) {return (NULL)}
  
  while ((min(Ns[Res>Opt])-max(Ns[Res<Opt]))>1)
  {
    ResL = Res[which(Ns==max(Ns[Res<Opt]))]
    ResH = Res[which(Ns==min(Ns[Res>Opt]))]
    NL = max(Ns[Res<Opt])
    NH = min(Ns[Res>Opt])
    NewN = round((NH-NL)*((Opt-ResL)/(ResH-ResL))+NL)
    while (NewN%in%Ns)
    {
      NewN = ifelse(Res[length(Res)]>Opt, NewN - 1, NewN + 1)
    }
    Ns = c(Ns, NewN)
    Res = c(Res, Fun(Ns[length(Ns)], Arguments[1], Arguments[2]))
  }

  c(min(Ns[Res>Opt]), Res[which(Ns==min(Ns[Res>Opt]))])
}