ML_OPE <- function(params, R, t_design, Y, S){
  rhovecR=params[-((length(params)-1):length(params))]
  rhovecS=params[length(params)-1]
  rhovecT=params[length(params)]
  Ws <- kappas(X=S, theta=rhovecS, delta = 1e-3)
  Wt <- kappat(X=t_design, theta = rhovecT, delta = 1e-3)
  myOPE <- initOPE3(gr = gr, kappar = kappar, rhovecR = rhovecR, Gs=Gs, Ws=Ws, Gt=Gt, Wt=Wt, NIG = NIG) # set up GP prior
  # Here r represent inputs, s represent  patches, T represent time
  myOPE <- adjustOPE3(myOPE, R = R, Y = Y, rhovecR = rhovecR, nuggetR=1e-3)# posterior update
  ML <- myOPE$adjust$other$mhd
  return(ML)
}
