## initialise the OPE. Index 3 (OPE3) means same grid for S and T.
initOPE3 <- function (gr, kappar, rhovecR, Gs, Ws, Gt, Wt, NIG, nuggetR = 0) 
{
  argnms <- names(formals(initOPE3))
  if (inherits(gr, "OPE3")) {
    tmp <- gr
    invisible(lapply(argnms, function(nm) assign(nm, tmp[[nm]])))
  }
  stopifnot(is.function(gr), is.function(kappar), 
            is.list(NIG), all(c("m", "V", "a", "d") %in% names(NIG)))
  stopifnot(nuggetR >= 0)
  stopifnot(is.numeric.vector(NIG$m), is.variance(NIG$V), length(NIG$m) == 
              nrow(NIG$V), is.scalar(NIG$a), is.scalar(NIG$d) && NIG$d >= 
              0)
  OPE <- lapply(structure(argnms, names = argnms), function(nm) get(nm))
  class(OPE) <- "OPE3"
  return(OPE)
}

## predict function
predictOPE3 <- function (OPE, Rp, type = c("Student-t", "EV"), drop = TRUE) 
{
  if (!inherits(OPE, "OPE3")) 
    stop("Expecting an object of class 'OPE3'")
  lapply(names(OPE), function(nm) assign(nm, OPE[[nm]], pos = sys.frame(-2)))
  type <- match.arg(type)
  Grp <- try(gr(Rp))
  if (inherits(Grp, "try-error") || !is.matrix(Grp)) 
    stop("Cannot call 'gr' with argument 'Rp'")
  Gp <- kronecker(Gt, kronecker(Gs, Grp)) #check if this is correct order
  Wrpp <- try(kappar(Rp, theta = rhovecR, delta = nuggetR))
  if (inherits(Wrpp, "try-error") || !is.variance(Wrpp, strict = FALSE)) 
    stop("Cannot call 'kappar' with arguments 'Rp', 'rhovecR' and 'nuggetR'")
  Wpp <- kronecker(Wt, kronecker(Ws, Wrpp)) #check all dimensions and order
  rm(Wrpp) 
  np <- nrow(Grp)
  q <- nrow(Gs)
  l <- nrow(Gt)
  gotEnsemble <- !is.null(OPE$adjust)
  if (!gotEnsemble) {
    EfRp <- drop(Gp %*% NIG$m)
    dim(EfRp) <- c(np, q, l)
    SfRp <- crossprod(tcrossprod(chol(NIG$V), Gp)) + Wpp
    dim(SfRp) <- rep(c(np, q, l), 2)
    ap <- NIG$a
    dp <- NIG$d
  }
  else {
    Wrp <- try(kappar(adjust$R, Rp, theta = rhovecR, delta = nuggetR))
    if (inherits(Wrp, "try-error")) 
      stop("Cannot call 'kappar' with arguments 'R', Rp', 'rhovecR' and 'nuggetR'")
    lapply(names(adjust$other), function(nm) assign(nm, adjust$other[[nm]], 
                                                    pos = sys.frame(-2)))
    
    GtiWWp <- kronecker(t(Gt), kronecker(t(Gs), Hr %*% Wrp))
    
    iWWrp_block_t <- t(iWr %*% Wrp)
    iWWpc <- NULL
    nr <- dim(iWr)[1]
    for(i in 1:(l*q)){
      iWWpc <- c(iWWpc,iWWrp_block_t%*%(c(C)[((i-1)*nr+1):(i*nr)]))
    }
    WpiWWp <- kronecker(Wt,kronecker(Ws, crossprod(backsolve(QWr, Wrp, 
                                                             transpose = TRUE))))
    rm(Wrp)
    iQGtiWWp <- backsolve(Q, GtiWWp, transpose = TRUE)
    Eep <- drop(iWWpc - crossprod(iQGtiWWp, 
                                  iQGtiWc))

    Sep <- Wpp - WpiWWp + crossprod(iQGtiWWp)
    rm(Wpp, WpiWWp) 
    Cbep <- -NIG$V %*% (GtiWWp - crossprod(iQGtiWG, iQGtiWWp))
    EfRp <- drop(Gp %*% adjust$NIG$m) + Eep
    dim(EfRp) <- c(np, q, l)
    tmp <- Gp %*% Cbep
    SfRp <- crossprod(tcrossprod(chol(adjust$NIG$V), Gp)) + 
      Sep + tmp + t(tmp) 
    dim(SfRp) <- rep(c(np, q, l), 2)
    ap <- adjust$NIG$a
    dp <- adjust$NIG$d
  }
  nms <- NULL
  if (gotEnsemble && !is.null(nnms <- colnames(adjust$Y))) 
    nms <- nnms
  if (drop && np == 1) {
    EfRp <- EfRp[1,,]
    dim(SfRp) <- rep(length(EfRp), 2)
    if (!is.null(nms)) 
      names(EfRp) <- nms
  }
  else {
    if (!is.null(nms)) 
      colnames(EfRp) <- nms
    if (!is.null(nms <- rownames(Grp))) 
      rownames(EfRp) <- nms
  }
  if (type == "Student-t")
    return(list(mu = EfRp, Sigma = (dp/ap) * SfRp, df = ap))
  else if (type == "EV") {
    mu <- EfRp
    if (ap <= 1) 
      mu[] <- NA
    Sigma <- (dp/(ap - 2)) * SfRp
    if (ap <= 2) 
      Sigma[] <- NA
    return(list(mu = mu, Sigma = Sigma, sgrid = Sp))
  }
  else stop("Never get here.")
}

## adjust function
adjustOPE3 <- function (OPE, R, rhovecR, Y, nuggetR = 0) 
{
  OPE$rhovecR <- rhovecR # update correlation parameters
  OPE$nuggetR <- nuggetR # update nuggets
  if (!inherits(OPE, "OPE3")) 
    stop("Expecting an object of class 'OPE3'")
  lapply(names(OPE), function(nm) assign(nm, OPE[[nm]], pos = sys.frame(-2)))
  
  Gr <- try(gr(R))
  if (inherits(Gr, "try-error") || !is.matrix(Gr)) 
    stop("Cannot call 'gr' with argument 'R'")
  Wr <- try(kappar(R, theta = rhovecR, delta = nuggetR))
  if (inherits(Wr, "try-error") || !is.variance(Wr)) 
    stop("Cannot call 'kappar' with arguments 'R', 'rhovecR' and 'nuggetR'")
  n <- nrow(Gr)
  q <- nrow(Gs)
  l <- nrow(Gt)
  stopifnot(is.array(Y), identical(dim(Y), c(n, q, l)))
  vr <- ncol(Gr)
  vs <- ncol(Gs)
  vt <- ncol(Gt)
  v <- vr * vs * vt
  stopifnot(length(NIG$m) == v)
  M <- array(NIG$m, dim=c(vr, vs, vt))
  
  Mt <- as.tensor(M, drop = FALSE)
  MG <- ttm(ttm(ttm(Mt, Gt, m = 3), Gs, m = 2), Gr, m = 1)
  MG <- as.array(MG@data, dim=MG@modes, drop=F)
  
  C <- Y - MG
  QWr <- chol(Wr)
  QWs <- chol(Ws)
  QWt <- chol(Wt)
  iWr <- chol2inv(QWr)
  iWs <- qr.solve(Ws)
  iWt <- qr.solve(Wt)
  GtiWG <- kronecker(crossprod(backsolve(QWt, Gt, transpose = TRUE)),
                     kronecker(crossprod(backsolve(QWs, Gs, transpose = TRUE)), 
                               crossprod(backsolve(QWr, Gr, transpose = TRUE))))
  Hr <- crossprod(Gr, iWr)
  Hs <- crossprod(Gs, iWs)
  Ht <- crossprod(Gt, iWt)
  
  C_t <- as.tensor(C, drop = FALSE)
  CH <- ttm(ttm(ttm(C_t, Ht, m = 3), Hs, m = 2), Hr, m = 1)
  CH <- as.array(CH@data, dim=CH@modes, drop=F)
  
  GtiWc <- c(CH)
  
  Q <- chol(chol2inv(chol(NIG$V)) + GtiWG)
  iQall <- backsolve(Q, cbind(GtiWG, GtiWc), transpose = TRUE)
  iQGtiWG <- iQall[, 1:v, drop = FALSE]
  iQGtiWc <- iQall[, v + 1]
  
  CW <- ttm(ttm(ttm(C_t, iWt, m = 3), iWs, m = 2), iWr, m = 1)
  CW <- as.array(CW@data, dim=CW@modes, drop=F)
  
  mhd <- sum(C * CW) - drop(crossprod(iQGtiWc))
  mnew <- NIG$m + drop(NIG$V %*% (GtiWc - crossprod(iQGtiWG, 
                                                    iQGtiWc)))
  Vnew <- chol2inv(Q)
  anew <- NIG$a + length(Y)
  dnew <- NIG$d + mhd
  adjust <- list(R = R, Y = Y)
  adjust$NIG <- list(m = mnew, V = Vnew, a = anew, d = dnew)
  keep <- c("C", "QWr", "iWr", "iWs", "iWt", "Hr", "Q", "iQGtiWG", "iQGtiWc","mhd")
  adjust$other <- lapply(structure(keep, names = keep), function(nm) get(nm))
  OPE$adjust <- adjust
  return(OPE)
}