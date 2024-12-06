#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Install original 2D OPE package locally
if(!('OPE' %in% installed.packages()[, "Package"])) {
  install.packages(paste(getwd(),"/OPE_0.7.tar.gz", sep=''), repos = NULL, type="source")
}
if(!('rTensor' %in% installed.packages()[, "Package"])) {
  install.packages('rTensor')
}

library(OPE)
library(rTensor)

source("code/ML_OPE.R")#Maximum Likelihood for OPE
source("code/OPE 3D extension.R")#Source 3D OPE extension
load("code/patch model data.RData")


#Select regression function for OPE
gr <- function(x) {
  as.matrix(cbind(1, x))
}
#Select priors for OPE
local({
  vr <- ncol(gr(t(1:3)))#ncol(gr(0))
  vs <- ncol(gr(0))#ncol(gr(t(1:2)))#1#
  vt <- ncol(gr(0))
  m <- rep(0, vr * vs *vt)
  V <- diag(1, vr * vs *vt)
  a <- 1
  d <- 1^2
  NIG <<- list(m = m, V = V, a = a, d = d)
})

#Loop over design sizes 
for(training_size in c(20,50)){
  
xTraining_descaled <- get(paste('x',training_size,sep=''))
fxTraining <- get(paste('lfx',training_size,sep=''))
xDiagnostic_descaled <- x150
fxDiagnostic <- lfx150

#Scale the inputs to [-1,1]
xDiagnostic_scaled <- matrix(NA,dim(xDiagnostic_descaled)[1],3)
for(i in 1:3){
  xDiagnostic_scaled[,i] <- 2*(xDiagnostic_descaled[,i]-ranges[i,1])/(ranges[i,2]-ranges[i,1]) -1
}
xTraining_scaled <- matrix(NA,dim(xTraining_descaled)[1],3)
for(i in 1:3){
  xTraining_scaled[,i] <- 2*(xTraining_descaled[,i]-ranges[i,1])/(ranges[i,2]-ranges[i,1]) -1
}

# Add a scaling function to scale the Gt and Gs to [-1,1] based on their actual ranges
scaling <- function(x){
  x_s <- NULL
  x_max <- max(x)
  x_min <- min(x)
  for(i in 1:length(x)){
    x_s <- c(x_s,2*(x[i]-x_min)/(x_max-x_min) -1)
  }
  return(x_s)
}

n_patches <- length(patchNames)
Y <- fxTraining
order_max_inf <- sapply(seq_len(dim(Y)[1]),function(i) order(apply(Y[i,,],1,which.max)))
order_max_inf <- order(rowMeans(order_max_inf))
dist_max_inf <- order_max_inf-order_max_inf[15]
S <- scaling(dist_max_inf)
Gs <- gr(scaling(patch_sizes))

t_design <- 1:150
Gt <- gr(scaling(t_design))

# Take all LH design point for training
x_subset <- 1:dim(xTraining_descaled)[1]
R <- xTraining_scaled[x_subset,]
colnames(R) <-  c( "beta_u", "alpha", "gamma_u")
fxTraining <- replace(fxTraining, fxTraining<=0, 0)
Y <- fxTraining[x_subset,,]

  kappar <- GaussianCF
  kappas <- GaussianCF
  kappat <- GaussianCF
  
  cat(paste("Starting parameter optimisation for design size =",training_size,'\n'))
  test_params <- stats::optim(par=rep(1,5), fn=ML_OPE,
                              lower=rep(1e-3,5),
                              R=R,
                              t_design=scaling(t_design),
                              Y=Y[,,t_design],
                              S=S,
                              method="L-BFGS-B")

  rhovecR=test_params$par[1:3]; rhovecS=test_params$par[4]; rhovecT=test_params$par[5]; degreeR=2
  Ws <- kappas(S, theta = rhovecS, delta = 1e-3)
  Wt <- kappat(scaling(t_design), theta = rhovecT, delta = 1e-3)

cat(paste("Initialising and predicting OPE for design size =",training_size,'\n'))
myOPE <- initOPE3(gr = gr, kappar = kappar, rhovecR = rhovecR, Gs=Gs, Ws=Ws, Gt=Gt, Wt=Wt, NIG = NIG) # set up GP prior
# Here r represent inputs, s represent  patches, T represent time
myOPE <- adjustOPE3(myOPE, R = R, Y = Y[,1:n_patches,t_design], rhovecR = rhovecR, nuggetR=1e-3) # posterior update

mu <- stdev <- array(NA,dim = c(150,n_patches,length(t_design)))
df <- NULL
for(ind in 3*(1:50)){
  cat(paste("Curernt input prediction",ind,'out of 150 \n'))
  x_sample <- (ind-2):ind
  pp2 <- predictOPE3(myOPE, Rp = xDiagnostic_scaled[x_sample,],  type='Student-t')
  mu[(ind-2):ind,,] <- pp2$mu
  stdev[(ind-2):ind,,] <- array(0, dim=dim(pp2$Sigma)[1:3])
  for(i in 1:dim(stdev[(ind-2):ind,,])[1]){
    for(j in 1:dim(stdev[(ind-2):ind,,])[2]){
      for(k in 1:dim(stdev[(ind-2):ind,,])[3]){
        stdev[ind-3+i,j,k] <- sqrt(pp2$Sigma[i,j,k,i,j,k])
      }
    }
  }
  df = c(df,rep(pp2$df, length(mu)))
}

assign(paste('OPE_patch_prediction',training_size,'_log',sep=''),list(mu=mu,stdev=stdev,df=df, opt_params = test_params$par))

}