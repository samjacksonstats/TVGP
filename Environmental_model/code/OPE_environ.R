
#Select regression function for OPE
gr <- function(x) {
  as.matrix(cbind(1, x))
}
#Select priors for OPE
local({
  vr <- ncol(gr(t(1:4)))
  vs <- ncol(gr(0))
  vt <- ncol(gr(0))
  m <- rep(0, vr * vs *vt)
  V <- diag(1^2, vr * vs *vt)
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
xDiagnostic_scaled <- matrix(NA,dim(xDiagnostic_descaled)[1],4)
for(i in 1:4){
  xDiagnostic_scaled[,i] <- 2*(xDiagnostic_descaled[,i]-ranges[i,1])/(ranges[i,2]-ranges[i,1]) -1
}
xTraining_scaled <- matrix(NA,dim(xTraining_descaled)[1],4)
for(i in 1:4){
  xTraining_scaled[,i] <- 2*(xTraining_descaled[,i]-ranges[i,1])/(ranges[i,2]-ranges[i,1]) -1
}

#Create output grid objects and regressors for s and t
S <- s
Gs <- gr(Scale(s,a=min(s),b=max(s),l=-1,u=1))
t_design <- t
Gt <- gr(Scale(t,a=min(t),b=max(t),l=-1,u=1))

# Take all MaxPro design point for taining
x_subset <- 1:dim(xTraining_descaled)[1]
R <- xTraining_scaled[x_subset,]
#Input names
colnames(R) <- c("M","D","L", "tau")
#Output for training
Y <- fxTraining[x_subset,,]

#Set correlation functions
kappar <- kappas <- kappat <- GaussianCF

# ML parameter optimisation
cat(paste("Starting parameter optimisation for design size =",training_size,'\n'))
test_params <- stats::optim(par=rep(1,6), fn=ML_OPE,
lower=rep(1e-10,6),
R=R,
t_design=t_design,
Y=Y,
S=S,
method="L-BFGS-B")

#Assign optimised parameters
rhovecR=test_params$par[1:4]; rhovecS=test_params$par[5]; rhovecT=test_params$par[6]; degreeR=2

#Create covariance matrices
Ws <- kappar(S, theta = rhovecS, delta = 1e-3)
Wt <- kappat(t_design, theta = rhovecT, delta = 1e-3)

#Create OPE emulator
cat(paste("Initialising and predicting OPE for design size =",training_size,'\n'))
# set up GP prior. Here r represent inputs, s represent  space, t represent time
myOPE <- initOPE3(gr = gr, kappar = kappar, rhovecR = rhovecR, Gs=Gs, Ws=Ws, Gt=Gt, Wt=Wt, NIG = NIG) 
myOPE <- adjustOPE3(myOPE, R = R, Y = Y, rhovecR = rhovecR, nuggetR=1e-3) # posterior update

#Predict using OPE emulator
mu <- stdev <- array(NA,dim = c(150,15,length(t_design)))
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
  df <- array(pp2$df,dim = c(150,15,length(t_design)))
}

#Save all predictions
assign(paste('OPE_env_prediction',training_size,'_log',sep=''),list(mu=mu,stdev=stdev,df=df, opt_params = test_params$par))
}

# #Save all into file
# save(x20,
#      fx20,
#      lfx20,
#      x50,
#      fx50,
#      lfx50,
#      x150,
#      fx150,
#      lfx150,
#      OPE_20_training_log,
#      OPE_50_training_log,
#      file='Environ_OPE_20_and_50_log.RData')

