
# Create DataFrame for Plotting

# # Select the same point as in the paper
# input <- c(M = 10, D = 0.07, L = 1.505, tau = 30.152)
# lfxD <- log(environ(input)+1)

# Select a random point in the test set
select_point <- sample(1:150,1)
lfxD <- lfx150
# select design size
for(size in c("20","50")){
OPE_prediction <- get(paste('OPE_env_prediction',size,'_log', sep=""))
mu_OPE <- OPE_prediction$mu
stdev_OPE <- OPE_prediction$stdev
df_OPE <- OPE_prediction$df

PPE_prediction <- get(paste('PPE_env_prediction',size,'_log', sep=""))
mu_PPE <- PPE_prediction$mu
stdev_PPE <- PPE_prediction$stdev

plots <- list()
k <- 1
for(j in 1:15){
  for(i in select_point){
    DF3 <- NULL
    DF3 <-  rbind(DF3, data.frame(X = t,
                                  Mean = mu_OPE[i,j,],
                                  min = mu_OPE[i,j,]+stdev_OPE[i,j,] * qt(0.975, df = df_OPE[i,j,]),
                                  max = mu_OPE[i,j,]-stdev_OPE[i,j,] * qt(0.975, df = df_OPE[i,j,]),
                                  Group=as.factor('OPE')#paste('OPE:',j, 'input:', run_subset[i])
    ))
    DF3 <- rbind(DF3, data.frame(X = t,
                                 Mean = mu_PPE[i,j,],
                                 min = mu_PPE[i,j,]+
                                   stdev_PPE[i,j,]*qnorm(0.975),
                                 max = mu_PPE[i,j,]-
                                   stdev_PPE[i,j,]*qnorm(0.975),
                                 Group=as.factor('PPE')#paste('PPE:',j,'input:', run_subset[i])
    ))
    DF3 <- rbind(DF3, data.frame(X = t,
                                 Mean = lfxD[i,j,],
                                 min = lfxD[i,j,],
                                 max = lfxD[i,j,],
                                 Group=as.factor('Output')#paste('Output:',j,'input:', run_subset[i])
    ))



  # ggplot2 LineGraph with Shading Confidence Interval
  g <- ggplot(DF3,aes(X, Mean)) +
    geom_line(aes(group=Group, col=Group, linewidth=Group)) +
    scale_linewidth_manual(values=c(.5,.5,.5))+
    geom_ribbon(aes(ymin=min, ymax=max, fill=Group),  alpha=.3)+
    scale_fill_manual(values=rep(c("#F8766D", "#00BA38", "#ffffff00"),200))+
    ggtitle(paste('Space coordinate',round(s[j],2)))+
      xlab("time")+ylab("")

  plots[[k]] <- g
  k <- k+1
  }
}

plot <- ggarrange(plots[[1]], plots[[2]], ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
print(annotate_figure(plot,top = text_grob(paste("Trainig size: ",size,sep=""),  size = 15)))



plot <- ggarrange(plots[[1]],plots[[2]],plots[[3]],
             plots[[4]],plots[[5]],plots[[6]],
             plots[[7]],plots[[8]],plots[[9]],
             plots[[10]],plots[[11]],plots[[12]],
             plots[[13]],plots[[14]],plots[[15]],
             ncol=3,
             nrow=5,
             common.legend = TRUE, legend="bottom")
print(annotate_figure(plot,top = text_grob(paste("Training size: ",size,sep=""),  size = 15)))

}

time_d <- 100
thin_sample <- sample(1:length(as.vector(lfxD[,,1:time_d])), 1e4)

par(mfcol = c(2, 2),
    oma = c(1,1,0,0) + 0.1,
    mar = c(2,2,1,1) + 0.1)

# par(oma = c(0,0,0.5,0.5) + 0.1,
#     mar = c(4,4,0,0) + 0)
for(size in c("20","50")){
  OPE_pred <- get(paste('OPE_env_prediction',size,'_log', sep=""))
  y <- as.vector(OPE_pred$mu[,,1:time_d])[thin_sample]
  x <- as.vector(lfxD[,,1:time_d])[thin_sample]
  sdev <-  sqrt(as.vector(OPE_pred$stdev[,,1:time_d])[thin_sample])
  plot(1, type="n", xlab=expression(log(f(bold(x))+1)),ylab=expression(mu(bold(x))), xlim=c(0,5), ylim=c(-1,6),cex.axis = 1.3,cex.lab=1.3)
   arrows(x, y-3*sdev, x, y+3*sdev, length=0.1, angle=90, code=3, col=4)
  points(x,y, pch=19, col=2)
  points(x=c(0,17),y=c(0,17), type='l', lwd=2)
  title(paste('Environmental model : OPE n=', size, sep=''))#, adj = .95, line = -16, cex.main =2)
  
  
  PPE_prediction <- get(paste('PPE_env_prediction',size,'_log', sep=""))
  stdev_PPE <- as.vector(PPE_prediction$stdev[,,1:time_d])[thin_sample]
  y <- as.vector(PPE_prediction$mu[,,1:time_d])[thin_sample]
  plot(1, type="n", xlab=expression(log(f(bold(x))+1)),ylab=expression(mu(bold(x))), xlim=c(0,5), ylim=c(-1,6),cex.axis = 1.3,cex.lab=1.3)
  apply(cbind(x, y, stdev_PPE), 1, \(z) {
        if(6 * z[3] > 0.01) {
         arrows(z[1], z[2]-3*z[3], z[1], z[2] + 3*z[3], length=0.1, angle=90, code=3, col=4)
        }
   }
  )
  points(x,y, pch=19, col=2)
  points(x=c(0,17),y=c(0,17), type='l', lwd=2)
  title(paste('Environmental model : PPE n=', size, sep=''))#, adj = .95, line = -16, cex.main=2)
}

par(mfcol = c(1, 1))


if(!('plotly' %in% installed.packages()[, "Package"])) {
  install.packages('plotly')
}

# Specify axis titles and camera angle for 3D plot
axx <- list(
  title = "space"
)

axy <- list(
  title = "time"
)

axz <- list(
  title = "log(f(x)+1)"
)
camera = list(eye = list(x = -1.7, y = -1.7, z = 1))

# Create data frames for plotting
#OPE
OPE_data <- cbind(expand.grid(s=s,t=t),z=c(mu_OPE[select_point,,]))
#PPE
PPE_data <- cbind(expand.grid(s=s,t=t),z=c(mu_PPE[select_point,,]))
#Full response surface for extended grid
s_full = seq(from=0.5, to=2.5, length.out = 100)
t_full = seq(from=0.3, to=60, by=0.3)
input <- x150[select_point,]
output <- log(environ(input, s=s_full, t=t_full)+1)
z = t(output)

# Surface + mean prediction scatter
# OPE
fig <- plot_ly(OPE_data,
        x = ~s,
        y = ~t,
        z = ~z,
        type = "scatter3d",
        mode = "markers",
        size=1) %>% add_surface(x = s_full, y = t_full, z = t(output) ,opacity = 0.8, showlegend = FALSE) %>% layout(
                                                                                                            scene = list(
                                                                                                            xaxis=axx,
                                                                                                            yaxis=axy,
                                                                                                            zaxis=list(title = "OPE mean"),
                                                                                                            camera = camera
                                                                                                            )                                                                                                            )
suppressWarnings(print(fig))

# Surface + mean prediction scatter
# PPE
fig <- plot_ly(PPE_data,
        x = ~s,
        y = ~t,
        z = ~z,
        type = "scatter3d",
        mode = "markers",
        size=1) %>% add_surface(x = s_full, y = t_full, z = t(output) ,opacity = 0.8, showlegend = FALSE) %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=list(
                                                                                                            title = "PPE mean"),camera = camera))
suppressWarnings(print(fig))

# Surface plots
# Response f(x)
fig <- plot_ly(x = s_full, y = t_full, z = z) %>% add_surface() %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz,
  camera = camera))
print(fig)

# OPE mean
OPE_mean=t(mu_OPE[select_point,,])
fig <- plot_ly(x = s, y = t, z = OPE_mean) %>% add_surface() %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=list(
  title = "OPE mean"),
  camera = camera))
print(fig)

# PPE mean
PPE_mean=t(mu_PPE[select_point,,])
fig <- plot_ly(x = s, y = t, z = PPE_mean) %>% add_surface() %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=list(
  title = "PPE mean"),
  camera = camera))
print(fig)

