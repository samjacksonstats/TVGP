
# Pulling data for plotting
run_subset <- c(9,23,38)# x_sample
patch_subset <- c(2,3,13,15)
t_design <- 1:150
plots <- list()
for(training_size in c(20,50)){
direct_flu_prediction <- get(paste('PPE_patch_prediction',training_size,'_log',sep=''))
fxD <- lfx150
OPE_pred <- get(paste('OPE_patch_prediction',training_size,'_log',sep=''))
mu <- OPE_pred$mu
stdev <- OPE_pred$stdev
df <- OPE_pred$df
k <- 1
for(j in patch_subset){
  for(i in run_subset){
    DF3 <- NULL
    DF3 <- rbind(DF3, data.frame(X = t_design,
                                 Mean = fxD[i,j,t_design],
                                 min = fxD[i,j,t_design],
                                 max = fxD[i,j,t_design],
                                 Group=as.factor('Output')#paste('Output:',j,'input:', i)
    ))
    DF3 <- rbind(DF3, data.frame(X = t_design,
                                 Mean = direct_flu_prediction$mu[i,j,t_design],
                                 min = direct_flu_prediction$mu[i,j,t_design]-
                                  direct_flu_prediction$stdev[i,j,t_design]*qnorm(0.975),
                                 max = direct_flu_prediction$mu[i,j,t_design]+
                                  direct_flu_prediction$stdev[i,j,t_design]*qnorm(0.975),
                                 Group=as.factor('PPE')#paste('PPE:',j,'input:', i)
    ))
    DF3 <-  rbind(DF3, data.frame(X = t_design,
                                  Mean = mu[i,j,t_design],
                                  min = mu[i,j,t_design]-stdev[i,j,t_design] * qt(0.975, df = df[i]),
                                  max = mu[i,j,t_design]+stdev[i,j,t_design] * qt(0.975, df = df[i]),
                                  Group=as.factor('OPE')#paste('OPE:',j, 'input:', i)
    ))
    
    g <- ggplot(DF3,aes(X, Mean)) +
      geom_ribbon(aes(ymin=min, ymax=max, fill=Group),  alpha=0.3)+
      # guides(linetype = guide_legend(override.aes = list(size = c(.5,.5,2))))+
      scale_fill_manual(values=rep(c("#ffffff00", "#00BA38", "#F8766D"),150))+
      geom_line(aes(group=Group, col=Group, size=Group)) +
      scale_size_manual(values=c(1,1,1))+ 
      scale_colour_manual(values=rep(c("#619cffff", "#00BA38", "#F8766D"),150))+
      ggtitle(paste('Input: ',i,' Patch: ',j, sep=''))+
      # ggtitle(paste('Input: ',i,'Patch: ',patchNames[j]))+
      # guides(linetype = guide_legend(override.aes = list(linewidth = c(.5,.5,2))),
      #        fill = guide_legend(override.aes = list( alpha=c(.3,.3, 0))))+
      xlab("")+ylab("")+theme(legend.title=element_blank())
    plots[[k]] <- g
    k <- k+1
  }
}

plot <- ggpubr::ggarrange(plots[[1]],plots[[2]],plots[[3]],
                  plots[[4]],plots[[5]],plots[[6]],
                  plots[[7]],plots[[8]],plots[[9]],
                  plots[[10]],plots[[11]],plots[[12]],
                  ncol=3, nrow=4, common.legend = TRUE, legend="bottom")
print(annotate_figure(plot,top = text_grob(paste("Training size: ",training_size,sep=""),  size = 15)))
}

time_d <- 150


thin_sample <- sample(1:length(as.vector(lfx150[,,1:time_d])), 1e4)
par(mfcol = c(2, 2),
    oma = c(1,1,0,0) + 0.1,
    mar = c(2,2,1,1) + 0.1)

# par(oma = c(0,0,0.5,0.5) + 0.1,
#     mar = c(4,4,0,0) + 0)

for(training_size in c("20","50")){
  OPE_pred <- get(paste('OPE_patch_prediction',training_size,'_log',sep=''))
  mu <- OPE_pred$mu
  stdev <- OPE_pred$stdev
  df <- OPE_pred$df
  
  y <- as.vector(mu[,,1:time_d])[thin_sample]
  x <- as.vector(lfx150[,,1:time_d])[thin_sample]
  sdev <-  sqrt(as.vector(stdev[,,1:time_d])[thin_sample])
  plot(1, type="n", xlab=expression(log(f(bold(x))+1)),ylab=expression(mu(bold(x))), xlim=c(0,13), ylim=c(-4,15),cex.axis = 1.3,cex.lab=1.3)
  arrows(x, y-3*sdev, x, y+3*sdev, length=0.1, angle=90, code=3, col=4)
  points(x,y, pch=19, col=2)
  points(x=c(0,17),y=c(0,17), type='l', lwd=2)
  title(paste('Influenza OPE : ', training_size, sep=''))#, adj = .95, line = -16, cex.main =2)
  
  
  direct_flu_prediction <- get(paste('PPE_patch_prediction',training_size,'_log',sep=''))
  y <- as.vector(direct_flu_prediction$mu[,,1:time_d])[thin_sample]
  sdev <-  as.vector(direct_flu_prediction$stdev[,,1:time_d])[thin_sample]
  plot(1, type="n", xlab=expression(log(f(bold(x))+1)),ylab=expression(mu(bold(x))), xlim=c(0,13), ylim=c(-4,15),cex.axis = 1.3,cex.lab=1.3)
  
  apply(cbind(x, y, sdev), 1, \(z) {
   if(6 * z[3] > 0.01) {
    arrows(z[1], z[2]-3*z[3], z[1], z[2] + 3*z[3], length=0.1, angle=90, code=3, col=4)
   }
  }
  )
  
  points(x,y, pch=19, col=2)
  points(x=c(0,17),y=c(0,17), type='l', lwd=2)
  title(paste('Influenza PPE : ', training_size, sep=''))#, adj = .95, line = -16, cex.main=2)
}
par(mfrow = c(1, 1))
