### Run Environmental Model ###

#Generate outut for generated designs x20, x50 and x150
fx20 <- array(unlist(apply(x20, 1, environ)), dim=c(s_n,t_n,20))
fx20 <- aperm(fx20, c(3,1,2))
lfx20 <- log(fx20+1)
fx50 <- array(unlist(apply(x50, 1, environ)), dim=c(s_n,t_n,50))
fx50 <- aperm(fx50, c(3,1,2))
lfx50 <- log(fx50+1)
fx150 <- array(unlist(apply(x150, 1, environ)), dim=c(s_n,t_n,150))
fx150 <- aperm(fx150, c(3,1,2))
lfx150 <- log(fx150+1)
