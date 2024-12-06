### MaxPro Design ###
if(!('MaxPro' %in% installed.packages()[, "Package"])) {
  install.packages('MaxPro')
}

library( 'MaxPro' )

# 3 designs - of size 150, 50 and 20.

# Dimension
d = 4

ranges <- matrix( c( 7, 13,
                     0.02, 0.12,
                     0.01, 3,
                     30.01, 30.295), nrow = d, byrow = TRUE )

# Diagnostic runs - of size 150.

m = 150

# Use MLH
initial_design <- MaxPro::MaxProLHD( n = m, p = d, itermax = 20 )
Maxpro_design <- MaxPro::MaxPro( InitialDesign = initial_design$Design, iteration = 10 )
x150 <- QuickFunc::Scale( Maxpro_design$Design, l = ranges[,1], u = ranges[,2] )
colnames(x150)=c("M","D","L", "tau")

# Plot and have a look.
# graphics::pairs( x150, pch = 16 )

# Training runs - of size 50.

m = 50

# Use MLH
initial_design <- MaxPro::MaxProLHD( n = m, p = d, itermax = 20 )
Maxpro_design <- MaxPro::MaxPro( InitialDesign = initial_design$Design, iteration = 10 )
x50 <- QuickFunc::Scale( Maxpro_design$Design, l = ranges[,1], u = ranges[,2] )
colnames(x50)=c("M","D","L", "tau")

# Plot and have a look.
# graphics::pairs( x50, pch = 16 )

# Training runs - of size 20.

m = 20

# Use MLH
initial_design <- MaxPro::MaxProLHD( n = m, p = d, itermax = 20 )
Maxpro_design <- MaxPro::MaxPro( InitialDesign = initial_design$Design, iteration = 10 )
x20 <- QuickFunc::Scale( Maxpro_design$Design, l = ranges[,1], u = ranges[,2] )
colnames(x20)=c("M","D","L", "tau")
