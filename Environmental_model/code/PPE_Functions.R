### Environmental Model Emulation and Prediction Functions ###

PPE_build <- function( x, fx, ranges ){
  
  # check input conditions.
  if( is.matrix( x ) == FALSE & is.data.frame( x ) == FALSE ){ stop( "x should be a matrix or dataframe" ) }
  if( is.array( fx ) == FALSE ){ stop( "fx should be an array" ) }
  if( nrow( x ) != dim( fx )[1] ){ stop( "number of rows of x should be the same as the first dimension of fx" ) }
  if( is.matrix( ranges ) == FALSE | ncol( ranges ) != 2 | nrow( ranges ) != ncol( x ) ){ stop( "ranges should be a matrix with two columns and number of rows equal to the number of columns of x" ) }
  
  # Dimensions.
  p <- ncol ( x )
  n <- nrow( x )
  n_outputs <- dim( fx )[2]
  T <- dim( fx )[3]
  
  # Scale the input onto [-1,1] scale for emulation purposes.
  x_scaled <- QuickFunc::Scale( x, a = ranges[,1], b = ranges[,2] )
  
  # Stack fx into a matrix with n rows and n_outputs*T columns.
  fx_mat <- matrix( fx, nrow = dim( fx )[1] )
  
  # Direct emulator construction using emulate function.
  emulator <- NetworkPPBLE::emulate( x = x_scaled, # training run inputs
                                         fx = fx_mat,
                                         CF_para = list( theta = NA,
                                                         delta = 0.00001 ),
                                         CF_para_optim = NetworkPPBLE::CL_ML,
                                         CF_para_optim_para = list( x = x_scaled,
                                                                    fx = fx_mat,
                                                                    mean_function_model = 1,
                                                                    initial = rep( 0.5, p ),
                                                                    lower = rep( 0.1, p ),
                                                                    upper = rep( 20, p ),
                                                                    method = "L-BFGS-B" ) )
  
  # attach the ranges to Env emulator object and return.
  emulator$ranges <- ranges
  emulator$n_outputs <- n_outputs
  emulator$T <- T
  return( emulator )
  
}

PPE_predict <- function( emulator, x ){
  
  # Dimensions.
  n <- nrow( x )
  
  # Extract necessary objects from emulator.
  ranges <- emulator$ranges
  n_outputs <- emulator$n_outputs
  T <- emulator$T
  
  # Scale the input onto [-1,1] scale for prediction purposes.
  x_scaled <- QuickFunc::Scale( x, a = ranges[,1], b = ranges[,2] )
  
  # Predict using Direct Env Emulator.
  fx_pred_mat <- predict( emulator, x_scaled )
  
  # Convert the matrix into an array the same format as the model output arrays.
  E_fx_predict <- array( fx_pred_mat$Efx, dim = c( n, n_outputs, T ) )
  Var_fx_predict <- array( fx_pred_mat$Varfx, dim = c( n, n_outputs, T ) ) + 10^(-8)
  
  # Return the prediction objects.
  return( list( "mu" = E_fx_predict, "Varfx" = Var_fx_predict, "stdev" = sqrt( Var_fx_predict ) ) )
  
}
