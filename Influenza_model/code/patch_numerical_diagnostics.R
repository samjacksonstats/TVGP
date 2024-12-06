### Numerical Diagnostics ###

## Obtaining Diagnostics ##

# predictions_list <- list( OPE_patch_prediction20_log, PPE_patch_prediction20_log, OPE_patch_prediction50_log, PPE_patch_prediction50_log )
predictions_list <- list( OPE_patch_prediction20_log, PPE_patch_prediction20_log, OPE_patch_prediction50_log, PPE_patch_prediction50_log )

MASPE <- RMSPE <- MGES <- rep( NA, length( predictions_list ) )               

for( j in 1:length( predictions_list ) ){
  
  MASPE[j] <- mean( abs( lfx150 - predictions_list[[j]]$mu ) / predictions_list[[j]]$stdev  )
  RMSPE[j] <- sqrt( mean( ( lfx150 - predictions_list[[j]]$mu )^2 ) )
  MGES[j] <- - sum( ( lfx150 - predictions_list[[j]]$mu )^2 / predictions_list[[j]]$stdev^2 + log( predictions_list[[j]]$stdev^2 )  ) 
  
}


# Create the data frame as before
results_table <- data.frame(
 `OPE 20` = c(MASPE[1], RMSPE[1], MGES[1]),
 `PPE 20` = c(MASPE[2], RMSPE[2], MGES[2]),
 `OPE 50` = c(MASPE[3], RMSPE[3], MGES[3]),
 `PPE 50` = c(MASPE[4], RMSPE[4], MGES[4])
)

rownames(results_table) <- c("MASPE", "RMSPE", "MGES")
cat("Numerical diagnostics results\n")
print(results_table)
