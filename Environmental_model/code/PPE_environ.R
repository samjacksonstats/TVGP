### Environmental Model - PPE ###

## Build emulators using designs of size 20 and 50. ##
env_emulator20 <- PPE_build( x = x20, fx = lfx20, ranges = ranges )
env_emulator50 <- PPE_build( x = x50, fx = lfx50, ranges = ranges )

## Make predictions at test points. ##
PPE_env_prediction20_log <- PPE_predict( emulator = env_emulator20, x = x150 )
PPE_env_prediction50_log <- PPE_predict( emulator = env_emulator50, x = x150 )


