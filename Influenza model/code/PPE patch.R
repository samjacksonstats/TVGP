### Flu Emulation For Article ###

## Source Emulator Function Files. ##
source("code/PPE Functions.R" )

## Build emulators using designs of size 20 and 50. ##
patch_emulator20 <- PPE_build( x = x20, fx = lfx20, ranges = ranges )
patch_emulator50 <- PPE_build( x = x50, fx = lfx50, ranges = ranges )

## Make predictions at test points. ##
PPE_patch_prediction20_log <- PPE_predict( emulator = patch_emulator20, x = x150 )
PPE_patch_prediction50_log <- PPE_predict( emulator = patch_emulator50, x = x150 )

