# Set the wd to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# If you are not using RStudio you will have to manually set your wd
# setwd("~set_your_own_path/Influenza model")

### Run ###

# For installing R packages from github...
library( devtools )

# Install libraries.
install_github( "samjacksonstats/NetworkPPBLE/QuickFunc" )
install_github( "samjacksonstats/NetworkPPBLE/NetworkPPBLE" )

# Libraries.
library( QuickFunc )
library( NetworkPPBLE )

## Load model runs utilised in article. 
load("code/patch model data.rdata" )

## Outer Product Emulation (OPE) ##
source("code/OPE patch.R" )

## Parallel Partial Emulation ##
source("code/PPE patch.R" )

## Numerical Diagnostics ##
source("code/patch numerical diagnostics.R" )

## Visual Diagnostics ##
## This script will produce for one randomly selected diagnostic point a number of plots that could be viewed on the 'Plots' tab in RStudio and 
source("code/patch visual diagnostics.R" )
