# Set the wd to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# If you are not using RStudio you will have to manually set your wd
# setwd("~set_your_own_path/Environmental model")

### Run ###

# For installing R packages from github...
library( devtools )

# Install libraries.
install_github( "samjacksonstats/NetworkPPBLE/QuickFunc" )
install_github( "samjacksonstats/NetworkPPBLE/NetworkPPBLE" )

# Libraries.
library( QuickFunc )
library( NetworkPPBLE )

## Generate designs of size 20, 50 and 150, or load in designs from paper. 
# source( "max pro design.R" )
load("code/article_env_design.rdata" )

## Run the environmental model at designs x20, x50 and x150. ##
source("code/run environmental model.R" )

## Outer Product Emulation (OPE) ##
source("code/OPE environ.R" )

## Parallel Partial Emulation ##
source("code/PPE environ.R" )

## Numerical Diagnostics ##
source("code/environmental numerical diagnostics.R" )

## Visual Diagnostics ##
## This script will produce for one randomly selected diagnostic point 1) a number of plots that could be viewed on the 'Plots' tab in RStudio and 
## 2) a number of interactive 3D plots that could be viewed on the 'Viewer' tab.
source("code/environmental visual diagnostics.R" )
