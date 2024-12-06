## We suggest using an RStudio project and the "here" package to manage paths
library("here")

## Libraries
library("devtools")
library("rTensor")
library("ggplot2")
library("ggpubr")

## Install libraries from Github
install_github( "samjacksonstats/NetworkPPBLE/QuickFunc" )
install_github( "samjacksonstats/NetworkPPBLE/NetworkPPBLE" )

## Install original 2D OPE package from local source
install.packages(here("OPE_0.7.tar.gz"), repos = NULL, type="source")

## Libraries.
library("QuickFunc")
library("NetworkPPBLE")
library("OPE")

## Source functions
source(here("code", "ML_OPE.R")) # Maximum Likelihood for OPE
source(here("code", "OPE_3D_extension.R")) # Source 3D OPE extension
source(here("code", "PPE_functions.R")) # PPE

## Load model runs used in the article. 
load(here("influenza_model", "data", "patch_model_data.rdata" ))

## Outer Product Emulation (OPE) ##
source(here("influenza_model", "code", "OPE_patch.R"))

## Parallel Partial Emulation ##
source(here("influenza_model", "code", "PPE_patch.R"))

## Numerical Diagnostics ##
source(here("influenza_model", "code", "patch_numerical_diagnostics.R"))

## Visual Diagnostics ##
## This script will produce, for one randomly selected diagnostic point, a number of plots that could be viewed on the 'Plots' tab in RStudio 
source(here("influenza_model", "code", "patch_visual_diagnostics.R"))
