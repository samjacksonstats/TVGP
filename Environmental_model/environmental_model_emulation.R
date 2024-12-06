## We suggest using an RStudio project and the "here" package to manage paths
library("here")
### Run ###

## Libraries
library("devtools")
library("rTensor")
library("ggplot2")
library("ggpubr")
library("plotly")
library("MaxPro") # only needed if designs will be generated

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
source(here("Environmental_model", "code", "environmental_model_function.R")) # Environmental function
source(here("code", "ML_OPE.R")) #Maximum Likelihood for OPE
source(here("code", "OPE_3D_extension.R")) #Source 3D OPE extension
source(here("code", "PPE_Functions.R"))

## Generate designs of size 20, 50 and 150, or load in designs from paper. 
# source(here("Environmental_model", "code", "max_pro_design.R"))
load(here("Environmental_model", "data", "article_env_design.rdata"))

## Run the environmental model at designs x20, x50 and x150. ##
source(here("Environmental_model", "code", "run_environmental_model.R"))

## Outer Product Emulation (OPE) ##
source(here("Environmental_model", "code", "OPE_environ.R"))

## Parallel Partial Emulation ##
source(here("Environmental_model", "code", "PPE_environ.R"))

## Numerical Diagnostics ##
source(here("Environmental_model", "code", "environmental_numerical_diagnostics.R"))

## Visual Diagnostics ##
## This script will produce for one randomly selected diagnostic point 1) a number of plots that could be viewed on the 'Plots' tab in RStudio and 
## 2) a number of interactive 3D plots that could be viewed on the 'Viewer' tab.
source(here("Environmental_model", "code", "environmental_visual_diagnostics.R")) 
