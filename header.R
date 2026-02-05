library(sf)
library(dplyr)
library(readr)
library(raster)
library(bcmaps)
#library(rgdal)
library(fasterize)
library(readxl)
library(mapview)
library(WriteXLS)
library(foreign)
library(ggplot2)
library(ggspatial)
library(ggnewscale)
library(viridis)
library(stars)
#library(rgrass7)
library(exactextractr)
library(expss)
library(openxlsx)
library(cleangeo)
library(geos)
library(tidyr)
library(plyr)
library(bcdata)
library(tmap)
library(smoothr)
library(terra)
library(rmapshaper)
library(tibble)
library(stringr)
library(matrixStats)
library(purrr)
library(datawizard)
library(gtools)

options(scipen=999)
options(warn = 1)
options(timeout=10000)
control.compute=list(save.memory=TRUE)

#Install climr - climate BC R package
#install.packages("remotes") # to install climr
#Sys.unsetenv("GITHUB_PAT") # need to remove token for climr install to work
#remotes::install_github("bcgov/climr", force=TRUE)
#library(climr)

DataDir <- '../../../BuMo Analysis/BuMo_data_prep/data'
#DataDir<-'data'
dir.create(DataDir, showWarnings = FALSE)
spatialDir <- file.path(DataDir,'spatial')
dir.create(spatialDir, showWarnings = FALSE)

OutDir <- '../../../BuMo Analysis/BuMo_data_prep/out'
#OutDir<-'out'
dir.create(file.path(OutDir), showWarnings = FALSE)
dataOutDir <- file.path(OutDir,'data')
dir.create(file.path(dataOutDir), showWarnings = FALSE)
spatialOutDir <- file.path(OutDir,'spatial')
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)

ProvData<- file.path('../../../_dev/PROVData')
FireData<-file.path(ProvData,'BC_Fire')



