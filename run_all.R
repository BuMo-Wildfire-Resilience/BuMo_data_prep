# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

options(timeout=180)
source("header.R")

#Base load
spatialOutDir <- file.path('out','spatial')
spatialInDir <- file.path(spatialOutDir)
dataOutDir <- file.path(OutDir,'data')
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
tempAOIDir<-paste0("tmp/")
dir.create(tempAOIDir, showWarnings = FALSE)

#Build AOI
source('01_AOI.R')
#Data load
source('01_load.R')
