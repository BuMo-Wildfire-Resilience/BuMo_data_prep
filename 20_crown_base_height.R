# 20 - Crown-base height side question 

# https://www.for.gov.bc.ca/ftp/HTS/external/!publish/ground_plot_compilations/ 
  

## notes from Anya Reid (FAIB)
#You will need to look at the ‘tree detail’ spreadsheet in both the ‘psp’ and the ‘non-psp’ folder 
# (assuming you want all plot types). One thing to note is that PSPs are not measured after fire and YSMs 
# are not measured after fire until the VRI layer says that area is 15 years old.

#The column ‘HEIGHT’ = The field measured tree length (m).  
# This is the total tree length for trees without a broken top and the tree length to 
# the broken top for trees with a broken top (BROKEN_TOP_IND = Y).   
# For species that typically have an apical droop (e.g., cedar and hemlock), 
# the measurement of the length is made while standing perpendicular to the droop and 
# measured to the apex of the droop. No additional estimated length should be added to 
# account for the remainder of the tree length that extends beyond the apex of the droop.


# The column ‘HT_BRCH’ = Height to live crown (m) is the distance along the bole from the 
# high side ground level to the crown base. The crown base is normally the location on the 
# stem where live branches occupy about three-quarters of the stem circumference.  
# Measured on all live standing trees (live fallen trees do not require a height to live crown).


#All the variables for all the spreadsheets in these folders are provided in the ‘data_dictionary’ spreadsheet.




# The idea is to review the plot data and look at per species for plot data to see if there is a relationship 
# collect data per species and estimate a general rule. 
# relationship of crown base to total height 
# look at distribution and write up. 

library(fs)
library(sf)
library(dplyr)
library(lubridate)
library(terra)
library(purrr)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')
psp_data <- fs::path(spatialDir, "psp_nonpsp_plots")

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')



psp_data <- fs::path(spatialDir, "psp_nonpsp_plots", "psp")
list.files(psp_data)

HEIGHT
HT_BRCH

# df1 <- read.csv(fs::path(psp_data, "faib_compiled_smeries_ht.csv"))
# df2 <- read.csv(fs::path(psp_data, "faib_compiled_smries.csv"))
# df3 <- read.csv(fs::path(psp_data, "faib_compiled_spcsmries_siteage.csv")) # species data
# df4 <- read.csv(fs::path(psp_data, "faib_compiled_spcsmries.csv")) #
# df5 <- read.csv(fs::path(psp_data, "faib_header.csv" ))
# df6 <- read.csv(fs::path(psp_data,  "faib_plot_header.csv"  ))
# df7 <- read.csv(fs::path(psp_data,  "faib_sample_byvisit.csv"  ))
# df8 <- read.csv(fs::path(psp_data, "PSP_data_dictionary_20250514.xlsx" )) 

npsp_data <- fs::path(spatialDir, "psp_nonpsp_plots", "non_psp")
df1 <- read.csv(fs::path(npsp_data, "faib_tree_detail.csv"))
df2 <- read.csv(fs::path(npsp_data, "faib_tree_detail (1).csv"))

df1 <- df1 |> 
  select(SPECIES, HEIGHT, BROKEN_TOP_IND, HT_BRCH) |> 
  filter(BROKEN_TOP_IND == "Y") |> 
  filter(!is.na(HT_BRCH))

df2 <- df2 |> 
  select(SPECIES, HEIGHT, BROKEN_TOP_IND, HT_BRCH) |> 
  filter(BROKEN_TOP_IND == "Y") |> 
  filter(!is.na(HT_BRCH))

df1 <- bind_rows(df1, df2)


df1 <- df1 |> 
  mutate(HT_BR_RATIO = round(HEIGHT/HT_BRCH,1))

df_sum <- df1 |> 
  group_by(SPECIES) |> 
  summarise(count = n()) |> 
  filter(count > 10) |> 
  pull(SPECIES)


dfsub <- df1 |> 
  filter(SPECIES %in% df_sum)


# review the species types 
library(ggplot2)

# Look at the ratio of height vs ht Branch
ggplot(df1, aes(x = HEIGHT, y = HT_BRCH, colour = SPECIES))+
  geom_point()+
  facet_wrap(~SPECIES)


# Look at the ratio of height vs ht Branch
ggplot(dfsub, aes(x = HEIGHT, y = HT_BRCH, colour = SPECIES))+
  geom_point()+
  facet_wrap(~SPECIES, scales = "free")+
  geom_smooth(method = "lm") 



