 # Copyright 2021 Province of British Columbia
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

HistoricFire<-st_read(file.path(spatialOutDir,'HistoricFire.gpkg')) 

#Evaluate fire history in study area
hist(as.numeric(HistoricFire$FIRE_YEAR))
plot(HistoricFire$FIRE_YEAR,HistoricFire$FIRE_SIZE_HECTARES,type="h")

#Make some breaks based on jenks...
#Calculate Jenks breaks and add to data.frame
#Min-Max normalization function
min_max_norm <- function(x) {
  #if_else((max(x) == min(x)),1,0)
  (x - min(x)) / (max(x) - min(x))
}
#apply Min-Max normalization
FireNorm <- as.data.frame(lapply(wespRaw[2:nFB], min_max_norm)) %>%
  mutate(site=as.numeric(rownames(.)), .before=1) %>%
  replace(is.na(.),1)
HistoricFire$YearNorm<-min_max_norm (HistoricFire$FIRE_YEAR)
  
#Do Jenks breaks using normalized scores
# use BAMMtools' getJenksBreaks function
library('BAMMtools')
jen_breaks<-getJenksBreaks(HistoricFire$YearNorm, 3)
HistoricFire$FireYrBrks<-.bincode(HistoricFire$YearNorm, jen_breaks,include.lowest=TRUE)

#Change numeric to character High, Medium, Low
HistoricFire.1<-HistoricFire %>%
  mutate(FireYrBrksC=case_when(
  FireYrBrks ==1 ~ 'E PDO H',
  FireYrBrks ==2  ~ 'PDO L',
  FireYrBrks ==3  ~ 'L PDO H'
)) %>%
  dplyr::select(id,FIRE_NUMBER,FIRE_YEAR,FIRE_CAUSE,FIRE_LABEL, FIRE_SIZE_HECTARES,SOURCE,YearNorm,FireYrBrks,FireYrBrksC)

  mapview(HistoricFire.1,zcol='FireYrBrksC')

#PDO Phases 
# Morgan, 2011: warm (45 years; 1925-1946, 1977-1998) and 
# cool (37 years; 1919-1924, 1947-1976, 1999-2000) 
# PDO phases (University of Washington 2010)
HistoricFire.2<-HistoricFire.1 %>%
  mutate(FireYrBrksPDO=case_when(
  FIRE_YEAR>0 &  FIRE_YEAR<1925 ~ 'PDO cool', #1922 lots of fires?
  FIRE_YEAR>=1925 & FIRE_YEAR<1947 ~ 'PDO warm',
  FIRE_YEAR>=1947 & FIRE_YEAR<1977 ~ 'PDO cool',
  FIRE_YEAR>=1977 & FIRE_YEAR<1999 ~ 'PDO warm',
  FIRE_YEAR>=1999 & FIRE_YEAR<2003 ~ 'PDO cool',
  FIRE_YEAR>=2003 & FIRE_YEAR<2007 ~ 'PDO warm',
  FIRE_YEAR>=2007 & FIRE_YEAR<2014 ~ 'PDO cool',
  FIRE_YEAR>=2014 ~ 'PDO warm'
)) %>%
  dplyr::select(id,FIRE_NUMBER,FIRE_YEAR,FIRE_CAUSE,FIRE_LABEL, FIRE_SIZE_HECTARES,SOURCE,
                YearNorm,FireYrBrks,FireYrBrksC,FireYrBrksPDO)

  mapview(HistoricFire.2,zcol='FireYrBrksPDO') +
  mapview(BEC_BuMo,zcol='ZONE')

#https://sealevel.jpl.nasa.gov/data/el-nino-la-nina-watch-and-pdo/pacific-decadal-oscillation-pdo/
#Vance, Tessa R., Anthony S. Kiem, Lenneke M. Jong, Jason L. Roberts, Christopher T. Plummer, Andrew D. Moy, Mark A. J. Curran, and Tas D. van Ommen. “Pacific Decadal Variability over the Last 2000 Years and Implications for Climatic Risk.” Communications Earth & Environment 3, no. 1 (February 17, 2022): 33. https://doi.org/10.1038/s43247-022-00359-z.
#https://iri.columbia.edu/our-expertise/climate/forecasts/enso/current/
HistoricFire.2<-HistoricFire.1 %>%
  mutate(FireYrBrksPDO=case_when(
  FIRE_YEAR>0 &  FIRE_YEAR<1925 ~ 'PDO cool',
  FIRE_YEAR>=1925 & FIRE_YEAR<1947 ~ 'PDO warm',
  FIRE_YEAR>=1947 & FIRE_YEAR<1977 ~ 'PDO cool',
  FIRE_YEAR>=1977 & FIRE_YEAR<1999 ~ 'PDO warm',
  FIRE_YEAR>=1999 & FIRE_YEAR<2003 ~ 'PDO cool',
  FIRE_YEAR>=2003 & FIRE_YEAR<2007 ~ 'PDO warm',
  FIRE_YEAR>=2007 & FIRE_YEAR<2014 ~ 'PDO cool',
  FIRE_YEAR>=2014 ~ 'PDO warm'
)) %>%
  dplyr::select(id,FIRE_NUMBER,FIRE_YEAR,FIRE_CAUSE,FIRE_LABEL, FIRE_SIZE_HECTARES,SOURCE,
                YearNorm,FireYrBrks,FireYrBrksC,FireYrBrksPDO)

  mapview(HistoricFire.2,zcol='FireYrBrksPDO') +
  mapview(BEC_BuMo,zcol='ZONE')

#Wang, S., Huang, J., He, Y. et al. Combined effects of the Pacific Decadal Oscillation and El Niño-Southern Oscillation on Global Land Dry–Wet Changes. Sci Rep 4, 6651 (2014). https://doi.org/10.1038/srep06651https://www.nature.com/articles/srep06651 
#Warm PDO - El Niño
WrmPDO_ElNino<-c(1902,1904,1905,1911,1913,1923, 1925,1930,1939,1940,1941,1982, 1986,1987,1991,1994,1997,2002, 2014,2015,2016,2018,2019,2023,2024)
#Cold PDO - El Niño
ColdPDO_ElNino<-c(1914,1918,1957,1963,1965,1968, 1972,1976,1977,2004,2006,2009)
#Warm PDO - La Niña
WrmPDO_LaNina<-c(1903,1908,1909,1910,1922,1924, 1933,1938,1942,1983,1984,1988, 1998,1999,2000,2017,2020,2021,2022)
#Cold PDO - La Niña
ColdPDO_LaNina<-c(1916,1917,1949,1950,1954,1955, 1964,1970,1973,1975,2005,2007, 2008,2010,2011,2012)

 HistoricFire.3<-HistoricFire.2 %>%
  mutate(FireYrBrksPDO_ENSO=case_when(
  FIRE_YEAR %in%  WrmPDO_ElNino ~ 'WrmPDO_ElNino',
  FIRE_YEAR %in%  ColdPDO_ElNino ~ 'CoolPDO_ElNino',
  FIRE_YEAR %in%  WrmPDO_LaNina ~ 'WrmPDO_LaNina',
  FIRE_YEAR %in%  ColdPDO_LaNina ~ 'CoolPDO_LaNina',
  FireYrBrksPDO=='PDO warm' & !(FIRE_YEAR %in%  c(WrmPDO_ElNino,ColdPDO_ElNino,WrmPDO_LaNina,ColdPDO_LaNina)) ~ 'WrmPDO',
  FireYrBrksPDO=='PDO cool' & !(FIRE_YEAR %in%  c(WrmPDO_ElNino,ColdPDO_ElNino,WrmPDO_LaNina,ColdPDO_LaNina)) ~ 'CoolPDO',
  .default = "neutral"
)) %>%
  dplyr::select(id,FIRE_NUMBER,FIRE_YEAR,FIRE_CAUSE,FIRE_LABEL, FIRE_SIZE_HECTARES,SOURCE,
                YearNorm,FireYrBrks,FireYrBrksC,FireYrBrksPDO,FireYrBrksPDO_ENSO)

  mapview(HistoricFire.3,zcol='FireYrBrksPDO_ENSO') 
  
  table(HistoricFire.3$FireYrBrksPDO_ENSO)
  table(HistoricFire.3$FireYrBrksPDO)
  plot(HistoricFire.3$FIRE_YEAR,HistoricFire.3$FIRE_SIZE_HECTARES)
  #plot(HistoricFire.3$FireYrBrksPDO,HistoricFire.3$FIRE_SIZE_HECTARES)

write_sf(HistoricFire.3,file.path(spatialOutDir,'HistoricFire.3.gpkg')  )
 
HistoricFire.4<-HistoricFire.3 %>%
   st_drop_geometry() %>%
   group_by(FireYrBrksPDO_ENSO) %>%
   dplyr::summarise(AreaBurned=sum(FIRE_SIZE_HECTARES))
  
HistoricFire.5<-HistoricFire.3 %>%
   st_drop_geometry() %>%
   group_by(FireYrBrksPDO) %>%
   dplyr::summarise(AreaBurned=sum(FIRE_SIZE_HECTARES))
  

