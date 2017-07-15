library(leaflet)
library(dplyr)
library(DT)

tday.03.16<-readRDS("tday.rds") %>% 
  select(Description, FL_NUM, FL_DATE, Year, WEEKDAY, CRS_DEP_TIME, CRS_ARR_TIME, ORIGIN_AIRPORT, DEST_AIRPORT, DEST_LAT, DEST_LONG, Pass.Flight)
