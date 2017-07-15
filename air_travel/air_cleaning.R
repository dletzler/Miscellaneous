#PART I: Load and Prepare Data
setwd("~/Alternate Career/Data Science/Code Challenges/Spotify")
#load("~/Alternate Career/Data Science/Code Challenges/Spotify/iday.RData")
library(dplyr)
library(lubridate)

#Set up supplemental files--airport/airline lookup tables, dates of Thanksgiving

airports<-read.csv("airports.csv", stringsAsFactors = F)[-8]
airports2<-airports %>% group_by(AIRPORT_ID) %>% filter(AIRPORT_SEQ_ID==max(AIRPORT_SEQ_ID))


thanks<-read.csv("thanks_date.csv", stringsAsFactors = F)
names(thanks)<-c("Year", "Date")
thanks$Date<-as.Date(thanks$Date, format="%m/%d/%Y")
airline<-read.csv("airline_id.csv", stringsAsFactor=F)

#Set up ETL functions

#Extract Tues-Sun of Thanksgiving week
trim.tday<-function(df){
  year<-year(min(df$FL_DATE))
  tday<-filter(thanks, Year==year)$Date
  range<-filter(df, FL_DATE <= tday+3 & FL_DATE >= tday-2)
  return(range)
}

#Transform airline ID to airline name
airline.id<-function(df){
  airline.df<-left_join(df, airline, by=c("AIRLINE_ID"="Code"))
  airline.final<-select(airline.df, -AIRLINE_ID)
  return(airline.final)
}

#Convert all timestamps to 4-digits
fix.time1<-function(vec){
  vec<-as.character(vec)
  vec[nchar(vec)==3]<-paste0("0", vec[nchar(vec)==3])
  vec[nchar(vec)==2]<-paste0("00", vec[nchar(vec)==2])  
  vec[nchar(vec)==1]<-paste0("000", vec[nchar(vec)==1])
  vec[nchar(vec)==0]<-"0000"

  return(vec)
}

#Convert 4-digit number to proper timestamp
fix.time2<-function(x){
  x<-paste(substr(x, 1,2), substr(x, 3,4), "00", sep=":")
  return(x)
}

#Merge table with airport coordinates
add.coord<-function(df){
  origin.df<-df %>% left_join(airports2, by=c("ORIGIN_AIRPORT_ID"="AIRPORT_ID"))  %>% 
                    select(-AIRPORT_SEQ_ID, -AIRPORT)
  both.df<-origin.df %>% left_join(airports2, by=c("DEST_AIRPORT_ID"="AIRPORT_ID"))  %>% 
          select(-AIRPORT_SEQ_ID, -AIRPORT)
  names(both.df)[15:22]<-c("ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_LAT", "ORIGIN_LONG", "DEST_AIRPORT", "DEST_CITY", "DEST_LAT", "DEST_LONG")
  origin<-strsplit(both.df$ORIGIN_CITY, split=", ")
  origin.df<-data.frame(do.call(rbind, origin))
  names(origin.df)<-c("ORIGIN_CT", "ORIGIN_ST")
  dest<-strsplit(both.df$DEST_CITY, split=", ")
  dest.df<-data.frame(do.call(rbind, dest))
  names(dest.df)<-c("DEST_CT", "DEST_ST")
  final.df<-cbind(both.df, origin.df, dest.df) %>% select (-ORIGIN_CITY, -DEST_CITY)
  return(final.df)
}

library(chron)

#Meta-cleaning function to combine all functions in sequence and provide additional small-scale cleaning
clean.flight<-function(file){
  df<-read.csv(file, stringsAsFactors = F)[-14]
  df$FL_DATE<-as.Date(df$FL_DATE)
  df$WEEKDAY<-weekdays(df$FL_DATE)
  print(paste("There were", nrow(df), "flights for November", year(min(df$FL_DATE))))
  trim<-trim.tday(df)
  print(paste("There were", nrow(trim), "flights for Thanksgiving", year(min(df$FL_DATE))))
  air.code<-airline.id(trim)
  air.code$CRS_ARR_TIME<-fix.time2(fix.time1(air.code$CRS_ARR_TIME))
  air.code$CRS_DEP_TIME<-fix.time2(fix.time1(air.code$CRS_DEP_TIME))
  flight.transform<-add.coord(air.code)
  return(flight.transform)
}

#Secondary cleaning function for different date formats
clean.flight2<-function(file){
  df<-read.csv(file, stringsAsFactors = F)[-14]
  df$FL_DATE<-as.Date(df$FL_DATE, format="%m/%d/%Y")
  df$WEEKDAY<-weekdays(df$FL_DATE)
  print(paste("There were", nrow(df), "flights for November", year(min(df$FL_DATE))))
  trim<-trim.tday(df)
  print(paste("There were", nrow(trim), "flights for Thanksgiving", year(min(df$FL_DATE))))
  air.code<-airline.id(trim)
  air.code$CRS_ARR_TIME<-fix.time1(air.code$CRS_ARR_TIME)
  air.code$CRS_DEP_TIME<-fix.time1(air.code$CRS_DEP_TIME)
  air.code$CRS_ARR_TIME<-fix.time2(fix.time1(air.code$CRS_ARR_TIME))
  air.code$CRS_DEP_TIME<-fix.time2(fix.time1(air.code$CRS_DEP_TIME))
  flight.transform<-add.coord(air.code)
  return(flight.transform)
}

#Load CSVs, clean, combine, and save
setwd("~/Alternate Career/Data Science/Code Challenges/Spotify/november2")
tday.flight<-lapply(dir()[1:12],clean.flight)
tday.sup<-lapply(dir()[13:14],clean.flight2)
tday.flight[13:14]<-tday.sup
#names(tday.flight[[17]])[c(6,8)]<-c("DEP_DELAY", "ARR_DELAY")
tday.combined<-do.call(rbind, tday.flight)
rm(tday.flight)

#Get set sizes for comparison to macro-data from DoT
check.size<-function(file){
  df<-read.csv(file, stringsAsFactors = F)
  size<-nrow(df)
  return(size)
}

setwd("~/Alternate Career/Data Science/Code Challenges/Spotify/november")
nov.num<-sapply(dir(),check.size)
nov.num2<-data.frame(c(2000:2016, 1987:1999), nov.num)
names(nov.num2)[1]<-"Year"

#Focus solely on 2003-on
tday.03.16<-tday.combined %>% filter(year(FL_DATE)>= 2003)

setwd("~/Alternate Career/Data Science/Code Challenges/Spotify")
save(tday.03.16, file="tday.RData")








