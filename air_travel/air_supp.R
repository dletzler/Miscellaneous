#PART II: Analysis

#Load/examine overall flight statistics
setwd("~/Alternate Career/Data Science/Code Challenges/Spotify")
flights<-read.csv("total_flights.csv", stringsAsFactors = F)[1:190,]
names(flights)[1]<-"Year"
passengers<-read.csv("passengers.csv", stringsAsFactors = F)[1:190,]
names(passengers)[1]<-"Year"

#Merge and clean overall
flight.pass<-full_join(flights, passengers, by=c("Year", "Month"))
names(flight.pass)[3:8]<-c("Domestic.Flights", "International.Flights", "Total.Flights", "Domestic.Passengers", "International.Passengers", "Total.Passengers")
flight.pass[,3:8]<-data.frame(apply(flight.pass[,3:8], 2, function(y) as.numeric(gsub(",", "", y))))
flight.pass$Year<-as.numeric(flight.pass$Year)

#The following reveals the discrepancy between the overall stats and the Thanksgiving stats and the need to emphasize major carriers, only
comp<-full_join(nov.num2, filter(flight.pass, Month=="11"), by="Year") %>% select(nov.num, Domestic.Flights) %>% mutate(Year = c(2000:2016, 1987:1999), Ratio = nov.num/Domestic.Flights)
plot(x=comp$Year, y=comp$Ratio, pch=19, title="")

#MAJOR AIRLINE DATA
#Number of Flights
setwd("~/Alternate Career/Data Science/Code Challenges/Spotify/flights")
airline.fl<-lapply(dir(), read.csv, stringsAsFactors = F)

#The following was to deal with initial, uncorrectable errors in the CSVs due to my mishanalding, including
#removing extra rows, commas, etc. It is not necessary for correctly handled data.
remove.rows<-function(df){
  df2<-df[1:(nrow(df)-4),]
  return(df2)
}

airline.fl2<-lapply(airline.fl, remove.rows)
airline.fl2[[25]]<-gsub(",", "", data.frame(airline.fl2[[25]])$airline.fl2..25..)
airline.fl2[[13]]<-airline.fl[[13]]

airline.fl3[[13]]<-data.frame(apply(airline.fl2[[13]], 2, function(y) as.numeric(gsub(",", "", y))))[1:121,]
airline.fl3[[13]]$Month[is.na(airline.fl3[[13]]$Month)]<-"TOTAL"

airline.fl3[[25]]<-separate(data.frame(airline.fl2[[25]]), "airline.fl2..25..", c("Year", "Month", "DOMESTIC", "INTERNATIONAL", "TOTAL", sep=" "))[,-6]

#This part is necessary--it associates airline names to each csv
names(airline.fl)<-c("airtran", "alaska", "aloha", "america west", "american", "ata", "comair", "continental", "delta", "endeavor", "envoy",
                     "express jet", "expressj", "frontier", "hawaiian", "independence", "jetblue", "mesa", "northwest", "skywest", "southwest",
                     "spirit", "united", "usair", "virgin")

for (i in 1:length(airline.fl)){
  airline.fl[[i]]$Airline<-names(airline.fl)[i]
#  write.csv(airline.fl[[i]], paste0(names(airline.fl[i]), ".csv"), row.names=F)
}

#Finally, we can create a merged dataframe of all airline monthly flight information
airline.flight<-do.call(rbind, airline.fl)

#The Passenger Data
setwd("~/Alternate Career/Data Science/Code Challenges/Spotify/passengers")
#The following is based on correct data--what comes afterward is similar correction of my mistakes.
airline.p<-lapply(dir(), read.csv, stringsAsFactors=F)
#Knock out weird extra column
for (i in 1:25){
  airline.p[[i]]<-airline.p[[i]][,-6]
}

#Corrections
airline.p1[[1]]<-apply(data.frame(airline.p1[[1]]), 2, function(y) gsub(",", "", y))

for (i in 2:6){
  airline.p1[[i]]$Year.Month.DOMESTIC.INTERNATIONAL.TOTAL<-gsub("\.00", "", airline.p1[[i]]$Year.Month.DOMESTIC.INTERNATIONAL.TOTAL)  
}

airline.p2<-list()
for (i in 1:length(airline.p1)){
  airline.p2[[i]]<-separate(data.frame(airline.p1[[i]]), "Year.Month.DOMESTIC.INTERNATIONAL.TOTAL", c("Year", "Month", "DOMESTIC", "INTERNATIONAL", "TOTAL", sep=" "))[,-6]
}

airline.p2[[10]]<-airline.p2[[10]][1:190,]

#As before, this associates the airlines with the dataframe components
names(airline.p)<-c("airtran", "alaska", "aloha", "america west", "american", "ata", "comair", "continental", "delta", "endeavor", "envoy",
                    "express jet", "expressj","frontier", "hawaiian", "independence", "jetblue", "mesa", "northwest", "skywest", "southwest",
                    "spirit", "united", "usair", "virgin")

for (i in 1:length(airline.p)){
  airline.p[[i]]$Airline<-names(airline.p)[i]
  #write.csv(airline.p[[i]], paste0(names(airline.p[i]), ".csv"), row.names=F)
}

#Again, finally, we create a single dataframe
names(airline.p[[4]])[1]<-"Year"
airline.pass1<-do.call(rbind, airline.p)

#More corrections
#airline.flight$Airline[airline.flight$Airline=="americawest"]<-"america west"
#airline.flight$Airline[airline.flight$Airline=="expressjet"]<-"express jet"

#Finally, we can combine these sets into one and determine passengers/flight
airline.flight$Year<-as.numeric(airline.flight$Year)
airline.all2<-full_join(airline.flight, airline.pass1, by=c("Airline", "Year", "Month"))
names(airline.all2)[c(3:5, 7:9)]<-c("Domestic.Flights", "International.Flights", "Total.Flights", "Domestic.Pass", "International.Pass", "Total.Pass")
airline.all2<-airline.all2 %>% mutate(Pass.Flight = as.numeric(Domestic.Pass)/as.numeric(Domestic.Flights))

airline.all2$Month<-factor(airline.all2$Month)
levels(airline.all2$Month)<- c("Jan", "Oct", "Nov", "Dec", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "TOTAL")
airline.all2$Month<-factor(airline.all2$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "TOTAL"))


