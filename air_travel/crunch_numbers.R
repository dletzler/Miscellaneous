library(ggplot2)
library(ggthemes)
library(scales)


#EDA on the new information
totals<-filter(airline.all2, Month=="TOTAL") %>% group_by(Year) %>% summarize(Total.Flights = sum(as.numeric(Domestic.Flights)), Total.Pass = sum(as.numeric(Domestic.Pass))) %>% mutate(Pass.Per = Total.Pass/Total.Flights)
air.totals<-filter(airline.all2, Month=="TOTAL") 
nov.totals<-airline.all2 %>% filter(Month=="Nov") %>% group_by(Year) %>%
  summarize(Total.Flights = sum(as.numeric(Domestic.Flights)), Total.Pass = sum(as.numeric(Domestic.Pass))) %>% mutate(Pass.Per = Total.Pass/Total.Flights)
month.totals<-filter(airline.all2, Month!="TOTAL") %>% group_by(Year, Month) %>% summarize(Passengers = sum(as.numeric(Domestic.Pass)))
months<-airline.all2 %>% filter(Month!="TOTAL") %>% group_by(Month) %>% summarize(Total.Pass = sum(as.numeric(Domestic.Pass)), Total.Flights = sum(as.numeric(Domestic.Flights)))
month.year<-airline.all2 %>% group_by(Year, Month) %>% summarize(Total.Flights=sum(as.numeric(Domestic.Flights)), Total.Pass = sum(as.numeric(Domestic.Pass)))


pass.per.both<-(full_join(pass.per, totals, by="Year") %>% select(Year, Pass.Per.Flight, Pass.Per))[1:15,]
names(pass.per.both)[2:3]<-c("All.Domestic", "Major.Carrier")
library(tidyr)
pass.per.tidy<-cbind(pass.per.both$Year, gather(pass.per.both[,2:3], key="Type", value="Passengers.Per.Flight"))
names(pass.per.tidy)[1]<-'Year'

comp3<-full_join(wed, filter(month.totals, Month=="Jul"), by="Year")
comp3$Passengers.y<-comp3$Passengers.y/31

mean(comp3$Passengers.x[8:13])
mean(comp3$Passengers.y[8:13])

p<-ggplot(pass.per.tidy[], aes(x=Year, y=Passengers.Per.Flight))
p + geom_bar(stat="identity", aes(fill=Type), position="dodge") + labs(title="Airlines Are Packing More Passengers Per Flight", y="Passengers Per Flight") + 
    guides(fill=guide_legend(title="Flights")) + scale_fill_manual(labels=c("All Domestic", "Major Carrier Only"), values = c("skyblue", "red")) + theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

#Basic per-year EDA as well as Wed./Sun. EDA on Thanksgiving set
time<-tday.03.16 %>% mutate(Year = year(FL_DATE)) %>% group_by(Year) %>% summarize(Total.Flights = n()/1000)
wed<-tday.03.16 %>% filter(WEEKDAY=="Wednesday") %>% mutate(Year = year(FL_DATE)) %>% group_by(Year) %>% summarize(Passengers = sum(Pass.Flight))
#sun<-tday.02.16 %>% filter(WEEKDAY=="Sundday") %>% mutate(Year = year(FL_DATE)) %>% group_by(Year) %>% summarize(Total.Flights = n()/1000)



#IMPORTANT--Reconciliation between two datasets
comp2<-left_join(nov.totals, nov.num2, by="Year") %>% mutate(Ratio = nov.num/Total.Flights)

#Which airlines still run?
airline<-tday.02.16 %>% mutate(Year = year(FL_DATE)) %>% group_by(Year, Description) %>% summarize(Count=n()) %>% group_by(Year) %>% summarize(Airlines = n())
airline2<-tday.02.16 %>% mutate(Year = year(FL_DATE)) %>% group_by(Year, Description) %>% summarize(Count=n()) %>% group_by(Description) %>% summarize(Years=n())

airport.airline<- tday.02.16 %>% group_by(DEST_AIRPORT, Description) %>% summarize(Total = n()) %>% group_by(DEST_AIRPORT) %>% filter(Total==max(Total))

tday.02.16$DEST_LONG<-round(tday.02.16$DEST_LONG, 1)
tday.02.16$DEST_LAT<-round(tday.02.16$DEST_LAT, 1)

airport1<-tday.02.16 %>% filter(WEEKDAY %in% c("Tuesday", "Wednesday", "Thursday")) %>% group_by(DEST_AIRPORT, DEST_LONG, DEST_LAT) %>% summarize(Count=sum(Pass.Flight.x))
airport2<-tday.02.16 %>% filter(WEEKDAY %in% c("Friday", "Saturday", "Sunday")) %>% group_by(DEST_AIRPORT, DEST_LONG, DEST_LAT) %>% summarize(Count=sum(Pass.Flight.x))

airport.comp<-full_join(airport1, airport2, by=c("DEST_AIRPORT", "DEST_LONG", "DEST_LAT")) %>% filter(Count.y > 10000)  %>% mutate(Difference = (Count.x-Count.y)/Count.y)


#Make translation
air.code2<-data.frame(unique(airline.all2$Airline)[1:25][order(unique(airline.all2$Airline)[1:25])], unique(tday.03.16$Description)[order(unique(tday.03.16$Description))])
names(air.code2)<-c("Airline", "Description")
airline.all2<-left_join(airline.all2, air.code2, by="Airline")

airline.merge<-airline.all2 %>% filter(Month=="Nov") %>% select(Year, Description, Pass.Flight)
tday.03.16<-tday.03.16 %>% mutate(Year = year(FL_DATE))
tday.03.16<-left_join(tday.03.16, airline.merge, by=c("Year", "Description"))  

t.pass2<-tday.03.16 %>% group_by(Year) %>% summarize(Passengers = sum(Pass.Flight), Flights=n())

t<-ggplot(data=t.pass2, aes(x=Year, y=Passengers/1000000))
t + geom_line(aes(y=Flights/10000, color="Domestic Flights")) + geom_line(aes(color="Total Passengers")) + 
  labs(title="Thanksgiving Week Travel (Major Carriers, 2003-2016)", y="Domestic Flights (10,000s) & Passengers(1,000,000s)") + 
 scale_color_manual(name="", values=c("Domestic Flights"="red", "Total Passengers"="blue")) + theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  

#THE BIG QUESTIONS
#Is Wednesday the nightmare everyone thinks it is?
tday.03.16$WEEKDAY<-factor(tday.03.16$WEEKDAY, levels=c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

w<-ggplot(data=tday.03.16, aes(x=WEEKDAY))
w + geom_histogram(stat="count", aes(fill=WEEKDAY)) + labs(title="Major Carrier Flights During Thanksgiving Week (2003-2016)", x="", y="Number of Flights") +
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette = "OrRd") + guides(fill=F) + theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic()

tday.02.16 %>% filter(Year==2012, WEEKDAY=="Wednesday") %>% summarize(Passengers = sum(Pass.Flight))
#1,765,125 Wed. travelers in 2012, versus 1,736,665 on an average day in July 2012.

#PART III: VISUAL EDA


ggplot(totals[1:15,], aes(x=Year, y=Total.Flights/1000000)) + geom_line(aes(color="Domestic Flights")) + geom_line(aes(y=Total.Pass/100000000, color="Total Passengers")) +
  labs(title="Total Maj. Carrier Domestic Flights & Passengers (USA)", x="Year", y="Flights (1,000,000s) & Passengers(100,000,000s)") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(name="", values=c("Domestic Flights"="red", "Total Passengers"="blue"))


ggplot(data=t.pass2, aes(x=Year, y=Flights/1e3)) + geom_line(aes(color="Domestic Flights")) + geom_line(aes(y=Passengers/1e5, color="Total Passengers")) +
  labs(title="Maj. Carrier Thanksgiving Flights & Est. Passengers (Tues.-Sun.)", x="Year", y="Flights (1,000s) & Passengers (100,000s)") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(name="", values=c("Domestic Flights"="red", "Total Passengers"="blue"))

m<-ggplot(data=months, aes(x=Month, y=Total.Flights/1e6))
zoom <- coord_cartesian(ylim = c(7, 9))
m + geom_bar(stat="identity", aes(fill=Month)) + geom_hline(yintercept=mean(months$Total.Flights)/1e6) +
  labs(title="Domestic Flights by Month (2002-2016)", y="Total Flights(1,000,000s)") + scale_fill_brewer(palette = "Paired") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F) + zoom


p<-ggplot(data=months, aes(x=Month, y=Total.Pass/1e8))
zoom2 <- coord_cartesian(ylim = c(6, 8))
p + geom_bar(stat="identity", aes(fill=Month)) + geom_hline(yintercept=mean(months$Total.Pass)/1e8) +
  labs(title="Total Domestic Passengers by Month (Major Carriers, 2003-2016)", y="Total Passengers(100,000,000s)") + scale_fill_brewer(palette = "Paired") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F) + zoom2

per<-ggplot(data=monthly, aes(x=Month, y=Pass.Per.Flight))
zoom3 <- coord_cartesian(ylim = c(65, 80))
per + geom_bar(stat="identity", aes(fill=Month)) + geom_hline(yintercept=mean(monthly$Pass.Per.Flight)) +
  labs(title="Passengers per Domestic Flight (2002-2016)", y="Passengers per Flight") + scale_fill_brewer(palette = "Paired") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=F) + zoom3
