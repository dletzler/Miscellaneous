#Step 1: Load pre-prepared data
setwd("~/Alternate Career/Data Science/Code Challenges/ImpactRadius")
elem<-read.csv("elpop.csv", na.strings="")
names(elem)[1]<-"Year"

#Step 2: Clean data: standardize dates into datetime years, convert percentages to decimals, 
#and generate duplicate dataframe for projections
library(lubridate)
elem$Year<-as.Date(elem$Year, format="%m/%d/%Y")
elem$Year<-year(elem$Year)
elem$Birth.Rate<-as.numeric(gsub("%", "", elem$Birth.Rate))/100
elem$Death.Rate<-as.numeric(gsub("%", "", elem$Death.Rate))/100
elem.proj<-elem

#Step 3: Generate back-of-envelop 25-year projections for birth/death rates & net immigration
#Inspect birth/death trends
plot(x=elem$Year, y=elem$Birth.Rate, pch=19, xlab="Year", ylab="Birth Rate")
plot(x=elem$Year, y=elem$Death.Rate, pch=19, xlab="Year", ylab="Death Rate")
#Overall trend lines are irregular, but recent data provides linear downward slope
#Local maximum of child-bearing age millennials has not seemed to generate third-wave "Baby Boom"--generational trend smoothing over time

#Generate linear regression line on birth-rate data since 2001, then input predictions for 2014-2038
birth.lm<-lm(Birth.Rate ~ Year, data=elem.proj[52:64,])
summary(birth.lm)
birth.proj<-predict.lm(birth.lm, newdata = data.frame(Year = elem$Year[65:89]))
elem.proj$Birth.Rate[65:89]<-birth.proj
#Generate linear regression line on death-rate data since 1994, then input predictions for 2014-2038
death.lm<-lm(Death.Rate ~ Year, data = elem.proj[45:64,])
summary(death.lm)
death.proj<-predict.lm(death.lm, newdata = data.frame(Year = elem$Year[65:89]))
elem.proj$Death.Rate[65:89]<-death.proj
#Inspect immigration trend
plot(x=elem$Year, y=elem$Immigration, pch=19, xlab="Year", ylab="Net Immigration (Thousands)")
#Trend is highligh irregular & data is imprecise (5-year average input for each 5-year period)
#Take mean
mean(elem.proj$Immigration[31:64], na.rm=T)
#Mean is almost equal to most recent 5-year average: input mean for all projections
elem.proj$Immigration[65:89]<-mean(elem.proj$Immigration[31:89], na.rm=T)

#Step 4: Make projections for necessary values 
#Project total population 2014-2038: previous year's population plus birth-rate*popluation minus death-rate*population plus net immigration
#(Problem--need current population to generate total births/deaths: using previous year's population instead, letting underestimates 
#in birth/death cancel each other out)
for (i in 65:89){
  elem.proj$Total.Pop[i]<-elem.proj$Total.Pop[i-1] + elem.proj$Total.Pop[i-1]*elem.proj$Birth.Rate[i] -
    elem.proj$Total.Pop[i-1]*elem.proj$Death.Rate[i] + elem.proj$Immigration[i]
}

#Generate total births 1950-2038: population times birth rate
elem.proj$Total.Birth<-elem.proj$Total.Pop*elem.proj$Birth.Rate

#Generate proportion of under-15 population within whole population
elem.proj$Under15.Per<-elem.proj$Under.15.Pop/(elem.proj$Total.Pop)
plot(x=elem.proj$Year[1:64], y=elem.proj$Under15.Per[1:64], pch=19, xlab="Year", ylab="Fraction of Population Under 15")
#Slight uptick in recent years, but likely to smooth downard again a la second-wave baby-boom of 80s-90s.
#Generate gentle regression for projection
under15.lm<-lm(Under15.Per^1.5 ~ Year, data=elem.proj[54:64,])
summary(under15.lm)
under15.proj<-predict(under15.lm, newdata = elem.proj[65:89,])^(2/3)
elem.proj$Under15.Per[65:89]<-under15.proj


#Generate historical number of theoretical elementary-age children for each year on which we have data.
#Theoretically, elementary-age children should equal births in the years 5-11 years prior plus 5-11-year-old immigrants
#The latter must be calculated by determining this year's 5-11-year-old immigrant population, last year's 4-10s, etc., down to the newborn immigrants
#of 11 years prior.
#We will not consider premature deaths, since there is not sufficient data to estimate that within the dataset,
#and as using the proportion of native-born under-15s as our benchmark surely underestimates the immigrant
#youth population (who are disproportionately young).
elem.proj$Theo.Elem<-NA
for (i in 17:63){
  elem.proj$Theo.Elem[i]<-sum(elem.proj$Total.Birth[(i-11):(i-5)]) + 7/15*(sum(elem.proj$Immigration[(i-5):i]*elem.proj$Under15.Per[(i-5):(i)])) 
                           + 6/15*(elem.proj$Immigration[i-6]*elem.proj$Under15.Per[i-6]) + 1/3*(elem.proj$Immigration[i-7]*elem.proj$Under15.Per[i-7])
                           + 4/15*(elem.proj$Immigration[i-8]*elem.proj$Under15.Per[i-8]) + 1/5*(elem.proj$Immigration[i-9]*elem.proj$Under15.Per[i-10])
                           + 2/15*(elem.proj$Immigration[i-10]*elem.proj$Under15.Per[i-10]) + 1/15*(elem.proj$Immigration[i-11]*elem.proj$Under15.Per[i-11])
}
#The theoretical estimate is consistently, but proportionately, higher than the actual total.

elem.proj$Fract.Over<-elem.proj$Elem.Pop/elem.proj$Theo.Elem
mean(elem.proj$Fract.Over[34:63])
#Proportion has stabilized at around 85% actual elementary-age children per theoretical total.

#Generate final projections for elementary-age population using adjusted formula from above.
for (i in 64:89){
  elem.proj$Elem.Pop[i]<-0.85*(sum(elem.proj$Total.Birth[(i-11):(i-5)]) + 7/15*(sum(elem.proj$Immigration[(i-5):i]*elem.proj$Under15.Per[(i-5):(i)])) 
                               + 6/15*(elem.proj$Immigration[i-6]*elem.proj$Under15.Per[i-6]) + 1/3*(elem.proj$Immigration[i-7]*elem.proj$Under15.Per[i-7])
                               + 4/15*(elem.proj$Immigration[i-8]*elem.proj$Under15.Per[i-8]) + 1/5*(elem.proj$Immigration[i-9]*elem.proj$Under15.Per[i-10])
                               + 2/15*(elem.proj$Immigration[i-10]*elem.proj$Under15.Per[i-10]) + 1/15*(elem.proj$Immigration[i-11]*elem.proj$Under15.Per[i-11]))
}

#Visualize time series
library(ggplot2)
library(ggthemes)
e<-ggplot(data=elem.proj[1:63,], aes(x=Year, y=Elem.Pop))
e + geom_line() + geom_line(data=elem.proj[64:89,], aes(x=Year, y=Elem.Pop), linetype=2, color="red") +
  labs(title="Projected US Elementary-Age Children (Age 5-11), 1950-2038", x="Year", y="Elementary Population (Thousands)") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_few() + geom_text(x=2020, y=30175, label="30.2 million (2038 projection)") +
  geom_text(x=1995, y=elem.proj[64,2], label="25.4 million (2012 actual)")

#Export csv and visual
ggsave("projection3.png")
write.csv(elem.proj[,1:2], "elem_proj.csv", row.names=F)


simple.lm<-lm(Elem.Pop~Year, data=elem[31:63,])
predict(simple.lm, newdata=elem[64:89,])
