##SO2 (sulfur dioxide) is a harsh chemical that derives from fuel emission and other locomotive technology. Short term exposure can lead to respiratory issues especially in individuals with asthma.
##It can also harm trees and plants and cause acid rain

install.packages("MASS")
library(MASS)
SO2data <- read.csv('C:\Users\diuno\Desktop\Masters Business Analytics and Intelligence\Archive\Data Mining\Final Project EPA', header="TRUE")
dim(SO2data)  ##106270 observations and 25 variables
head(SO2data)
choices <- length(unique(SO2data$State.Name)) ##55
summary(SO2data)  


##################### Columns in data set
## AQI - Air Quality Index
## Mean value - mean daily value of gas level
## Max hour - hour where max value was registered
## Location setting - where the monitoring site is located suburban, urban, rural
## Land Use - type of land and it's usage industrial, agricultural, or forest
## CBSA - Statistical polygon of multiple monitoring sites
####################


hist(SO2data$AQIMax)
AQIhist <- SO2data$AQIMax
h<-hist(AQIhist, breaks=10, col="red", xlab="AQIMax", 
        main="Histogram with Normal Curve of AQIMax to Frequency") 
xfit<-seq(min(AQIMax),max(AQIMax),length=40) 
yfit<-dnorm(xfit,mean=mean(AQIMax),sd=sd(AQIMax)) 
yfit <- yfit*diff(h$mids[1:2])*length(AQIMax) 
lines(xfit, yfit, col="blue", lwd=2)
##creating a density curve within the plot since histograms do not show the full picture of the distribution and frequency and depend on bins
##In general, AQI levels do not supersede 200, with more SO2 levels dwelling in the 0 area than in 100 and above.

qplot(AQIMax, data=SO2data, geom="density", fill=MaxHourMax, alpha=I(.5), 
      main="Distribution AQIMax to Max Hour Gas", xlab="AQIMax", 
      ylab="Density") 
##density distribution of AQI Max
#################
hist(SO2data$MaxHour)
MaxHour <- SO2data$MaxHour
h<-hist(MaxHour, breaks=10, col="red", xlab="MaxHour", 
        main="Histogram with Normal Curve of MaxHour to Frequency") 
xfit<-seq(min(MaxHour),max(MaxHour),length=40) 
yfit<-dnorm(xfit,mean=mean(MaxHour),sd=sd(MaxHour)) 
yfit <- yfit*diff(h$mids[1:2])*length(MaxHour) 
lines(xfit, yfit, col="blue", lwd=2)
##The peak of SO2 levels throughout the day is between 5 to 12 AM when it hits a maximum between 9-11 AM. This suggests that perhaps there is a strong correlation between commuting time to SO2 rise in the air


#################

install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
ggplot(SO2data, aes(Land.Use, AQIMax, colour = Location.Setting)) + geom_point()
##plot of land use to AQI max based off location settings.
##We can see that rural areas suffer the most from peaks in SO2 AQI max. Suburban areas equally reach a peak. Mobile Land use in urban and center cities peaks AQI levels as well. Uknown location settings in agricultural land use have the lowest.

ggplot(SO2data, aes(MaxHourMax, AQIMax, colour=State.Name)) + geom_point()
##Georgia suffers the highest levels of AQI levels throughout most hours of the day.

ggplot(SO2data, aes(Land.Use, Latitude, colour=AQIMax)) + geom_point()
##Higher latitude does not have implication on the higher AQI levels, however, lower altitudes in residential, forest, and commercial areas do have higher AQI levels.



ggplot(SO2data, aes(MaxHour, MaxValue, colour=Location.Setting)) +
  geom_point()
##Rural areas tend to have the highest values of AQI.
##This could be due to the factor that most fuel mining areas as well as industrial areas are located in such places.


ggplot(SO2data, aes(MaxHour, MaxValue, colour=Location.Setting)) +
  geom_point() +
  facet_grid(. ~ Land.Use)
##Forest and Residential rural areas have the highest emissions of SO2 in the peak hours of the day.


###################

ggplot(SO2data, aes(Location.Setting, AQIMax, colour=Location.Setting)) +
  geom_point() +
  facet_grid(. ~ DateMonth)  
## we can see that SO2 patterns are seasonal and every year fluctuate depending on temperature.
## SO2 chemically is impacted from O2 levels in the air due to the chemical composition.


####################
ggplot(SO2data, aes(State.Name,AQIMax, colour=Location.Setting)) +
  geom_point() +
  facet_grid(. ~ MaxHour)  
##
install.packages("scatterplot3d")
library(scatterplot3d) 
attach(SO2data)
s3d <-scatterplot3d(AQIMax,Elevation,MaxHourMax, pch=16, highlight.3d=TRUE,type="h", main="Max AQI to Max Hour in Elevation Levels")
s3d$plane3d(fit)
legend3d("topright", legend = paste('Type', c('AQIMax', 'Elevation', 'MaxHourMax')), pch = 16, col = rainbow(3), cex=1, inset=c(0.02))

#### The higher the altitude, the lower SO2 levels exist in the air. This is due to the fact that SO2 is a heavy gas



