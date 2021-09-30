#1
#Installing all the required packages
#install.packages("FSA")
#install.packages("FSAdata")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("moments")
#install.packages("plotrix")
#install.packages("devtools")
#install.packages("gtools")
#install.packages("gmodels")

#2 Importing all the required Packages
library(devtools)
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("ggplot2")
library("moments")
library("plotrix")
library(data.table)
library(gtools)
library(gmodels)
library(viridisLite)
library(tidyverse)
library(tidyr)
library(psych)


#3 Increasing the r console limit and uploading a csv data file to a dataframe
options(max.print = 10000)

#Uploading the data into dataframme
data <- read.csv(file.choose( ) , header = TRUE , na.strings = c("", "NA")) 
data
head(data , 5)

#checking structure and class of dataframe
str(data)
class(data)

#checking data for cleaning , dropping taster_name , and description column as there is no use of these datasets for analysis
data$X <- NULL
data$taster_twitter_handle <- NULL
data$description <- NULL

str(data)

#Removing the duplicate data form the dataset
data <- data[!duplicated(data),]

#Checking the Percent of Missing Value for Each Column
country <- sum(is.na(data$country))/nrow(data)
country <- country * 100
country

designation <- sum(is.na(data$designation))/nrow(data)
designation <- designation * 100
designation

points <- sum(is.na(data$points))/nrow(data)
points <- points * 100
points

price <- sum(is.na(data$price))/nrow(data)
price <- price * 100
price

province <- sum(is.na(data$province))/nrow(data)
province <- province * 100
province

region_1 <- sum(is.na(data$region_1))/nrow(data)
region_1 <- region_1 * 100
region_1

region_2 <- sum(is.na(data$region_2))/nrow(data)
region_2 <- region_2 * 100
region_2

taster_name <- sum(is.na(data$taster_name))/nrow(data)
taster_name <- taster_name * 100
taster_name

title <- sum(is.na(data$title))/nrow(data)
title <- title * 100
title

variety <- sum(is.na(data$variety))/nrow(data)
variety <- variety * 100
variety

winery <- sum(is.na(data$winery))/nrow(data)
winery <- winery * 100
winery

nullvaluePercent <-  c(country, designation, points, price , province , region_1 , region_2, taster_name , title , variety , winery )
VariableName <- c("country", "designation", "points", "price" , "province" , "region_1" , "region_2", "taster_name" , "title" , "variety" , "winery")
nullValueData <- data.frame(VariableName, nullvaluePercent)
nullValueData

#removing the column with highest percent of data loss , Region_2 = 63.3 percent
data$region_2 <- NULL

#filling the missing values in the column with second and third highest missing value percent , Designation = 24.7 , taster_name = 20.6
data$designation <- data$designation %>% replace(is.na(.), "None")
data$designation

data$taster_name <- data$taster_name %>% replace(is.na(.), "Not Available")
data$taster_name

#As country and Province column has only 0.1 of missing values, dropping those rows
data <- data[!is.na(data$country),];
data <- data[!is.na(data$province),]

#As province column has no  null and garbage value, replacing the missing region_1 values with Province Values
data[is.na(data$region_1),]$region_1 <- data[is.na(data$region_1),]$province

#Price column has 5.7 percent missing value, replacing the values with mean price values of wines
meanPrice <- mean(data$price , na.rm = TRUE)
meanPrice <- round(meanPrice , 0)
meanPrice

data$price <- data$price %>% replace(is.na(.), meanPrice)
data$price

#changing the column names
colnames(data)
names(data)[names(data) == "region_1"] <- "region"
colnames(data)
str(data)


#1
#Selecting population for Test 1:
pinotNoirWineData <- filter(data, variety == "Pinot Noir" )
pinotNoirWineData

#conduting graphical analysis for regression 
scatter.smooth(x=pinotNoirWineData$price, y=pinotNoirWineData$points, main="Wine Price ~ Points for Pinot Noir Wine Variety")  

corr <-  cor(pinotNoirWineData$price , pinotNoirWineData$points)  
corr

par(mfrow=c(1, 2))  
boxplot(pinotNoirWineData$price, main="Pinot Noir Wine Price", sub=paste("Outlier rows: ", boxplot.stats(pinotNoirWineData$price)$out))
boxplot(pinotNoirWineData$points, main="Pinot Noir Wine Points", sub=paste("Outlier rows: ", boxplot.stats(pinotNoirWineData$points)$out))  

par(mfrow=c(1, 2))  
plot(density(pinotNoirWineData$price), main="Density Plot: Pinot Noir Wine Price", ylab="Wine Price", sub=paste("Skewness:", round(e1071::skewness(pinotNoirWineData$price), 2)))  
polygon(density(pinotNoirWineData$price), col="red")

plot(density(pinotNoirWineData$points), main="Density Plot: Pinot Noir Wine Points", ylab="Wine Points", sub=paste("Skewness:", round(e1071::skewness(pinotNoirWineData$points), 2)))  
polygon(density(pinotNoirWineData$points), col="royalblue")

filteredpinotNoirWineData <- subset(pinotNoirWineData, subset=(pinotNoirWineData$price < 100))
filteredpinotNoirWineData

par(mfrow=c(1, 2))
boxplot(filteredpinotNoirWineData$price, main="Density Plot: Wine Price - Outliers Removed", sub=paste("Outlier rows: ", boxplot.stats(filteredpinotNoirWineData$price)$out))
plot(density(filteredpinotNoirWineData$price), main="Density Plot: Wine Price - Outliers Removed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(filteredpinotNoirWineData$price), 2)))  
polygon(density(filteredpinotNoirWineData$price), col="red")

linearMod <- lm(points ~ price, data=filteredpinotNoirWineData)  # build linear regression model on full data
print(linearMod)
summary(linearMod)
confint(linearMod, level=0.95)

#2
usWineData <- filter(data, country == "US" )
usWineData
str(usWineData)

corr <-  cor(usWineData$price , usWineData$points)  
corr

#conduting graphical analysis for regression 
scatter.smooth(x=usWineData$points, y=usWineData$price, main="Wine Price ~ Points for Wines Created in United States")  

par(mfrow=c(1, 2))  
boxplot(usWineData$price, main="Wine Prices for United States", sub=paste("Outlier rows: ", boxplot.stats(pinotNoirWineData$price)$out))
boxplot(usWineData$points, main="Wine Points for United States", sub=paste("Outlier rows: ", boxplot.stats(pinotNoirWineData$points)$out))  

par(mfrow=c(1, 2))  
plot(density(usWineData$price), main="Density Plot: Wine Prices for United States", ylab="Wine Price", sub=paste("Skewness:", round(e1071::skewness(usWineData$price), 2)))  
polygon(density(usWineData$price), col="red")

plot(density(usWineData$points), main="Density Plot: Wine Points for United States", ylab="Wine Points", sub=paste("Skewness:", round(e1071::skewness(usWineData$points), 2)))  
polygon(density(usWineData$points), col="royalblue")

#remove outliers - complete the process.
filteredusWineData <- subset(usWineData, subset=(usWineData$price < 80))
filteredusWineData
str(filteredusWineData)

par(mfrow=c(1, 2))
boxplot(filteredusWineData$price, main="Density Plot: Pinot Noir Wine Price - Outliers Removed", sub=paste("Outlier rows: ", boxplot.stats(filteredusWineData$price)$out))
plot(density(filteredusWineData$price), main="Density Plot: Pinot Noir Wine Price - Outliers Removed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(filteredusWineData$price), 2)))  
polygon(density(filteredusWineData$price), col="red")

linearMod2 <- lm(points ~ price, data=filteredusWineData)  # build linear regression model on full data
print(linearMod2)
summary(linearMod2)
confint(linearMod2, level=0.95)

#3  
data$Country_US <- ifelse(data$country == "US", 1,0)
str(data)

#removing outliers
corr <-  cor(data$price , data$Country_US)  
corr

par(mfrow=c(1, 2))
boxplot(data$price, main=" Wine Price", sub=paste("Outlier rows: ", boxplot.stats(data$price)$out))

filteredWineData <- subset(data, subset=(data$price < 68))
str(filteredWineData)

boxplot(filteredWineData$price, main=" Wine Price - Without Outliers", sub=paste("Outlier rows: ", boxplot.stats(filteredWineData$price)$out))

linearMod3 <- lm(price ~ Country_US, data=filteredWineData)  # build linear regression model on full data
print(linearMod3)
summary(linearMod3)
confint(linearMod3, level=0.95)