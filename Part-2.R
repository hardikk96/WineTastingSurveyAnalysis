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

#title <- sum(is.na(data$province))/nrow(data)
#title 

#As province column has no  null and garbage value, replacing the missing region_1 values with Province Values
data[is.na(data$region_1),]$region_1 <- data[is.na(data$region_1),]$province

#Price column has 5.7 percent missing value, replacing the values with mean price values of wines
meanPrice <- mean(data$price , na.rm = TRUE)
meanPrice <- round(meanPrice , 0)
meanPrice

data$price <- data$price %>% replace(is.na(.), meanPrice)
data$price

#title <- sum(is.na(data$price))/nrow(data)
#title

meanPrice1 <- mean(data$price , na.rm = TRUE)
meanPrice1 <- round(meanPrice , 0)
meanPrice1

sdWinePrice <- sd(data$price)
sdWinePrice <- round(sdWinePrice , 2)
sdWinePrice

#changing the column names
colnames(data)
names(data)[names(data) == "region_1"] <- "region"
colnames(data)
str(data)

#Selecting population for Test 1:
usWinePrice <- filter(data, country == "US" )
usWinePrice
usWinePrice <- select(usWinePrice ,country ,price)
usWinePrice
sampleSize1 <-nrow(usWinePrice)*0.25  
sampleSize1 <- round(sampleSize1 , 0)
sampleSize1
sampleUsWinePrice <- sample_n(usWinePrice,sampleSize1,replace = TRUE)
sampleUsWinePrice
str(sampleUsWinePrice) 

italyWinePrice <- filter(data, country == "Italy" )
italyWinePrice
italyWinePrice <- select(italyWinePrice ,country ,price)
italyWinePrice
sampleSize2 <-nrow(italyWinePrice)*0.25  
sampleSize2 <- round(sampleSize2 , 0)
sampleSize2
sampleItalyWinePrice <- sample_n(italyWinePrice,sampleSize2,replace = TRUE)
sampleItalyWinePrice
str(sampleItalyWinePrice)

result1 <- t.test(sampleUsWinePrice$price,sampleItalyWinePrice$price , var.equal= TRUE)
result1

#Selecting population for Test 2:
napaValleyWinedata <- filter(data, region == "Napa Valley" )
napaValleyWinedata
napaValleyWinedata <- select(napaValleyWinedata ,region ,price)
napaValleyWinedata
sampleSize3 <-nrow(napaValleyWinedata)*0.40  
sampleSize3 <- round(sampleSize3 , 0)
sampleSize3
sampleNapaValleyWinedata <- sample_n(napaValleyWinedata,sampleSize3,replace = TRUE)
sampleNapaValleyWinedata
str(sampleNapaValleyWinedata)
sampleWinePriceMean <- mean(sampleNapaValleyWinedata$price , na.rm = TRUE)
sampleWinePriceMean <- round(sampleWinePriceMean , 2)
sampleWinePriceMean

#Conducting Hypothesis Test - 2
#Using a Z- Test distribution for mean age of students in the baseline survey report
n                 <- sampleSize3         # wines
X_bar             <- sampleWinePriceMean      # US Wine Price - sample mean
Mean              <- meanPrice1          # Wine Price - population mean
Stdev             <- sdWinePrice       # Wine Price - population standard deviation
alpha             <- 0.05               # significance level
confidence_level  <- 1 - alpha          # 95 %

#H0 = 37 , H1 > 37 , calculating the confidence interval
zCriticalValue <- qnorm(confidence_level)
cat("z-Score critical_value ", zCriticalValue, "\n")

zStatistic <- (X_bar - Mean) / (Stdev / sqrt(n))
cat("z-Statistic ", zStatistic, "\n")

pValue = 1 - pnorm(zStatistic)
cat("p-value is ", pValue, "\n")


#Selecting population for Test 2:
pinotNoirWineData <- filter(data, variety == "Pinot Noir" )
pinotNoirWineData
pinotNoirWineData <- select(pinotNoirWineData ,variety ,points)
pinotNoirWineData
sampleSize4 <-nrow(pinotNoirWineData)*0.25 
sampleSize4 <- round(sampleSize4 , 0)
sampleSize4
samplePinotNoirWineData <- sample_n(pinotNoirWineData,sampleSize4,replace = TRUE)
samplePinotNoirWineData
str(samplePinotNoirWineData) 

result2 <- t.test(samplePinotNoirWineData$points, mu = 88)
result2
