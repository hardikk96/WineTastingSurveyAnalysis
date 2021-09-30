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
#install.packages("vcd")

#2 Importing all the required Packages
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
library("vcd")
library(psych)
library(tidyverse)



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

#title <- sum(is.na(data$region_1))/nrow(data)
#title 

#Price column has 5.7 percent missing value, replacing the values with mean price values of wines
meanPrice <- mean(data$price , na.rm = TRUE)
meanPrice <- round(meanPrice , 0)
meanPrice

data$price <- data$price %>% replace(is.na(.), meanPrice)
data$price

#title <- sum(is.na(data$price))/nrow(data)
#title

#changing the column names
colnames(data)
names(data)[names(data) == "region_1"] <- "region"
colnames(data)

#Data Cleaning Finished
#Visulaisation Parts Starts

countryTable <- table(data$country)
countryTable <- sort(countryTable , decreasing=TRUE)
countryTable

barplot(countryTable,
        main="Frequency of Wine Production By Country", 
        ylab=" Frequency" ,
        xlim=c(0,5), ylim=c(0,400),
        col="darkolivegreen3", width = 0.20,
        cex.names = .6 , las = 2,
)

provinceTable <- table(data$province)
provinceTable <- sort(provinceTable )
provinceTable

barplot(provinceTable,
        main="Frequency of Wine Production By Province", 
        xlab= "Number of Wines",
        xlim=c(0,300),
        col="royalblue", 
        cex.names = .6 , las = 2,horiz = TRUE
)

data %>% group_by(province) %>% summarise(n = n()) %>% 
  filter(n < 30) %>% arrange(desc(n)) %>%
  ggplot(aes(fct_reorder(province, n),n) ) + geom_bar(stat = "identity" , fill="darkblue") +  
  labs(title = "Frequency of Wine Production By Province - Without Outliers", x = '', y = "Number of Wines") +
  coord_flip()

data %>% group_by(province) %>% summarise(n = n()) %>% 
  filter(n > 30) %>% arrange(desc(n)) %>%
  ggplot(aes(fct_reorder(province, n),n)) + geom_bar(stat = "identity" , fill="azure3") +
  labs(title = "Largest Wine Producers by Province", x = '', y = "Number of Wines") +
  coord_flip()



regionTable <- table(data$region)
regionTable <- sort(regionTable )
regionTable

barplot(regionTable,
        main="Frequency of Wine Production By Region", 
        xlab= "Number Of Wines",
        xlim=c(0,40),
        col="darkolivegreen3", 
        cex.names = .4 , las = 2,horiz = TRUE, width = .25
)

data %>% group_by(region) %>% summarise(n = n()) %>% 
  filter(n > 5) %>% arrange(desc(n)) %>%
  ggplot(aes(fct_reorder(region, n),n) ) + geom_bar(stat = "identity" , fill="deepskyblue1") +  
  labs(title = "Top Wine Producers By Region", x = '', y = "Number of Wines") +
  coord_flip()

data %>% group_by(variety) %>% summarise(n = n()) %>% 
  filter(n > 10) %>% arrange(desc(n)) %>%
  ggplot(aes(fct_reorder(variety, n),n) ) + geom_bar(stat = "identity" , fill="darkolivegreen3") +  
  labs(title = "Top Produced Varieties of Wines", x = '', y = "Number of Wines") +
  coord_flip()

data %>% group_by(winery) %>% summarise(n = n()) %>% 
  filter(n > 2) %>% arrange(desc(n)) %>%
  ggplot(aes(fct_reorder(winery, n),n) ) + geom_bar(stat = "identity" , fill="cornflowerblue") +  
  labs(title = "Wineries with Top Production", x = '', y = "Number of Wines") +
  coord_flip()


ggplot(data, aes(x=price, y=points)) +
  geom_point(shape=1) +
  labs(title = "Price Distribution by Points ", x = "Price", y = "Points")

#removing the outlines as the price of the wines which are less than 200
data %>% filter(price < 200) %>%
  ggplot(aes(x=price, y=points)) +
  geom_point(shape=1) +
  labs(title = "Price less than $200 Distribution by Points - Outliers Removed", x = "Price", y = "Points")

#removing the outlines as the price of the wines which are more than 200
data %>% filter(price > 200) %>%
  ggplot(aes(x=price, y=points)) +
  geom_point(shape=1) +
  labs(title = "Price More than $200 Distribution by Points", x = "Price", y = "Points")


ggplot(data, aes(x=price, y=country)) +
  geom_point(shape=1) +
  labs(title = "Price  Distribution by Country ", x = "Price", y = "Country")

#country with wines which has price less than $200
data %>% filter(price < 200) %>%
  ggplot(aes(x=price, y=country)) +
  geom_point(shape=1) +
  labs(title = "Price less than $200 Distribution by Country - Outliers Removed", x = "Price", y = "Country")

#country with wines which has price more than $200
data %>% filter(price > 200) %>%
  ggplot(aes(x=price, y=country)) +
  geom_point(shape=1) +
  labs(title = "Highest Priced Wine by Country", x = "Price", y = "Country")

#creating histogram to check the distribution of data
hist(data$price, main = "Price Distribution", col = "grey30", xlab = "Price", ylim = c(0,1000))
hist(data$points, main = "Points Distribution", col = "dodgerblue2", xlab = "Points")

#calculating skewness of data for price and points column
priceSkewness <- skewness(data$price , na.rm = T)
priceSkewness

pointsSkewness <- skewness(data$points , na.rm = T)
pointsSkewness

#finding descriptive statistics of data fields
describe(data$points)
describe(data$price)
describe(data)