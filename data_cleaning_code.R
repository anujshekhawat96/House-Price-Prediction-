#  linear regression model to predict house and condo prices in the Washington state 
# using housing information such as size, location, view, age, renovation etc.
# We have gathered Washington State house data in Q2 of 2014


#install packages and library
install.packages(c("tidyverse","sqldf"))



library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(DataExplorer)
library(sqldf)

# Data cleaning for predicting house and condo prices in the Washington state


#import the data file
#WS_house_condo_file <- read_excel(file.choose())

WS_house_condo_file <- read_excel('House_Price_Dataset.xlsx')

#familiarize with data set, check the structure, and summary 
str(WS_house_condo_file)
summary(WS_house_condo_file)
head(WS_house_condo_file)

#check the names of variables for spelling and types of data
names(WS_house_condo_file)
typeof(WS_house_condo_file$price)
typeof(WS_house_condo_file$bedrooms)

#check for string consistencies
unique(WS_house_condo_file$country)
WS_house_condo_file$country <- as.character(gsub("USAA","USA", WS_house_condo_file$country))
unique(WS_house_condo_file$city)


# -------Newcode------Anuj Begins---------28-03-2022

#First EDA on RAW DATA before Data Cleaning

glimpse(WS_house_condo_file)  #tells rows nd cols too


datalist <- list(WS_house_condo_file) 

plot_str(datalist)
plot_intro(WS_house_condo_file)

plot_missing(WS_house_condo_file)

plot_bar(WS_house_condo_file)

plot_histogram(WS_house_condo_file)

plot_correlation(na.omit(WS_house_condo_file), maxcat = 5L) #with all variables


# Data Cleaning Process(Removal of Cols, Zeroes, N/A) 

#Checking Zeroes and N/A for each columns of dataframe

colSums(WS_house_condo_file==0)  #checking number of zeros for each cols 

colSums(is.na(WS_house_condo_file))  #checking N/A values for each cols

condodata_na <- filter(WS_house_condo_file, !is.na(WS_house_condo_file$sqft_living))

glimpse(condodata_na)

summary(condodata_na) #No N/A in this 



#we cannot have price, bathroom and bedroom as 0 so we will remove all rows have 0 values

#Now zeroes 

condo_nonzero <- filter(condodata_na, price > 0, bedrooms >0 , bathrooms>0) 

glimpse(condo_nonzero)

colSums(condo_nonzero==0)


#check for waterfront how many values are not 0 and 1

print(max(condo_nonzero$waterfront))  #all values are 0 and 1 got confirmed 

#see number of waterfront houses we have (count)
condo_nonzero %>% 
  count(waterfront)   #so we have 4518 with non waterfront and 30 with waterfront


#check City variable 

#converting City to Factor inorder to plot 

condo_nonzero$city <- as.factor(condo_nonzero$city)

typeof(condo_nonzero$city)


#count distinct in cities 

sqldf("select distinct(city) from condo_nonzero")  #it exist a result with name NA city

#removing city with name NA
condo_nonzero <- filter(condo_nonzero, condo_nonzero$city!='NA')

sqldf("select distinct(city) from condo_nonzero")  #total we have 46 cities 

#drop check date

final_data <- drop_columns(condo_nonzero, c("date"))

summary(final_data)


#QQ plot helps

qq_data <- final_data[, c("price", "bedrooms", "bathrooms", "floors")]

plot_qq(qq_data, sampled_rows = 1000L)


#slicing and dicing

final_data_slicing <- final_data[, c("price", "bedrooms", "bathrooms", "floors")]

## Call boxplot function by price
plot_boxplot(final_data_slicing, by = "price")



#dummify Data to see hot encoding 


plot_str(
  list(
    "original" = final_data,
    "dummified" = dummify(final_data, maxcat = 5L)
  )
)



#first plot to see price vs bedrooms 

plot( final_data$bedrooms,final_data$price, pch=16, col='steelblue',
     main='bedrooms vs price',
     xlab='bedrooms', ylab='Price')



# price vs bathrooms

plot( final_data$bathrooms,final_data$price, pch=16, col='blue',
      main='bathrooms vs price', 
      xlab='bathrooms', ylab='Price')



#price vs total sqft (total = sqft_living+sqft_lot+sqft_above) 
plot( final_data$sqft_living,final_data$price, pch=16, col='blue',
      main='Sqft vs price', 
      xlab='Sqft', ylab='Price')




#city vs price (which city has the highest price and lowest)

plot(final_data$city,final_data$price, pch=16, col='blue',
      main='city vs price', 
      xlab='city', ylab='Price')


#price vs old (yr_built)

plot(final_data$yr_built,final_data$price, pch=16, col='blue',
     main='yr_built vs price', 
     xlab='yr_built', ylab='Price')


#EDA AFTER Cleaning 


plot_intro(final_data)

plot_missing(final_data)

plot_bar(final_data)

plot_histogram(final_data)

plot_correlation(na.omit(final_data), maxcat = 5L)



create_report(final_data)
