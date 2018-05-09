library(dplyr)
library(usdm)
library(ggplot2)
library(caTools) # splits
library(rpart) # CART
library(rpart.plot)
library(GGally)
library(ROCR)
library(MASS)
library(randomForest)
library(gbm)
data = read.csv("Final_Raw_Data.csv")

# Split data ``
data.train <- subset(data,Year<=2012)
data.test  <- subset(data,Year>2012)

# Remove columns - Year, Country
data.train = subset(data.train,select = -c(Year,Country.Name,X))
data.test = subset(data.test,select = -c(Year,Country.Name,X))
data= subset(data,select = -c(Year,Country.Name,X))
#colnames(data)

# Linear regression
linear_reg <- lm(Medal ~ .,data.train)
summary(linear_reg)



