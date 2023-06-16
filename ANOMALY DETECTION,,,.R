#libraries required
library(ggplot2)# for visualizing of the data
library(lattice)#visualizing and customizing of data
library(caret)#required package for preprocessing and training of data
library(tidyverse)#for visualization of data
library(mlbench) #artificial and real-world machine learning benchmark problems for testing and comparing the performance of different machine learning algorithms.
attach(data) #attaching  data to the dataset to the search path

data <- read.csv("walmart_cleaned.csv") #reading of the data
head(data) #describing the first six rows of the  data
colSums(is.na(data)) #count of missing values in each column
str(data) #checking data type for each column
summary(data) #summary of data

boxplot(data$Weekly_Sales,
        ylab="Weekly Sales") #boxplot for detecting outliers in weekly sales
boxplot.stats(data$Weekly_Sales)$out #outliers values in weekly sales

boxplot(data$Temperature,
        ylab="Temperature") #boxplot for detecting outliers in temperature
boxplot.stats(data$Temperature)$out #detecting of the outliers values in temperature

boxplot(data$Fuel_Price,
        ylab="Fuel Price")#boxplot for detecting outliers in fuel price
boxplot.stats(data$Fuel_Price)$out #detecting of outlier values in fuel price

#prediction model,splitting data into training and testing set
set.seed(543) #setting seed for reproducibility
train_index <- sample(1:nrow(data), 0.8 * nrow(data)) #Splitting the data set into a training set (80%) and a test set (20%)
train <- data[train_index,]#training data
test <- data[-train_index,]#testing data
summary(train) #summary of train data
summary(test) #summary of test data

model <- lm(Weekly_Sales ~ Temperature + Fuel_Price, data = train) #multiple linear regression model on train data
summary(model) #statistical summary of the model
predictions <- predict(model, test) #making prediction on the test data
print(predictions)
accuracy <- mean(predictions == test$Weekly_Sales) #checking the accuracy of model
print(accuracy) 
