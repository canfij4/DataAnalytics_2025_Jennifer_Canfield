library("ggplot2")
library("readr")
#Lab 2 Linear Regression 
#exercise 1: linear models 
#examine the influence of various variables on property price
NY_House_Dataset <- read.csv("C:/Users/Jennifer Canfield/Desktop/Data Analytics/Lab2/NY-House-Dataset.csv")
dataset <- NY_House_Dataset
#fit 3 linear models with price as the response variable 
  #and combinations of PropertySqFt, Beds, and Bath as predictors 
#do any data cleaning to get the best possible models 
#for each model print the model summary stats and plot the most significant variable vs price

##Linear model 1: PropertySqFt & Beds vs price 
ggplot(dataset, aes(x = log10(PROPERTYSQFT) + (BEDS), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset <- dataset[dataset$BEDS <30,]

## fit linear model
lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT)+BEDS, data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)
plot(log10(PRICE)~BEDS, data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = log10(PROPERTYSQFT)+BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

##Linear model 2: Beds & Bath vs price 
ggplot(dataset, aes(x = (BATH) + (BEDS), y = log10(PRICE))) +
  geom_point()
## filter data
dataset <- dataset[dataset$PRICE<195000000,]

#dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset <- dataset[dataset$BEDS <25,]
dataset <- dataset[dataset$BATH <15,]
ggplot(dataset, aes(x = (BATH) + (BEDS), y = log10(PRICE))) +
  geom_point()

## fit linear model
lmod <- lm(log10(PRICE)~BATH+BEDS, data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(log10(PRICE)~BATH, data = dataset)
abline(lmod)
plot(log10(PRICE)~BEDS, data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = BATH+BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

##Linear model 3: PropertySqFt & Beds & Bath vs price 
ggplot(dataset, aes(x = log10(PROPERTYSQFT) + (BEDS) + (BATH), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
dataset <- dataset[dataset$BEDS <25,]
dataset <- dataset[dataset$BATH <15,]
ggplot(dataset, aes(x = (BATH) + (BEDS), y = log10(PRICE))) +
  geom_point()

## fit linear model
lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT)+BEDS+BATH, data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)
plot(log10(PRICE)~BEDS, data = dataset)
abline(lmod)
plot(log10(PRICE)~BATH, data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = log10(PROPERTYSQFT)+BEDS+BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")
