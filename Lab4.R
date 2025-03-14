############
### Lab4 ### 
############

library(caret)
library(ggfortify)
library(ellipse)
library(class)
library(dplyr)

#1. compute the PCs andplot the dataset using the 1st and 2nd PC
# PCA with wine dataset
wine <- read.csv("C:/Users/Jennifer Canfield/Desktop/Data Analytics/Lab4/wine.data")
colnames(wine) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
wine.df <- wine
head(wine.df)

# creating another dataframe from iris dataset that contains the columns from 1 to 4
X <- wine.df[,2:14]
Y <- wine.df[,1]

## scatter plot of 2 variables colored by class
ggplot(X, aes(x = Ash, y = Magnesium, color = Y, fill = Y)) + geom_point() + 
  stat_ellipse(type = "t",geom = "polygon",alpha = 0.4)

## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## Variance/Covariance

var(X$Magnesium)
var(X$Ash)
cov(X$Ash,X$Magnesium) #covariance
cor(X$Ash,X$Magnesium) #correlation between 0-1

## scatter plot of 2 variables --> to show correlation 
ggplot(X, aes(x = Ash, y = Magnesium)) + geom_point(color="blue")

var(X$Flavanoids)
var(X$Hue)
cov(X$Hue,X$Flavanoids) #negative covariance --> as len inc, wid dec
cor(X$Hue,X$Flavanoids) #negative is the direction 

## scatter plot of 2 variables
ggplot(X, aes(x = Hue, y = Flavanoids)) + geom_point(color="blue")


####### PCA #######
#center and scale dataset
X.scaled <-scale(X,center = TRUE)
principal_components <- princomp(X.scaled, score = TRUE)

summary(principal_components)

principal_components$loadings #theta for set of weights
#would use to calculate the scores
principal_components$scores #new dataset projected onto principal components

# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#2. Identify the variables that contribute the most to the 1st PC
#variables: Flavanoids, OD280/od315, and Total phenols contribute the most
#3. Drop the variables that least contribute to 1st PCA and rerun PCA
# creating another dataframe from iris dataset that contains the columns from 1 to 4
X <- wine.df[,3:13]
Y <- wine.df[,1]

####### PCA #######
X.scaled <-scale(X,center = TRUE)
principal_components <- princomp(X.scaled, score = TRUE)

summary(principal_components)

principal_components$loadings #theta for set of weights
#would use to calculate the scores
principal_components$scores #new dataset projected onto principal components

# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#4. Train a classifier model to predict wine type using the original dataset
#using knn model
dataset <- wine

dataset$type.group[dataset$class==1] <- "Rose"
dataset$type.group[dataset$class==2] <- "White"
dataset$type.group[dataset$class==3] <- "Red"
#knn model
sqrt(177) #use to find k

knn.predicted <- knn(train =dataset[,2:14], test = dataset[,2:14], cl = dataset$type.group, k=13)

contingency.table <- table(knn.predicted, dataset$type.group, dnn=list('predicted','actual'))

#calculate classification accuracy
accuracy <- sum(diag(contingency.table))/length(dataset$type.group)
#calculate classification precision 
precision <- sum(diag(contingency.table))/sum(contingency.table[3:5,2])


#5 train a classifier model to predict wine type using the data projected into the first 3PCs
proj_data <- principal_components$scores
dataset1 <- as.data.frame(proj_data[,1:3]) #first 3 PCs
#put class column back into dataset
dataset1 <- cbind(dataset1, type.group = dataset$type.group)
#knn model 2
sqrt(177) #use to find k

knn.predicted <- knn(train =dataset1[,1:3], test = dataset1[,1:3], cl = dataset1$type.group, k=13)

contingency.table1 <- table(knn.predicted, dataset1$type.group, dnn=list('predicted','actual'))

#calculate classification accuracy
accuracy1 <- sum(diag(contingency.table1))/length(dataset1$type.group)
precision1 <- sum(diag(contingency.table1))/sum(contingency.table1[3:5,2])
#6. compare the 2 classification models using contingency tables and precision/recall/f1 metrics
#knn model 1 contigency table
contingency.table
#knn model 2 contigency table
contingency.table1
#knn model 1 accuracy 
accuracy
#knn model 2 accuracy 
accuracy1
#model 2 has a better contigency table and accuracy score, meaning it classifies the groups better. 


