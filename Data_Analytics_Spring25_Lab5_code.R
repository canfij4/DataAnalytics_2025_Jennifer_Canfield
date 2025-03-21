library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(class)


## read data
#NY_House_Dataset <- read_csv("Courses/Data Analytics/Fall24/assignments/NY-House-Dataset.csv")

#dataset <- NY_House_Dataset

wine <- read.csv("C:/Users/Jennifer Canfield/Desktop/Data Analytics/Lab4/wine.data")
colnames(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
dataset <- wine
dataset$Type <- as.factor(dataset$Type)

## column names
names(dataset)

## split train/test
train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

X <- dataset[,2:14] 
Y <- as.factor(as.matrix(dataset[,1]))

## feature boxplots
boxplot(X, main="wine features")

## class label distributions
plot(Y)

## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

ggplot(dataset, aes(x = Alcohol, y = Ash, colour = Type)) +
  geom_point()

attach(dataset)
#SVM Model 1 - Linear with Alcohol variable
svr.mod1 <- svm(Type ~ Alcohol, train)

summary(svr.mod1)

svr.pred <- predict(svr.mod1, test)

svr.pred <- data.frame(real=(test$Type),pred=svr.pred)

ggplot(svr.pred, aes(x = Alcohol, y = svr.pred)) +
  geom_point() +
  stat_smooth(method = "lm")

#SVM Model 2 - polynomial with Alcohol variable
svr.mod2 <- svm((Type) ~ (Alcohol), dataset, kernel="polynomial")

summary(svr.mod2)

tuned.svm <- tune.svm(Type ~ Alcohol, data = train, kernel = 'polynomial',gamma = seq(1/2^nrow(dataset),1, .01), cost = 2^seq(-6, 4, 2))
tuned.svm

svm.mod2 <- svm(Type ~ Alcohol, data = train, kernel = 'polynomial', gamma = .6, cost = 4)
svm.mod2

svm.pred <- predict(svm.mod2, train)

svm.outs <- data.frame(real=(train$Type), pred=svm.pred)

cm_train = as.matrix(table(Actual = train$Type, Predicted = svm.pred))


test.pred <- predict(svm.mod2, test)
cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


#Classifcation Model: kNN
#using knn model

#knn model

knn.predicted <- knn(train =dataset[,2:14], test = dataset[,2:14], cl = dataset$Type, k=13)

contingency.table <- table(knn.predicted, dataset$Type, dnn=list('predicted','actual'))
contingency.table
n = sum(contingency.table) # number of instances
nc = nrow(contingency.table) # number of classes
diag = diag(contingency.table) # number of correctly classified instances per class 
rowsums = apply(contingency.table, 1, sum) # number of instances per class
colsums = apply(contingency.table, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall_1 = diag / rowsums 
precision_1 = diag / colsums
f1 = 2 * precision_1 * recall_1 / (precision_1 + recall_1) 

data.frame(precision_1, recall_1, f1)


######
#comparing the two models 

#The kNN model has an overall higher percision score for the three types of wine compared to the SVM polynomial model
#the recall and f-score are also better for the kNN model
#Overall the kNN model is able to better classifier the types of wine based on the alcohol content

