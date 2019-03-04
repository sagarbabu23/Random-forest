install.packages("randomForest")
library(randomForest)
str(iris)
View(iris)
set.seed(1234) #To get reproducible result
ind <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_rf <- randomForest(myFormula, data=trainData)
train_predict <- predict(iris_rf,trainData,type="response")
table(train_predict,trainData$Species)
mean(train_predict != trainData$Species) * 100

test_predict <- predict(iris_rf, newdata= testData,type="response")
table(test_predict, testData$Species)
  mean(test_predict == testData$Species) * 100
  plot(iris_rf,lwd=2)
  legend("topright", colnames(iris_rf$err.rate),col=1:4,cex=0.8,fill=1:4)
  


##### comapany data set decision tree#####

company <- read.csv(file.choose())
View(company)
str(company)
hist(company$Sales)
high <- ifelse(company$Sales >=8,"yes","no")
company =data.frame(company,high)
forest.company <- randomForest(high ~.-Sales ,data=company)
summary(forest.company)
plot(forest.company)
forest.company
View(company)

set.seed(101)
train=sample(1:nrow(company), 200)
View(train)
forest.company= randomForest(high ~.-Sales,company, subset=train)
plot(forest.company)
print(forest.company)
text(forest.company)
forest.company
tree.pred = predict(forest.company,company[-train,])
with(company[-train,], table(tree.pred, high))
mean(tree.pred == -train$high) * 100
accuracy <- (106+52)/200
accuracy

########  fraud check#######

fraud <- read.csv(file.choose())
View(fraud)
str(fraud)
hist(fraud$Taxable.Income)
Risk <- ifelse(fraud$Taxable.Income <= 30000,"Risky","good")
fraud =data.frame(fraud,Risk)
tree.fraud <- randomForest(Risk ~.-Taxable.Income ,data=fraud)
summary(tree.fraud)
plot(tree.fraud)
tree.fraud
View(fraud)

set.seed(101)
train=fraud[1:400,]
test=fraud[400:600,]
View(train)
tree.fraud= randomForest(Risk ~.,data =train)
plot(tree.fraud)
print(tree.fraud)
tree.fraud
tree.pred = predict(tree.fraud,test)
with(test, table(tree.pred, Risk))
accu <- (170+31)/201
accu
