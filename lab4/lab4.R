install.packages(c('tree','ISLR','randomForest', 'e1071'))
library(tree)
library(ISLR)
library(randomForest)
library(e1071)

data <-read.csv(file="cancer_data_train.csv") 

for (i in 1:ncol(data)){
  data[is.na(data[,i]),i]<- median(data[,i],na.rm =TRUE)}

data$Cancer <- as.factor(data$Cancer)
data$Smoker <- as.factor(data$Smoker)

BMI <- function(height, weight){
  return(weight/(height*0.01)^2)
}
data$bmi <- BMI(data$Height,data$Weight)
data$Height <- NULL
data$Weight <- NULL

accuracy <- vector()
precision <- vector()
recall <- vector()

set.seed(3)
datarandom<-data[sample(nrow(data)),] 
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)


accuracy <- vector()
precision <- vector()
recall <- vector()


for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  trainIndexes <- which(folds!=i,arr.ind=TRUE) 
  data_all.test <- datarandom[testIndexes, ] 
  data_all.train <- datarandom[trainIndexes, ]
  set.seed(3)
  rf.data <- randomForest(data_all.train$Cancer~ ., data =
                            data_all.train,ntree=5)
  prediction <- predict(rf.data, data_all.test)
  table <- table(prediction, data_all.test$Cancer)
  accuracy <-
    c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  precision <- c(precision,table[2,2]/(table[2,1]+table[2,2]))
  recall <- c(recall,table[2,2]/(table[1,2]+table[2,2]))
}


accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

precision
precisionaverage = mean(precision)
precisionaverage

recall
recallaverage = mean(recall)
recallaverage








accuracy <- vector()
precision <- vector()
recall <- vector()
testIndexes <- which(folds==i,arr.ind=TRUE)
trainIndexes <- which(folds!=i,arr.ind=TRUE) 
data_all.test <- datarandom[testIndexes, ]
data_all.train <- datarandom[trainIndexes, ]

set.seed(3)
rf.data <- randomForest(data_all.train$Cancer~ ., data =
                          data_all.train,ntree=3)

prediction <- predict(rf.data, data_all.test, type="class")
table <- table(prediction, data_all.test$Cancer)

precision <- c(precision,table[2,2]/(table[2,1]+table[2,2]))

set.seed(3)
model.data <-
  svm(data$Cancer~.,data,4)
test<-read.csv(file="Cancer_data_test.csv") 
test$Cancer <- as.factor(test$Cancer) 
test$Smoker <- as.factor(test$Smoker)
test$bmi <- BMI(test$Height,test$Weight)
test$Height <- NULL
test$Weight <- NULL
prediction <- predict(model.data, test, type="class")
table <- table(prediction, test$Cancer)
table

#part B

set.seed(3)
rf.data <- randomForest(data$Cancer~ ., data =
                          data,ntree=10)

test<-read.csv(file="Cancer_data_test.csv") 
test$Cancer <- as.factor(test$Cancer) 

test$Smoker <- as.factor(test$Smoker)
test$bmi <- BMI(test$Height,test$Weight)
test$Height <- NULL
test$Weight <- NULL

prediction <- predict(rf.data, test, type="class")
table <- table(prediction, test$Cancer)
table
