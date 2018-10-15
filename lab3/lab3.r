data<-read.csv(file="cancer_data_train.csv")
for(i in 1:ncol(data)){ #for every column of our data
  data[is.na(data[,i]), i] <- mean(as.numeric(data[,i]), na.rm =
                                     TRUE) #replace every missing values with the mean of that column
}
install.packages(c('tree','ISLR','randomForest', 'e1071'))
library(tree)
library(ISLR)
library(randomForest)
library(e1071)
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
set.seed(3)
train =sample(nrow(data), nrow(data)*2/3)
test=-train
data_test=data[test,]
data_train=data[train,]

######Decision Trees####
set.seed(3)
tree.data_train=tree(data_train$Cancer ~.,data_train)
summary(tree.data_train)
plot(tree.data_train )
text(tree.data_train ,pretty =0)

####Random Forest####

set.seed(3)
rf.data <-randomForest(data_train$Cancer~.,data_train, ntree=3)
prediction <- predict(rf.data, data_test, type = 'class')
res.rf <- table(prediction, data_test$Cancer)
res.rf

######## SUPPORT VECTOR MACHINES #########

set.seed(3)
svm.data <- svm(data_train$Cancer~.,data_train, kernel ="linear") 
prediction <- predict(svm.data, data_test, type = 'class') 
res.svm <- table(prediction, data_test$Cancer)
res.svm
#lab4

set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  data_all.test <- datarandom[testIndexes, ] 
  data_all.train <- datarandom[trainIndexes, ]
  set.seed(3)
  svm.model <- svm(data_all.train$Cancer~ ., data =
                     data_all.train, kernel = "linear")
  prediction <- predict(svm.model, data_all.test)
  table <- table(prediction, data_all.test$Cancer)
  accuracy <-c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
}
  

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage
#random forest accuracy

accuracy <- vector()
precision <- vector()
recall <- vector()

for (i in  1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  set.seed(3)
  rf.data <- randomForest(data_all.train$Cancer~ ., data =
                            data_all.train,ntree=2)
  
  prediction <- predict(rf.data, data_all.test, type="class") 
  table <- table(prediction, data_all.test$Cancer)
  accuracy <-c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
}

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

#Recall for svm

accuracy <- vector()
precision <- vector()
recall <- vector()


set.seed(3)
datarandom<-data[sample(nrow(data)),]
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  data_all.test <- datarandom[testIndexes, ] 
  data_all.train <- datarandom[trainIndexes, ]
  set.seed(3)
  svm.model <- svm(data_all.train$Cancer~ ., data =
                     data_all.train, kernel = "linear")
  prediction <- predict(svm.model, data_all.test)
  table <- table(prediction, data_all.test$Cancer)
  recall <- c(recall, table[2,2]/(table [2,2]+table[1,2]))
}

recall
recallaverage <- mean (recall)
recallaverage

#Recall random forest
accuracy <- vector()
precision <- vector()
recall <- vector()

for (i in  1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  set.seed(3)
  rf.data <- randomForest(data_all.train$Cancer~ ., data =
                            data_all.train,ntree=10)
  
  prediction <- predict(rf.data, data_all.test, type="class") 
  table <- table(prediction, data_all.test$Cancer)
  recall <- c(recall, table[2,2]/(table [2,2]+table[1,2]))
}

recall
recallaverage <- mean (recall)
recallaverage

#randomforest accuracy

accuracy <- vector()
precision <- vector()
recall <- vector()

#Part B
accuracy <- vector()
precision <- vector()
recall <- vector()

set.seed(3)
model.data <-
  randomForest(data$Cancer~.,data,ntree=10)
test<-read.csv(file="cancer_data_test.csv")
test$Cancer <- as.factor(test$Cancer) 

test$Smoker <- as.factor(test$Smoker)
test$bmi <- BMI(test$Height,test$Weight)
test$Height <- NULL
test$Weight <- NULL

prediction <- predict(model.data, test, type="class")
table <- table(prediction, test$Cancer)
table