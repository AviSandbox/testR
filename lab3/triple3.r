cleveland <- read.csv("datasets/processed.cleveland.csv",header=T)
hungarian <- read.csv("datasets/processed.hungarian.csv",header=T)
va <- read.csv("datasets/processed.va.csv",header=T)
switzerland<- read.csv("datasets/processed.switzerland.csv",header=T)
#data <- bind_rows(cleveland, hungarian, switzerland, va)
data<-rbind(cleveland,hungarian,va,switzerland)
data$slope<-NULL
data$ca<-NULL
data$thal<-NULL

#find ? elements
idx<-data =="?"
#replace elements with NA
is.na(data) <- idx


for(i in 1:ncol(data)){ #for every column of our data
  data[is.na(data[,i]), i] <- mean(as.numeric(data[,i]), na.rm =
                                     TRUE) #replace every missing values with the mean of that column
}
data[] <- lapply(data, as.integer)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

datanorm <- as.data.frame(lapply(data, normalize))
cors<-cor(datanorm)

install.packages(c('tree','ISLR','randomForest', 'e1071'))
library(tree)
library(ISLR)
library(randomForest)
library(e1071)

set.seed(3)
train =sample(nrow(data), nrow(data)*2/3)
test=-train
data_test=data[test,]
data_train=data[train,]


######Decision Trees####
set.seed(3)
tree.data_train=tree(data_train)
summary(tree.data_train)
plot(tree.data_train )
text(tree.data_train ,pretty =0)



####Random Forest####
###skip random forest####
# Warning message:
#In randomForest.default(m, y, ...) :
# The response has five or fewer unique values.  Are you sure you want to do regression?###
##possible reason https://discuss.analyticsvidhya.com/t/what-does-the-warning-the-response-has-five-or-fewer-unique-values-while-building-random-forest-mean/6442###


#set.seed(3)
#rf.data <-randomForest(data_train$class ~.,data_train, ntree=3)
#prediction <- predict(rf.data, data_test, type = 'class')
#res.rf <- table(prediction, data_test$class)
#res.rf


######## SUPPORT VECTOR MACHINES #########

set.seed(3)
svm.data <- svm(data_train$class ~., data_train, kernel ="linear") 
prediction <- predict(svm.data, data_test, type = 'class') 
res.svm <- table(prediction, data_test$class)
res.svm


