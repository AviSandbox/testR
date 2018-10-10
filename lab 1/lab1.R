data <- read.csv(file="Cancer_data.csv")
?read.csv2()
summary(data)
head(data,1)
tail(data,2)
?tail
?names()
names(data)
#Get or set the names of an object
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- median(data[,i], na.rm =TRUE) 
}
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
datanorm <-as.data.frame(lapply(data,normalize))
hist(data$Age)
boxplot(data$Weight)
cor(data$Height,data$Weight)
cors<-cor(datanorm)
install.packages("corrplot")
library(corrplot)
corrplot(cors, method="circle")

BMI<-function(height,weight){
  return(weight/(height*0.01)^2)
}
datanorm$bmi<-normalize(BMI(data$Height,data$Weight))
cors<-cor(datanorm)
?corrplot


datanorm <- as.data.frame(lapply(data, normalize))
datanorm["BMI"]<-NA
datanorm$BMI<-normalize(BMI(data$Height,data$Weight))
corrplot(cor(datanorm),method="number")