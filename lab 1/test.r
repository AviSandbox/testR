# Load the .csv files
cleveland <- read.csv("https://drive.google.com/file/d/1UbdwnAWXMVJkpMn-RfNMaTSKgV9a7_X_/view?usp=sharing",header=T)
hungarian <- read.csv("https://drive.google.com/file/d/1cMsfQLlECSXIE479eSqfJW9c0m0dz8ZZ/view?usp=sharing",header=T)
switzerland<- read.csv("https://drive.google.com/file/d/1XcxDqlyLOSMOJ4c6_4nJNKPwNoicLihU/view?usp=sharing",header=T)
va <- read.csv("https://drive.google.com/file/d/1L3povBM03AJlW_q5n1-lngNIK2vWt-MS/view?usp=sharing",header=T)
cleveland1<-read.csv(file="cleveland.csv")
clevelandA<-data.frame(cleveland1)
rename(clevelandA, c("num"="class"))#"num"="class", "gamma"="three"
head(clevelandA)
colnames(cleveland1)
cleveland1$slope<-NULL
cleveland1$ca<-NULL
cleveland1$thal<-NULL
head(cleveland1)
cols.dont.want <- c("slope", "ca","thal")
cleveland1["slope","ca","thal"] <- NULL 
head(cleveland1)
install.packages("dplyr")
install.packages("plyr")
cleveland$slope<-NULL
cleveland$ca<-NULL
cleveland$thal<-NULL
hungarian$slope<-NULL
hungarian$ca<-NULL
hungarian$thal<-NULL
switzerland$slope<-NULL
switzerland$ca<-NULL
switzerland$thal<-NULL
va$slope<-NULL
va$ca<-NULL
va$thal<-NULL
install.packages("googledrive")
library(googledrive)
names(cleveland)
colnames(cleveland)
library(dplyr)
data <- bind_rows(cleveland, hungarian, switzerland, va)

library(plyr)
rename(cleveland1, c("num"="class"))#"num"="class", "gamma"="three"
warnings(50)