#Activity 1: K-means
install.packages(c('cluster','fpc','arules'))
library(cluster)
library(fpc)
library(arules)
# The need is the possible desire for reproducible results,
# which may for example come from trying to debug your program
#we set a random seed to allow us to reproduce the same results
set.seed(5)
?set.seed 
#The dataset contains 150 observations, each of which is described by a two-element
#vector drawn from the normal distribution
x<-matrix(rnorm(300),150,2)
#Generate a deviation matrix
xmean<-matrix(rnorm((8),4,2))
xmean
which<-sample(1:4, 150,replace=T)
x
x<-x+xmean[which,]
plot(x, col=which, pch=21)
x.cluster<-cbind(x,which)
x.cluster

while(TRUE){
  centroid<-c()
  for(g in 1:4){
    centroid<-c(centroid, mean(x.cluster[x.cluster[,3]==g,1]),mean(x.cluster[x.cluster[,3]==g,2]))
  }
  centroid<-matrix(centroid,4,2,byrow=T)
  distance<-c()
  for(i in 1:nrow(x)){
    for(j in 1:4){
      dis<-sqrt(sum((x[i,]-centroid[j,])^2))
      distance<-c(distance,dis)
    }
  }
  distance<-matrix(distance,150,4,byrow=T)
  centroid.label<-apply(distance,1,which.min)
  if(all(centroid.label==x.cluster[,3])){
    km.clusters<-centroid.label
    centroid.matrix<-centroid
    break
  }else{
    x.cluster[,3]<-centroid.label
    
  }
  
}
plot(x, col=centroid.label, pch=19)
points(centroid, pch=19, col=6, cex=2)

# alternatives

set.seed(5)
km.out<-kmeans(x,2)
km.out
plot(x,col=km.out$cluster, pch=19)
points(km.out$centers, pch=19, col=6,cex=2)
#optimal nubmer of clusters
pamk.best<-pamk(x)
cat("number of clsters according to optimum average silhouette width(best seperated clusters):",pamk.best$nc,"\n")
plot(pam(x,pamk.best$nc))
