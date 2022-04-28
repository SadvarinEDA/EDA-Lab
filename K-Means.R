install.packages("factoextra")
install.packages("cluster")
rm(list=ls())
data <- read.csv("iris.csv",row.names=1)
df <- scale(data)

fit<- kmeans(df,3) 
fit$size
fit$withinss
fit$tot.withinss

Kmax <- 15 
WCSS <- rep(NA,Kmax) 
nClust <- list() 
for (i in 1:Kmax){ 
  fit<- kmeans(df,i) 
  WCSS[i] <- fit$tot.withinss 
  nClust[[i]] <- fit$size 
}
plot(1:Kmax,WCSS,type="b",pch=19)

library(factoextra) 
fviz_nbclust(df, kmeans, method = "wss") 
fviz_cluster(fit, data)

library(cluster) 
fit <- pam(df, 3, metric = "manhattan") # K-Medoids 
print(fit)