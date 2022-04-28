data <- read.csv("iris.csv",row.names=1) 
df <- scale(data) 

ed_dist <- dist(df, method = 'euclidean') 
hierClust <- hclust(ed_dist, method = 'complete')
plot(hierClust)

plot(hierClust)
cluster <- cutree(hierClust, k = 3) 
rect.hclust(hierClust, k = 3, border = 2:4) 