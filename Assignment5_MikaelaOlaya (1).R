install.packages("dbscan")
install.packages("cluster")
install.packages("clustertend")
install.packages("dplyr")


data <- read.csv("hospital_ortho.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

#4
nc_data<- data%>%
  filter(state== "SC"|state== "VA"|state== "GA"|state== "TN"|state== "NC")
View(nc_data)

#4.1 
nc_data<- subset(nc_data, select = -c(zip, hid, city, state, th, trauma, rehab))

#4.2
nc_data<-scale(nc_data)

#5.1
k.mean.fit <- kmeans(nc_data, 3, nstart = 25) 
attributes(k.mean.fit)
k.mean.fit$cluster
k.mean.fit$centers
k.mean.fit$size

withinssplot<- function(nc_data, nc= 4703, seed=1234){
  wss<- 1
  for (i in 1:nc){
    wss[i]<- sum(kmeans(nc_data, centers = i)$withinss)}
  plot(1:nc, wss, type= "b", xlab= "Number of CLusters",
       ylab= "Within Groups SSE") }
#5.2  
withinssplot(nc_data, nc=15)

#5.3
k.means.fit <- kmeans(nc_data, 5, nstart = 25)
k.means.fit$cluster
k.means.fit$tot.withinss
k.means.fit$size


#5.4
clusplot(nc_data, k.means.fit$cluster, main = "2D representation of the cluster solution", color = TRUE, shade = TRUE, labels = 2, lines = 0)

#6.1
par(mfrow = c(2,2))
d<- dist(nc_data, method="euclidean")

H.single <- hclust(d, method = "single")
plot(H.single)

H.complete <- hclust(d, method = "complete")
plot(H.complete)

H.average <- hclust(d, method = "average")
plot(H.average)

H.ward <- hclust(d, method = "ward.D2")
plot(H.ward)

#6.2
#written response 

#6.3
#written response 

#6.4
group <- cutree(H.ward, k = 3)
plot(H.ward)
rect.hclust(H.ward, k = 3, border = "red" )

#7.1 & 7.2
kNNdistplot(nc_data, k = 4)
abline(h = 3, col = "red")

#7.3
db <- dbscan(nc_data, eps = 3, minPts = 4)
db$cluster

#7.4
print(db)

#7.5
clusplot(nc_data, k.mean.fit$cluster, main = "2D representation of the cluster solution", color = TRUE, shade = TRUE, labels = 2, lines = 0)

#8.1
plot(silhouette(k.means.fit$cluster, d))

#8.2
plot(silhouette(groups,d))

#8.3
plot(silhouette(db$cluster,d))

               
               
               