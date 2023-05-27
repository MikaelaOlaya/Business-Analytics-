install.packages("dbscan")
install.packages("cluster")
install.packages("clustertend")
install.packages("dplyr")


data <- read.csv("universities.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
data <- drop_na(data)
#1A
library(dplyr)
library(tidytext)

text_df = tibble(line = 1:653, text = data$College.Name)
tidy_text = text_df %>%  
  unnest_tokens(word, text)
data(stop_words)
tidy_text <- tidy_text %>% 
  anti_join(stop_words)
tidy_text %>%  
  count(word, sort= TRUE)

#1C
tidy_text%>%
  dplyr::count(word, sort = TRUE) %>%
  filter(n>=7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


#2Aa 
college_data<- subset(data, select = -c(id, College.Name, Public..1...Private..2., State))
college_data<-scale(college_data)

# k-means 
library(cluster)
library(clustertend)
library(dbscan)
k.mean.fit <- kmeans(college_data, 3, nstart = 25) 
attributes(k.mean.fit)

k.mean.fit$cluster
k.mean.fit$centers
k.mean.fit$size

withinssplot<- function(college_data, nc= 653, seed=1234){
  wss<- 1
  for (i in 1:nc){
    wss[i]<- sum(kmeans(college_data, centers = i)$withinss)}
  plot(1:nc, wss, type= "b", xlab= "Number of CLusters",
       ylab= "Within Groups SSE") }

withinssplot(college_data, nc=15)

#2.A.B.
par(mfrow = c(2,2))
d<- dist(college_data, method="euclidean")

H.single <- hclust(d, method = "single")
plot(H.single)

H.complete <- hclust(d, method = "complete")
plot(H.complete)

H.average <- hclust(d, method = "average")
plot(H.average)

H.ward <- hclust(d, method = "ward.D2")
plot(H.ward)

group <- cutree(H.ward, k = 3)
plot(H.ward)
rect.hclust(H.ward, k = 3, border = "red" )


#2.A.C
kNNdistplot(college_data, k = 3)
abline(h = 3, col = "red")

db <- dbscan(college_data, eps = 3, minPts = 4)
db$cluster


plot(silhouette(k.mean.fit$cluster, d), border = NA)

plot(silhouette(group,d), border= NA)

plot(silhouette(db$cluster,d), border = NA)


#2B
data$cluster <- db$cluster 
cluster <- data[,c("College.Name","cluster")] %>% 
  filter(College.Name == "Georgia State University"| College.Name == "University of North Carolina at Charlotte"| College.Name == "University of Arizona"  )
cluster

#2C
cluster <- data[,c("College.Name","cluster")] %>% 
  filter(College.Name == "University of North Carolina at Chapel Hill"| College.Name == "University of Texas at Austin")
cluster


#2D

k.mean.fit <- kmeans(college_data, 3, nstart = 25) 
attributes(k.mean.fit)

k.mean.fit$cluster
k.mean.fit$centers
