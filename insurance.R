library(tidyverse)
library(showtext)
library(ggtext)
data45 <- data45
library(ggplot2)
library(ggrepel)


din (data45)

str(data45)

#--------------------------

e <- ggplot(data45, aes(County,Population))
e + geom_text(aes(label=County))
e + geom_text(aes(label=County,colour=factor(County)))


e + geom_point(aes(colour=County))

#--------------------------------------------------------------------------
ggplot(data45, aes(x= County, y= Population, colour="green", label=County))+
  geom_point() +geom_text(hjust=0, vjust=0)


ggplot(data45, aes(x= County, y= Age.Adjusted.Premature.Death.Rate, colour="green", label=County))+
  geom_point() +geom_text(hjust=0, vjust=0)
#------------------------------------------------------------------

e <- ggplot(data45, aes(County,Age.Adjusted.Premature.Death.Rate))
e + geom_text(aes(label=County))

e + geom_text(aes(label=County,colour=factor(County)))

e + geom_point(aes(colour=County))
#-------------------------------------------------------------
library("factoextra")

data45

#iris Labels
data45.labels = data45$Age.Adjusted.Premature.Death.Rate
table(data45.labels)
data45_data <- data45 [1:2]

#scale data

data45_data_scale <- scale(data45_data)

#distance 

data45_data <- dist(data45_data_scale)

#calculate how many clusters you need
#fviz_nbclust use this to find the right ammount of clusters
fviz_nbclust(data9_data_scale, kmeans, method= "wss") +
  labs(subtitle= "Elbow Method")


#kmeans

km.out <- kmeans(data45_data_scale, centers= 3, nstart=58)
print(km.out)


#Visualizing cluster data

km.clusters <- km.out$cluster
#rownames(data9_data_scale) <- paste(data9$Efficiency, 1: dim(data9)[1], sep = "_")

require(reshape2)
data25$id <- rownames(data25) 
melt(data25)

fviz_cluster(list(data=data9_data_scale, cluster= km.clusters))


#-------------------------------------------
install.packages("dplyr")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)


data45
mydata= select(data45,c(1,2))

#WSS plot function

wssplot <- function(data, nc=15, seed=12)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  
  for (i in 2: nc){
    set.seed(seed)
    wss[i]<- sum(kmeans(data,county=i)$withinss)}
  plot(1:nc,wss, type= "b", xlab= "Number of Clusters",
       ylab="within groups sum of squares")
}


wssplot(mydata)


KM=kmeans(mydata,3)


autoplot(KM,mydata, frame=TRUE)

KM$county

table(km.clusters, data9$E)
#---------------------------------------------

