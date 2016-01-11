library(RPostgreSQL)
library(ggplot2)
require(gridExtra)
library("plot3D")
require(tree)
library(caret)
library(partykit)
library("reshape2")
library(rpart)
library(doParallel)

set.seed(123)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')

query="select speed,distance_along_road as distance from map_matched_express_ways where month=8 and day=1 and hour=18
          and road_id in (30634,30635,30636,38541,30637,30638,30639,30640,30641,
          37981,30642,30643,38539,30644,30645,30646,30647,30648,30649,30650,30651,30580,30581) AND segment<>-1
          order by distance_along_road"

training<-dbGetQuery(con,query)

#Decision tree rpart

modelFit <- rpart(speed ~ distance,data=training,control=rpart.control(maxdepth=14))
rpart1a <- as.party(modelFit)
plot(rpart1a)

pred=predict(modelFit)
mse_rpart=mean((training$speed-pred)^2)

printcp(modelFit)
plotcp(modelFit)

index=1
decisonTreeSplit=numeric()
for(split in modelFit$splits[,4]){
  decisonTreeSplit[index]=split
  index=index+1
}

decisonTreeSplit<-sort(decisonTreeSplit)
decisonTreeSplit<-c(0.0,decisonTreeSplit)
