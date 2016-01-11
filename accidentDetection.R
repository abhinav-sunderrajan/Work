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

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')


day=3
month=8
hours="(17,18)"



query=paste("select track_info_id,road_id,segment,speed,time_stamp,day,month,hour,minute,distance_along_road from map_matched_express_ways where day=",day 
," and month=",month," and hour in ",hours, "and segment <>-1 and road_id in(30634,30635,30636,38541,30637,30638,30639,30640,30641,37981,30642,30643,38539,30644,30645,30646,30647,30648,30649,30650,30651,30580,30581) order by time_stamp",sep="")

print(query)

data<-dbGetQuery(con,query)

#data=data[data$track_info_id%%2==0,]

data$track_info_id=as.factor(data$track_info_id)
data$time=as.POSIXct(data$time_stamp/1000, origin="1970-01-01")


p <- ggplot(data, aes(x=time, y=distance_along_road, group=track_info_id, colour = track_info_id))
p + geom_point() +geom_line()

#+ geom_text(aes(label=time),hjust=0, vjust=0)


#Close Database connection
dbDisconnect(con)


