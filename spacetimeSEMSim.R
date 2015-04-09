library(RPostgreSQL)
library(ggplot2)
require(gridExtra)
library(rgl)
library("reshape2")
library("plot3D")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')
query<-"select time_stamp ,distance_along_road,speed from semsim_output WHERE time_stamp <=7200 AND  distance_along_road<=13000"
data<-dbGetQuery(con,query)
summary(data)

c1<-cut(data$distance_along_road,breaks = seq(0, 13000, by = 100),labels = seq(0, 12900, by = 100))
distance_split<-split(data,c1)

median_speed <- numeric()
distance_interval <- numeric()
time_interval <- numeric()
index=1

dist_index=0;
time_index=0;



for(name_dist in names(distance_split)){
  y<-distance_split[[name_dist]]
  c2<-cut(y$time_stamp,breaks =  seq(0, 7200, by = 300),labels = seq(0, 6900, by = 300))
  time_split<-split(y,c2)
  for(name_time in names(time_split)){
       speed<-median(time_split[[name_time]]$speed)
      median_speed[index] <-speed
       distance_interval[index] <- as.numeric(name_dist)
       time_interval[index] <- as.numeric(name_time)
       index<-index+1    
    
    }
  
  
}



df<-data.frame(distance_interval, time_interval,median_speed, stringsAsFactors=FALSE)
summary(df)



heatMap<-acast(df, time_interval~distance_interval, value.var="median_speed")
heatMap <- heatMap[, order(as.integer(colnames(heatMap)))]
image2D(heatMap,clab="speed",axes = TRUE, xlab = "Time", ylab = "Distance",
        main = "V(x,t) PIE at 1% probe vehicles",
        shade = 0.2, rasterImage = TRUE)

dbDisconnect(con)



