library(RPostgreSQL)
library(ggplot2)
require(gridExtra)
library(rgl)
library("reshape2")
library("plot3D")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')



  query<-paste(" select time_stamp ,distance_along_road,speed,is_accident from semsim_output WHERE iteration_count="
               ,54," AND time_stamp <=7200 AND  distance_along_road<=13000", sep="")


distBreaks=c(0.0, 745.13, 1441.13, 2907.97, 4645.05, 6001.21, 7184.86, 7741.78, 8300.75, 11445.9, 11794.3 )
distLabels=c( 0.0, 745.13, 1441.13, 2907.97, 4645.05, 6001.21, 7184.86, 7741.78, 8300.75,11445.9)


  
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
    c2<-cut(y$time_stamp,breaks =  seq(0, 7200, by = 100),labels = seq(0, 7100, by = 100))
    time_split<-split(y,c2)
    for(name_time in names(time_split)){
      speed<-mean(time_split[[name_time]]$speed)
      median_speed[index] <-speed
      distance_interval[index] <- as.numeric(name_dist)
      time_interval[index] <- as.numeric(name_time)
      index<-index+1    
      
    }
    
    
  }
  
  
  
  df<-data.frame(distance_interval, time_interval,median_speed, stringsAsFactors=FALSE)  
  heatMap<-acast(df, time_interval~distance_interval, value.var="median_speed")
  heatMap <- heatMap[, order(as.integer(colnames(heatMap)))]


#par(col="white",bg="black", col.axis="white", col.lab="white")

image2D(heatMap,clab="speed (m/s)",axes = FALSE, xlab = "Time (h)", ylab = "Distance (km)",
        shade = 0.2 ,rasterImage = TRUE,cex.axis = 1.1,cex.lab=1.2,xaxs = "i", yaxs = "i",zlim=c(0,20))

axis(1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels=c(0,0.4,0.8,1.2,1.6,2.0))
axis(2, at=c(0,0.2,0.4,0.6,0.8,1.0), labels=c(0,2.6,5.2,7.8,10.4,13.0))


dbDisconnect(con)



