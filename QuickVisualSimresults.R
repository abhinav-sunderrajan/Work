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
               ,50," AND time_stamp <=7200 AND  distance_along_road<=13000", sep="")


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


#Rough Work computations

time_acc=1
dist_acc=-1



for(time in seq_len(nrow(heatMap))) {
  if(time<3)
      next;
  if(time>nrow(heatMap)-2)
    break;
  dist1<-mean(sqrt((heatMap[time+1,70:75]- heatMap[time,70:75])*(heatMap[time+1,70:75]- heatMap[time,70:75])),na.rm=TRUE)
  dist2<-mean(sqrt((heatMap[time+2,70:75]- heatMap[time,70:75])*(heatMap[time+2,70:75]- heatMap[time,70:75])),na.rm=TRUE)
  a[iter]<-dist1
  b[iter]<-dist2
  iter<-iter+1
}

x<-c(1)
for(i in 1:44){
  if(i<23)
  x[i]<-1
else
  x[i]<-2
}




bluebands<-list("7 to 7.5 km"=c(70:75),"8.1 to 8.3 km"=c(81,83),"11.8 to 12.0 km"=c(118:120),"2.8 to 3.1 km"=c(28,31))
list_bands <- vector("list", length = 4)

i<-1


for(band in bluebands){
  
  a<-c(0)
  
  iter=1
  
  for(time in seq_len(nrow(heatMapLight))) {
    if(time<3)
      next;
   
    dist1<-mean(sqrt((heatMapHeavy[time,band]- heatMapLight[time,band])*(heatMapHeavy[time,band]- heatMapLight[time,band])),na.rm=TRUE)
    a[iter]<-dist1
    iter<-iter+1
  }
  print(a)
  list_bands[[i]]<-a
  i<-i+1
 
}


timeaxis<-seq(15,120,by=5)
colors<-c("blue","green","red","orange")
i=1
plot(0,0,xlim = c(10,120),ylim = c(2,12),type = "n",xlab="Time (min)", ylab="RMSD",cex.axis = 1.0,cex.lab=1.2)


for(rmse in list_bands){
  
    lines(timeaxis,rmse,type="b",col=colors[i])
    i=i+1
  
}


legend('bottomleft', names(bluebands) , 
       lty=1, col=colors, bty='n', cex=0.95)


dbDisconnect(con)



