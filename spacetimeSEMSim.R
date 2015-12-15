library(RPostgreSQL)
library(ggplot2)
require(gridExtra)
library(rgl)
library("reshape2")
library("plot3D")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')

heatMapList <- vector("list", length = 12)
heatMapAccidentList<-vector("list", length = 3)


iterAcc=1
iterNoAcc=1

light=c( 2, 5, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
medium=c(1, 4, 8, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)
heavy<-c(3, 6, 9, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45)



for(iter in heavy){
  query<-paste(" select time_stamp ,distance_along_road,speed,is_accident from semsim_output WHERE iteration_count="
               ,iter," AND time_stamp <=7200 AND agent_id=0 AND distance_along_road<=13000", sep="")

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
  
  
  
  df<-data.frame(distance_interval, time_interval,median_speed,stringsAsFactors=FALSE)  
  heatMap<-acast(df, time_interval~distance_interval, value.var="median_speed")
  
  heatMap <- heatMap[, order(as.integer(colnames(heatMap)))]
  if(data[1,4]==TRUE){
     heatMapAccidentList[[iterAcc]]<-heatMap
     iterAcc<-iterAcc+1
  }else{
    heatMapList[[iterNoAcc]]<-heatMap
    iterNoAcc<-iterNoAcc+1
  }

}

heatMapAvg<-matrix(0,24,130)
heatMapSd<-matrix(0,24,130)


#Compare the RMSD for all the stochastic simulations

for(i in 1:6){
  heatMapAvg=heatMapAvg+heatMapList[[i]]
}

heatMapAvg<-heatMapAvg/6

for(i in 1:6){
  heatMapSd=heatMapSd+(heatMapAvg-heatMapList[[i]])*(heatMapAvg-heatMapList[[i]])
  
}

heatMapSd=sqrt(heatMapSd/6)


for(i in 1:6){
  rmse<-sum((heatMapAvg-heatMapList[[i]])*(heatMapAvg-heatMapList[[i]]),na.rm=TRUE)
  print(sqrt(rmse/(130*24)))
}





rmse_average <- vector("numeric", length = 9)
is_accident <- vector("logical", length = 9)

for(i in 7:12){
  rmseMat<-sqrt((heatMapAvg-heatMapList[[i]])*(heatMapAvg-heatMapList[[i]]))
  rmse_average[i-6]<-mean(rmseMat,na.rm=TRUE)
  is_accident[i-6]<-FALSE
  sum(is.na(heatMapList[[i]]))
}

for(i in 1:3){
  rmseMat<-sqrt((heatMapAvg-heatMapAccidentList[[i]])*(heatMapAvg-heatMapAccidentList[[i]]))
  rmse_average[i+6]<-mean(rmseMat,na.rm=TRUE)
  is_accident[i+6]<-TRUE
  sum(is.na(heatMapAccidentList[[i]]))
}


results<-data.frame(rmse_average,is_accident,"iter"=seq(1,9,by=1))

g<-ggplot(results) + geom_point(size=3,aes(x=iter, y=rmse_average, group=is_accident,shape=is_accident,color=is_accident))+
    labs(x="Iteration", y="RMSE")+scale_colour_discrete(name  ="ACCIDENT") +
  scale_shape_discrete(name  ="ACCIDENT")+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),
                                                legend.title=element_text(size=14)
                                                ,axis.title=element_text(size=24), axis.title.y=element_text(vjust=0.25))+
  scale_x_discrete(breaks=seq(1,9,by=1),labels=seq(1,9,by=1))
g


diff<-sqrt((heatMapAvg-heatMapAccidentList[[1]])*(heatMapAvg-heatMapAccidentList[[1]]))
mean_diff<-mean(diff,na.rm=TRUE)
sd_diff<-sd(diff,na.rm=TRUE)

diff<-(diff-mean_diff)/sd_diff


image2D(heatMapAccidentList[[1]],clab="speed (m/s)",axes = FALSE, xlab = "Time (h)", ylab = "Distance (km)",
        shade = 0.2 ,rasterImage = TRUE,cex.axis = 1.1,cex.lab=1.2,xaxs = "i", yaxs = "i",zlim=c(0,20))

axis(1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels=c(0,0.4,0.8,1.2,1.6,2.0))
axis(2, at=c(0,0.2,0.4,0.6,0.8,1.0), labels=c(0,2.6,5.2,7.8,10.4,13.0))

  time_acc=1
  dist_acc=-1

  
  for(time in seq_len(nrow(diff))) {
    if(time>nrow(diff)-2)
        break;
     cor_diff1<- cor(diff[time+1,], diff[time,],use="complete")
     
        if(cor_diff1<0.3){    
          cor_diff2<- cor(diff[time+2,], diff[time,],use="complete")
          cor_diff3<- cor(diff[time+2,], diff[time+1,],use="complete")
          if(cor_diff2<0.3 && cor_diff3>0.5){
            time_acc<-time+1
          }
        }

  }


bad <- is.na(diff[time_acc,])
y<-diff[time_acc,][!bad]

wss<-c(0,0,0,0)
for (i in 1:4) wss[i] <- sum(kmeans(y, 
                                     centers=i+1)$withinss)
plot(2:5, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

clustered<-kmeans(y,centers=2,iter.max=10)
plot(y,col=clustered$cluster)
x<-data.frame(cbind(names(y),y,clustered$cluster))
subset<-filter(x,V3==2)




print(paste("Time of accident:",time_acc,sep=" -- "))
 print(paste("Place of accident:",dist_acc,sep=" -- "))









