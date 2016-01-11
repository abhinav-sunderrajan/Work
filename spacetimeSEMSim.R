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
distBreaks=c(24.0, 40.0, 56.0, 152.0, 160.0, 168.0, 272.0, 280.0, 288.0, 400.0, 552.0, 736.0, 744.0, 752.0, 808.0, 816.0, 856.0, 864.0, 872.0, 880.0, 920.0, 984.0, 992.0, 1008.0, 1016.0, 1024.0, 1032.0, 1176.0, 1184.0, 1280.0, 1288.0, 1304.0, 1312.0, 1328.0, 1336.0, 1344.0, 1352.0, 1360.0, 1440.0, 1448.0, 1456.0, 1464.0, 1472.0, 1496.0, 1800.0, 1832.0, 1912.0, 1992.0, 2000.0, 2080.0, 2088.0, 2096.0, 2128.0, 2136.0, 2144.0, 2152.0, 2248.0, 2288.0, 2312.0, 2416.0, 2432.0, 2440.0, 2448.0, 2544.0, 2560.0, 2568.0, 2576.0, 2584.0, 2592.0, 2600.0, 2608.0, 2728.0, 2744.0, 2752.0, 2768.0, 2864.0, 2888.0, 2896.0, 2904.0, 2912.0, 2976.0, 3032.0, 3040.0, 3048.0, 3136.0, 3176.0, 3184.0, 3416.0, 3504.0, 3520.0, 3536.0, 3632.0, 3640.0, 3648.0, 3656.0, 3776.0, 3784.0, 3896.0, 3904.0, 3912.0, 3920.0, 4032.0, 4040.0, 4048.0, 4144.0, 4160.0, 4248.0, 4256.0, 4264.0, 4296.0, 4400.0, 4408.0, 4528.0, 4536.0, 4592.0, 4648.0, 4664.0, 4672.0, 4688.0, 4768.0, 4776.0, 4784.0, 4792.0, 4800.0, 4808.0, 4824.0, 4832.0, 4904.0, 4912.0, 4952.0, 4960.0, 5056.0, 5080.0, 5128.0, 5296.0, 5304.0, 5360.0, 5504.0, 5512.0, 5528.0, 5536.0, 5544.0, 5624.0, 5656.0, 5720.0, 5776.0, 5880.0, 5888.0, 5936.0, 6000.0, 6008.0, 6120.0, 6184.0, 6240.0, 6352.0, 6360.0, 6472.0, 6480.0, 6488.0, 6584.0, 6592.0, 6600.0, 6616.0, 6648.0, 6728.0, 6736.0, 6744.0, 6840.0, 6856.0, 6920.0, 6968.0, 7080.0, 7304.0, 7408.0, 7520.0, 7632.0, 7744.0, 7856.0, 7968.0, 8080.0, 8192.0, 8304.0, 8416.0, 8488.0, 8528.0, 8616.0, 8648.0, 8760.0, 8768.0, 8856.0, 8880.0, 8888.0, 8968.0, 9000.0, 9072.0, 9200.0, 9232.0, 9336.0, 9344.0, 9352.0, 9472.0, 9576.0, 9584.0, 9680.0, 9800.0, 9808.0, 9832.0, 9912.0, 9920.0, 9936.0, 9952.0, 9960.0, 9992.0, 10064.0, 10080.0, 10096.0, 10104.0, 10184.0, 10240.0, 10264.0, 10272.0, 10280.0, 10448.0, 10456.0, 10464.0, 10472.0, 10488.0, 10568.0, 10584.0, 10592.0, 10608.0, 10624.0, 10640.0, 10648.0, 10664.0, 10760.0, 10776.0, 10792.0, 10808.0, 10816.0, 10832.0, 10928.0, 10968.0, 10992.0, 11056.0, 11104.0, 11240.0, 11336.0, 11392.0, 11448.0, 11520.0, 11600.0, 11688.0, 11808.0, 11816.0, 11928.0, 12032.0, 12048.0, 12096.0, 12160.0, 12200.0, 12272.0, 12384.0, 12592.0, 12696.0, 12728.0, 12840.0)
distLabels=c(24.0, 40.0, 56.0, 152.0, 160.0, 168.0, 272.0, 280.0, 288.0, 400.0, 552.0, 736.0, 744.0, 752.0, 808.0, 816.0, 856.0, 864.0, 872.0, 880.0, 920.0, 984.0, 992.0, 1008.0, 1016.0, 1024.0, 1032.0, 1176.0, 1184.0, 1280.0, 1288.0, 1304.0, 1312.0, 1328.0, 1336.0, 1344.0, 1352.0, 1360.0, 1440.0, 1448.0, 1456.0, 1464.0, 1472.0, 1496.0, 1800.0, 1832.0, 1912.0, 1992.0, 2000.0, 2080.0, 2088.0, 2096.0, 2128.0, 2136.0, 2144.0, 2152.0, 2248.0, 2288.0, 2312.0, 2416.0, 2432.0, 2440.0, 2448.0, 2544.0, 2560.0, 2568.0, 2576.0, 2584.0, 2592.0, 2600.0, 2608.0, 2728.0, 2744.0, 2752.0, 2768.0, 2864.0, 2888.0, 2896.0, 2904.0, 2912.0, 2976.0, 3032.0, 3040.0, 3048.0, 3136.0, 3176.0, 3184.0, 3416.0, 3504.0, 3520.0, 3536.0, 3632.0, 3640.0, 3648.0, 3656.0, 3776.0, 3784.0, 3896.0, 3904.0, 3912.0, 3920.0, 4032.0, 4040.0, 4048.0, 4144.0, 4160.0, 4248.0, 4256.0, 4264.0, 4296.0, 4400.0, 4408.0, 4528.0, 4536.0, 4592.0, 4648.0, 4664.0, 4672.0, 4688.0, 4768.0, 4776.0, 4784.0, 4792.0, 4800.0, 4808.0, 4824.0, 4832.0, 4904.0, 4912.0, 4952.0, 4960.0, 5056.0, 5080.0, 5128.0, 5296.0, 5304.0, 5360.0, 5504.0, 5512.0, 5528.0, 5536.0, 5544.0, 5624.0, 5656.0, 5720.0, 5776.0, 5880.0, 5888.0, 5936.0, 6000.0, 6008.0, 6120.0, 6184.0, 6240.0, 6352.0, 6360.0, 6472.0, 6480.0, 6488.0, 6584.0, 6592.0, 6600.0, 6616.0, 6648.0, 6728.0, 6736.0, 6744.0, 6840.0, 6856.0, 6920.0, 6968.0, 7080.0, 7304.0, 7408.0, 7520.0, 7632.0, 7744.0, 7856.0, 7968.0, 8080.0, 8192.0, 8304.0, 8416.0, 8488.0, 8528.0, 8616.0, 8648.0, 8760.0, 8768.0, 8856.0, 8880.0, 8888.0, 8968.0, 9000.0, 9072.0, 9200.0, 9232.0, 9336.0, 9344.0, 9352.0, 9472.0, 9576.0, 9584.0, 9680.0, 9800.0, 9808.0, 9832.0, 9912.0, 9920.0, 9936.0, 9952.0, 9960.0, 9992.0, 10064.0, 10080.0, 10096.0, 10104.0, 10184.0, 10240.0, 10264.0, 10272.0, 10280.0, 10448.0, 10456.0, 10464.0, 10472.0, 10488.0, 10568.0, 10584.0, 10592.0, 10608.0, 10624.0, 10640.0, 10648.0, 10664.0, 10760.0, 10776.0, 10792.0, 10808.0, 10816.0, 10832.0, 10928.0, 10968.0, 10992.0, 11056.0, 11104.0, 11240.0, 11336.0, 11392.0, 11448.0, 11520.0, 11600.0, 11688.0, 11808.0, 11816.0, 11928.0, 12032.0, 12048.0, 12096.0, 12160.0, 12200.0, 12272.0, 12384.0, 12592.0, 12696.0, 12728.0)
iter1<-c(35)


for(iter in iter1){
  query<-paste("select time_stamp ,distance_along_road,speed,is_accident from semsim_output WHERE iteration_count="
               ,iter," AND time_stamp <=7200 AND distance_along_road<=13000", sep="")
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
    c2<-cut(y$time_stamp,breaks =  seq(0, 7200, by = 120),labels = seq(0, 7080, by = 120))
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

heatMapAvg<-matrix(0,60,130)
heatMapSd<-matrix(0,60,130)


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


image2D(heatMapList[[1]],clab="speed (m/s)",axes = FALSE, xlab = "Time (h)", ylab = "Distance (km)",
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









