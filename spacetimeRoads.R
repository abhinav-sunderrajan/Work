library(RPostgreSQL)
library(ggplot2)
require(gridExtra)
library(rgl)
library("reshape2")
library("plot3D")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')

days<- dbGetQuery(con,"select distinct month,day from map_matched_express_ways where is_holiday =FALSE and month<>10 order by month,day")
summary(days)
distance_disruption=c(583.98,1973.35,2489.87,3261.27,4071.9,4834.84,5531.18,5743.11,5965.29,6207.74,7025.15,7658.4,8040.83,8554.28,8807.94,9591.84,10148.24,11286.2,11637.04,12438.23,13328.8)

median_speed <- numeric()
distance_interval <- character()
minute_interval <- character()
month<-numeric()
day<-numeric()
index=1


f <- function(x) {

query<-paste("select speed,to_char(hour, '09')||':'||to_char(minute, '09') as time,minute,distance_along_road from map_matched_express_ways where road_id in(30634, 30635, 30636, 38541, 30637, 30638, 30639, 30640, 30641, 37981,30642, 30643, 38539, 30644, 30645, 30646, 30647, 30648, 30649, 30650, 30651,30580, 30581) and segment<>-1 and month=",x[1], "and day=",x[2], "and hour in(18,19) and is_holiday=FALSE order by hour,minute,distance_along_road",sep=" ")
data<-dbGetQuery(con,query)
data$date<-strptime(paste(x[1],x[2],"2014",data$time,sep=" "),"%m %d %Y %H:%M")
c1<-cut(data$distance_along_road,breaks = seq(0, 13000, by = 100),labels = seq(0, 12900, by = 100))

distance_split<-split(data,c1)
data_breaks<-strptime(c(paste(x[1],x[2],"2014 18:00",sep=" "),paste(x[1],x[2],"2014 18:05",sep=" ")
                        ,paste(x[1],x[2],"2014 18:10",sep=" "),paste(x[1],x[2],"2014 18:15",sep=" ")
                        ,paste(x[1],x[2],"2014 18:20",sep=" "),paste(x[1],x[2],"2014 18:25",sep=" ")
                        ,paste(x[1],x[2],"2014 18:30",sep=" "),paste(x[1],x[2],"2014 18:35",sep=" ")
                        ,paste(x[1],x[2],"2014 18:40",sep=" "),paste(x[1],x[2],"2014 18:45",sep=" ")
                        ,paste(x[1],x[2],"2014 18:50",sep=" "),paste(x[1],x[2],"2014 18:55",sep=" ")
                        ,paste(x[1],x[2],"2014 19:00",sep=" "),paste(x[1],x[2],"2014 19:05",sep=" ")
                        ,paste(x[1],x[2],"2014 19:10",sep=" "),paste(x[1],x[2],"2014 19:15",sep=" ")
                        ,paste(x[1],x[2],"2014 19:20",sep=" "),paste(x[1],x[2],"2014 19:25",sep=" ")
                        ,paste(x[1],x[2],"2014 19:30",sep=" "),paste(x[1],x[2],"2014 19:35",sep=" ")
                        ,paste(x[1],x[2],"2014 19:40",sep=" "),paste(x[1],x[2],"2014 19:45",sep=" ")
                        ,paste(x[1],x[2],"2014 19:50",sep=" "),paste(x[1],x[2],"2014 19:55",sep=" ")
                        ,paste(x[1],x[2],"2014 20:00",sep=" ")),"%m %d %Y %H:%M")

time_distance_split<-sapply(distance_split,function(y){
  c2<-cut(y$date,breaks = data_breaks,labels = FALSE)
  time_split<-split(y,c2)
  #print(time_split)
  })


for(name_dist in names(time_distance_split)){
  time_split<-time_distance_split[[name_dist]]
  for(name_time in names(time_split)){
    speed<-median(time_split[[name_time]]$speed)
    median_speed[index] <<- speed
    distance_interval[index] <<- name_dist
    minute_interval[index] <<- name_time
    day[index]<<-x[2]
    month[index]<<-x[1]
    index<<-index+1    
  }
  
}
}

apply(days, 1, f)
df<-data.frame(day,month,distance_interval, minute_interval,median_speed, stringsAsFactors=FALSE)
summary(df)
#Remove NA speed values.
good <- complete.cases(df)
vxt<-df[good,]

pdf(file='qi_plot2.pdf',paper="a4r")
par(mar=c(5,7,2,2)+.1)
f <- function(x) {
  date<-paste(x[2],x[1],"2014",sep="/")
  print(date)
  
  vxt_15_Sep<-subset(vxt,day==x[2] & month==x[1])    
  mat_vxt_15_Sep<-acast(vxt_15_Sep, minute_interval~distance_interval, value.var="median_speed")
  mat_vxt_15_Sep <- mat_vxt_15_Sep[, order(as.integer(colnames(mat_vxt_15_Sep)))]
 
  main_heading<-paste("P.I.E(Changi)",date,"18:00-20:00 hours",sep=" ")
  
  p<-image2D(mat_vxt_15_Sep,clab="speed (m/s)",axes = FALSE, xlab = "Time (h)", ylab = "Distance (km)",
          main = main_heading,
          shade = 0.2 ,rasterImage = TRUE,cex.axis = 1.4,cex.lab=2.0,xaxs = "i", yaxs = "i")
  
  axis(1, at=c(0,0.2,0.4,0.6,0.8,1.0), labels=c("18:00","18:24","18:56","19:12","19:36","20:00"))
  axis(2, at=c(0,0.2,0.4,0.6,0.8,1.0), labels=c(0,2.6,5.2,7.8,10.4,13.0))
  p
}


plots<-apply(days, 1, f)
dev.off()










