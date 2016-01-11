library(RPostgreSQL)
library(reshape2)
library(ggplot2)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')
data<- dbGetQuery(con,"select * from map_matched_all where road_id IN(15267,15268,15269,15270,15271,15272,15273) AND segment<>-1
and is_holiday=false and hour in (8,9)")
 summary(data)
 
g<-ggplot(data,aes(x=disruption_distance_next,y=speed,ymax=max(speed)*1.05,colour=factor(road_id),group=factor(road_id)))+
  labs(x="disruption_distance_next", y="speed", title="speed vs distance at peak hours")
g<-g+geom_line(size=1)+geom_point(size=3)
g


data_time<-dbGetQuery(con,"select avg(speed) as avg_speed,stddev(speed) as stddev_speed,count(*) as counts,hour from map_matched_all 
where road_id in(3911,3912,3913,3914,3915,3916,3917,3918,18081,41453) 
and is_holiday=false group by hour having count(*)>15 order by hour")

attach(data_time)


g<-ggplot(data_time, aes(hour,label=counts)) + 
  geom_line(aes(y = avg_speed, colour = "avg_speed"),size=1) + 
  geom_line(aes(y = stddev_speed, colour = "stddev_speed"),size=1)+
  geom_point(size=3,aes(y = avg_speed))+geom_point(size=3,aes(y = stddev_speed))+
  geom_text(aes(y = avg_speed,label=counts),hjust=0, vjust=0)+
  geom_text(aes(y = stddev_speed,label=counts),hjust=0, vjust=0)+theme(legend.position = "bottom")+theme_bw()

g

wss <- (nrow(data_time)-1)*var(data_time$avg_speed)
wss
for (i in 2:15) wss[i] <- sum(kmeans(avg_speed,
                                     centers=i,iter.max = 30, nstart = 1)$withinss)
wss
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

kmeans_speed<-kmeans(avg_speed,centers=3,iter.max = 20, nstart = 1)
kmeans_speed$cluster
kmeans_speed$withinss

plot(avg_speed, col = kmeans_speed$cluster)

split_cluster=split(data_time,f=kmeans_speed$cluster)
split_cluster

#non-peak hours (11,20,5,6,21,22,14,15,23,12)

#clementi road morning peak roads (2270, 2271, 2272, 2273, 2274, 2275, 2276, 2278, 2279, 2283)
#clementi road evening peak roads  (16749,16750,16751,16752)
