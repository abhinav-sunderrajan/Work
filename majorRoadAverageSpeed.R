library(RPostgreSQL)
library(ggplot2)
require(gridExtra)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')
data<- dbGetQuery(con,"SELECT * FROM express_way_groupings WHERE road_name LIKE 'P.I.E (Changi)%' AND town='CENTRAL'")
summary(data)
index<-1

median_speed <- numeric()
stddev_speed <- numeric()
counts <- numeric()
hour <- numeric()

for(i in 0:23){
  query=paste("select speed from map_matched_express_ways where road_id in",data$road_id_list[1], " and is_holiday=false and hour=",i)
  print(query)
  speeds<-dbGetQuery(con,query)
  median_speed[index] <-median(speeds$speed)
  stddev_speed[index]<-sd(speeds$speed)
  counts[index]<-nrow(speeds)
  hour[index]<-i
  index<-index+1
  
}

df<-data.frame(median_speed, stddev_speed,counts, hour,stringsAsFactors=FALSE)
summary(df)

g<-ggplot(df, aes(hour,label=counts)) + 
  geom_line(aes(y = median_speed, colour = "median speed"),size=1.1) + 
  geom_line(aes(y = stddev_speed, colour = "stddev speed"),size=1.1)+
  geom_point(size=3,aes(y = median_speed, colour = "median speed"))+geom_point(size=3,aes(y = stddev_speed,colour = "stddev speed"))+
  geom_text(aes(y = median_speed,label=counts, colour = "median speed"),hjust=1.0, vjust=1.5)+
  #scale_fill_discrete(name="legend",breaks=c("median_speed", "stddev_speed"),labels=c("Median Speed", "Stddev speed"))+
  theme(legend.title=element_blank(),legend.position="bottom",
        axis.title.x = element_text( size=25, vjust=0.5),axis.title.y = element_text(size=25, vjust=0.25),
        axis.text.x  = element_text( vjust=0.5, size=16),axis.text.y  = element_text( vjust=0.5, size=16),
        legend.text = element_text( size = 20))+
  labs(x="Hour", y="Median and Stddev speed", title="")
g






#plots<-apply(data, 1, f)

n <- length(plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plots, ncol=nCol))


ggsave("pie-avg-speed.pdf",width=12, height=7, do.call(marrangeGrob, c(plots, list(nrow=2, ncol=1))))

