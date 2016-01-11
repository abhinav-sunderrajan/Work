library(RPostgreSQL)
library(ggplot2)
require(gridExtra)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')
data<- dbGetQuery(con,"SELECT * FROM express_way_groupings WHERE road_name LIKE 'P.I.E%'")
summary(data)


f <- function(x) {
  query=paste("select speed,distance_along_road from map_matched_express_ways where road_id in", x[2], " and segment<>-1 and hour in (17,18) and is_holiday=FALSE")
  data_distance<-dbGetQuery(con,query)
  name=paste(x[1],x[3],sep=" ")
  distance_disruption<-as.numeric(unlist(strsplit(x[4], ","),use.names = FALSE))
  type_disruption<-unlist(strsplit(x[5], ","),use.names = FALSE)
  label<-paste(distance_disruption,type_disruption,sep=" ")
      
  g<- ggplot(data_distance, aes(x = distance_along_road, y = speed))+
    geom_point(colour ="#990000",alpha=1/10)+scale_x_continuous(breaks=distance_disruption,labels=label)+
    theme(axis.text.x  = element_text(angle=45, vjust=0.5))+
    labs(x="distance", y="speed", title=paste("speed vs distance for ",name))
  g
}


plots<-apply(data, 1, f)

n <- length(plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plots, ncol=nCol))


ggsave("pie-distance-17-to-18.pdf",width=12, height=7, do.call(marrangeGrob, c(plots, list(nrow=2, ncol=1))))

