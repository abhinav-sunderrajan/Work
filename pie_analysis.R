library(RPostgreSQL)
library(ggplot2)





drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')
data<- dbGetQuery(con,"select a.road_id,b.fullname,b.town,a.hour,count(*) as count,avg(speed) as speed 
from map_matched_all as a inner join qi_roads as b ON
a.road_id=b.road_id where b.road_class=0 and is_holiday=false 
                  group by a.road_id,b.fullname,hour,b.town")
summary(data)


split_name=split(data,f=data$fullname)
typeof(split_name)
require(gridExtra)

plots=list() 

for(name in names(split_name)){ 
  medians <- aggregate(dat=split_name[[name]], speed~fullname+town+hour, FUN=median)
  counts <- aggregate(dat=split_name[[name]],count~fullname+town+hour, FUN=sum)
  total <- merge(medians,counts,by=c("hour","town"))

  g<-ggplot(data=total,aes(hour,label=count,ymax=max(speed)*1.05,colour=factor(town),group=factor(town)))+
    labs(x="hour", y="median speed", title=paste("speed vs hour for ",name))+
    geom_text(aes(y = speed,label=count),hjust=0, vjust=0)+theme_bw()
  g<-g+geom_line(aes(y = speed),size=1)+geom_point(aes(y = speed),size=3)
  plots[[name]]<-g
}

length(plots)
n <- length(plots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plots, ncol=nCol))


ggsave("arrange2x1.pdf", do.call(marrangeGrob, c(plots, list(nrow=2, ncol=1))))




