df=read.csv("C:/Users/abhinav.sunderrajan/Desktop/piejurong_on_weekdays_byday.csv",sep="\t")

library(ggplot2)
summary(df)

pd <- position_dodge(.1)
g<-ggplot(aes(x=TIME,y=STDDEV_SPEED,col=DAY),data=subset(df,DAY>3 & DAY<9))+
  labs(x="Time", y="Stddev Speed", title="Stddev speed vs Time")
g<-g+geom_line(position=pd,aes(group=DAY),size=1)+scale_colour_gradient(low="red",high="blue",space = "Lab")+ 
  geom_point(position=pd,size=3)+ylim(0, 10)+
  theme(axis.title = element_text(face="bold", colour="#990000", size=22),title=element_text(face="bold", size=25),
        axis.text = element_text(angle=90, vjust=0.5, size=20,face="bold"))+
scale_x_discrete(breaks=c("00:00-00:15", "04:00-04:15", "08:00-08:15","12:00-12:15","16:00-16:15","20:00-20:15","23:45-24:00"))

g
ggsave(g, file="C:/Users/abhinav.sunderrajan/Desktop/stddev_speed_vs_time.pdf", width=12, height=8)

df=read.csv("C:/Users/abhinav.sunderrajan/Desktop/piejurong_on_weekdays_Distance.csv",sep="\t")
road_end<-c(745,969,1410,2212,2793,3559,3922,4891,5342,5953,6154,6734,7579,8316,8934,9224,9675,10841,11556,11832)
pd <- position_dodge(.1)
g<-ggplot(aes(x=DISTANCE,y=AVG_SPEED,col=DAY),data=df)+
  labs(x="Distance", y="Avg Speed", title="Average speed vs distance")
g<-g+geom_line(position=pd,aes(group=DAY),size=1)+scale_colour_gradient(low="red",high="blue",space = "Lab")+ 
  geom_point(position=pd,size=3)+ylim(10, 22)+
  theme(axis.title = element_text(face="bold", colour="#990000", size=22),title=element_text(face="bold", size=25),
        axis.text = element_text(angle=90, vjust=0.5, size=20,face="bold"))+
  scale_x_discrete(breaks=road_end)

g

ggsave(g, file="C:/Users/abhinav.sunderrajan/Desktop/avg_speed_vs_distance_by_day.pdf", width=14, height=8)

