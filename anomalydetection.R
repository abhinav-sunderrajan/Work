library(AnomalyDetection)
library(ggplot2)
help(AnomalyDetectionTs)
data(raw_data)
summary(raw_data)

pd <- position_dodge(.1)
g<-ggplot(aes(x=timestamp,y=count,ymax=max(count)*1.05),data=raw_data)+
  labs(x="Time", y="count", title="Count vs Time")
g<-g+geom_line(position=pd,size=1,colour = "#4a6084")+
  geom_point(position=pd,size=2,,colour = "#4a6084")+ylim(21, 255)+
  theme_bw()

g
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot
