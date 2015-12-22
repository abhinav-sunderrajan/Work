library(RPostgreSQL)
library(ggplot2)
require(gridExtra)
library("plot3D")
require(tree)
library(caret)
library(partykit)
library("reshape2")
library(rpart)
library(doParallel)

set.seed(2123)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')

time1=1000
time2=1150

query<-paste("select distance_along_road as distance,speed,time_stamp from semsim_output WHERE iteration_count=47 AND  (agent_id%1=0) AND time_stamp >=",time1, " AND time_stamp <=",time2," AND distance_along_road<=13000", sep="")
training<-dbGetQuery(con,query)
penetration=1.0

#decisonTreeSplit=c(0.000,  2494.460,  7017.720,  7518.765,  7965.355, 10918.090)


#Decision tree rpart

modelFit <- rpart(speed ~ distance,data=training,control=rpart.control(maxdepth=14))
#rpart1a <- as.party(modelFit)
#plot(rpart1a)

pred=predict(modelFit)
mse_rpart=mean((training$speed-pred)^2)

printcp(modelFit)
plotcp(modelFit)

index=1
decisonTreeSplit=numeric()
for(split in modelFit$splits[,4]){
  decisonTreeSplit[index]=split
  index=index+1
}

decisonTreeSplit<-sort(decisonTreeSplit)
decisonTreeSplit<-c(0.0,decisonTreeSplit)

#Bagging

treebag=train(speed~distance,data=training, method="treebag")
treesList=treebag$finalModel$mtrees

index=1
bagging_split=numeric()

for(split in treesList[[length(treesList)]]$btree$splits[,4]){
  bagging_split[index]=split
  index=index+1
}

bagging_split<-sort(c(0.0,bagging_split))


#PIE
pieBreaks=c(0.0,583.9831796057117, 1973.3525390927846, 2367.5948374569775,
            2489.872751163762, 3261.271616457807, 4071.903598144841, 4834.836586015808,
            5531.1844928841365, 5743.1141418953775, 5965.289275894134, 6207.740716730275,
            6670.222224060167, 7025.154292404613, 7658.39571005447, 8040.834612543026,
            8554.283981491059, 8807.936731242416, 9591.841494223643, 10148.238120947959,
            11286.20075545962, 11637.04476230792, 12438.227750939186, 13328.802132638793)




  segments<-c(50,100,1300,2600,5200)
  avg_speed_list=list()
  sd_speed_list=list()
  
  for(segment in segments){
    c1<-cut(training$distance,breaks = seq(0, 13000, by = segment),labels = seq(0, (13000-segment), by = segment))
    distance_split<-split(training,c1)
    avg_speed <- numeric()
    sd_speed<-numeric()
    index=1
    
    
    for(name_dist in names(distance_split)){
      y<-distance_split[[name_dist]]
      if(length(y$speed)==0){
        avg_speed[index] =NA
        sd_speed[index] =NA
      }else{
        avg_speed[index] =mean(y$speed,na.rm=TRUE)
        sd_speed[index] =sd(y$speed,na.rm=TRUE)
      }
      
      index=index+1
    }
    
    

    avg_speed_list[[paste(segment)]]=avg_speed
    sd_speed_list[[paste(segment)]]=sd_speed
  }



 



#CUTS PIE

cutPIE<-cut(training$distance,breaks = pieBreaks,labels = pieBreaks[1:length(pieBreaks)-1])
distance_split_pie<-split(training,cutPIE)
avg_speed_pie=numeric()
sd_speed_pie=numeric()

index=1
for(name_dist in names(distance_split_pie)){
  y<-distance_split_pie[[name_dist]]
  avg_speed_pie[index] =mean(y$speed,na.rm=TRUE)
  sd_speed_pie[index]=sd(y$speed,na.rm=TRUE)
  index=index+1
}

#CUTS DECISION TREE

cutDT<-cut(training$distance,breaks = decisonTreeSplit,labels = decisonTreeSplit[1:length(decisonTreeSplit)-1])
distance_split_dt<-split(training,cutDT)
avg_speed_dt=numeric()
sd_speed_dt=numeric()

index=1
for(name_dist in names(distance_split_dt)){
  y<-distance_split_dt[[name_dist]]
  avg_speed_dt[index] =mean(y$speed,na.rm=TRUE)
  sd_speed_dt[index]=sd(y$speed,na.rm=TRUE)
  index=index+1
}


#Cuts Bagging TREE


cutBagging<-cut(training$distance,breaks = bagging_split,labels = bagging_split[1:length(bagging_split)-1])
distance_split_bagging<-split(training,cutBagging)
avg_speed_bagging=numeric()
sd_speed_bagging=numeric()
counts_bagging=numeric()

index=1
for(name_dist in names(distance_split_bagging)){
  y<-distance_split_bagging[[name_dist]]
  avg_speed_bagging[index] =mean(y$speed,na.rm=TRUE)
  sd_speed_bagging[index]=sd(y$speed,na.rm=TRUE)
  counts_bagging[index]=length(y$speed)
  index=index+1
}


avg_sd<-c(mean(sd_speed_list[["50"]],na.rm=TRUE),mean(sd_speed_list[["100"]],na.rm=TRUE),
          mean(sd_speed_list[["1300"]]),mean(sd_speed_list[["2600"]]),mean(sd_speed_pie),mean(sd_speed_bagging))


#Density esimation decision tree
par(mfrow=c(1,1),mar=c(1,1,1,1), oma=c(3,3,3,3))
  
  ls_density=numeric()
  ls_speed=numeric()

  i=1

  for(name_dist in names(distance_split_dt)){
    y<-distance_split_dt[[name_dist]]
    c2<-cut(y$time_stamp,breaks =  seq(time1, time2, by = 1),labels = seq(time1, (time2-1), by = 1))
    time_split<-split(y,c2)
    density_vec=numeric()
    speed_vec=numeric()
    index=1
    for(name_time in names(time_split)){
      num_of_vehicles<-length(time_split[[name_time]]$speed)*(1/penetration)
      density_vec[index]=num_of_vehicles*1000.0/(decisonTreeSplit[i+1]-decisonTreeSplit[i])
      speed_vec[index]=mean(time_split[[name_time]]$speed)
      index=index+1
    }
    ls_density=c(ls_density,density_vec)
    ls_speed=c(ls_speed,speed_vec)
    i=i+1
    
  }

good=complete.cases(ls_speed,ls_density)
ls_speed=ls_speed[good]
ls_density=ls_density[good]

good=ls_speed>0
ls_speed=ls_speed[good]
ls_density=ls_density[good]


plot(ls_speed,ls_density,xlab="",ylab="",cex.axis = 1.50,cex.lab=1.5)
fit.decision.tree=lm(ls_density~log(ls_speed))
summary(fit.decision.tree)
points(ls_speed, predict(fit.decision.tree), col="red")
mtext("Speed (m/s)", 1, 2.5,cex=1.5)
mtext("Density (veh/km)", 2, 2.5,cex=1.5)

#Desnity PIE breaks
ls_density=numeric()
ls_speed=numeric()

i=1

for(name_dist in names(distance_split_pie)){
  y<-distance_split_pie[[name_dist]]
  c2<-cut(y$time_stamp,breaks = seq(time1, time2, by = 1),labels = seq(time1, (time2-1), by = 1))
  time_split<-split(y,c2)
  density_vec=numeric()
  speed_vec=numeric()
  index=1
  for(name_time in names(time_split)){
    num_of_vehicles<-length(time_split[[name_time]]$speed)*(1/penetration)
    density_vec[index]=num_of_vehicles*1000.0/(pieBreaks[i+1]-pieBreaks[i])
    speed_vec[index]=mean(time_split[[name_time]]$speed)
    index=index+1
  }
  ls_density=c(ls_density,density_vec)
  ls_speed=c(ls_speed,speed_vec)
  i=i+1
  
}
plot(ls_speed,ls_density,xlab="",ylab="",cex.axis = 1.50,cex.lab=1.5)
good=complete.cases(ls_speed,ls_density)

ls_speed=ls_speed[good]
ls_density=ls_density[good]

good=ls_speed>0
ls_speed=ls_speed[good]
ls_density=ls_density[good]
fit.pie=lm(ls_density~log(ls_speed))
summary(fit.pie)
points(ls_speed, predict(fit.pie), col="red")
mtext("Speed (m/s)", 1, 2.5,cex=1.5)
mtext("Density (veh/km)", 2, 2.5,cex=1.5)

#Desnity estimation 100 m segments


ls_density=numeric()
ls_speed=numeric()

c1<-cut(training$distance,breaks = seq(0, 13000, by = 100),labels = seq(0, (13000-100), by = 100))
distance_split<-split(training,c1)

i=1

for(name_dist in names(distance_split)){
  y<-distance_split[[name_dist]]
  c2<-cut(y$time_stamp,breaks =  seq(time1, time2, by = 1),labels = seq(time1, (time2-1), by = 1))
  time_split<-split(y,c2)
  density_vec=numeric()
  speed_vec=numeric()
  index=1
  for(name_time in names(time_split)){
    num_of_vehicles<-length(time_split[[name_time]]$speed)*(1/penetration)
    density_vec[index]=num_of_vehicles
    speed_vec[index]=mean(time_split[[name_time]]$speed)
    index=index+1
  }
  ls_density=c(ls_density,density_vec)
  ls_speed=c(ls_speed,speed_vec)
  i=i+1
  
}
plot(ls_speed,ls_density,xlab="",ylab="",cex.axis = 1.50,cex.lab=1.5)
good=complete.cases(ls_speed,ls_density)
ls_speed=ls_speed[good]
ls_density=ls_density[good]
fit.decision.tree=lm(ls_density~ls_speed+I(ls_speed^2))
summary(fit.decision.tree)
mtext("Speed (m/s)", 1, 2.5,cex=1.5)
mtext("Density (veh/km)", 2, 2.5,cex=1.5)





#Close Database connection
dbDisconnect(con)

