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


times1=c(4500)
times2=times1+120

penetrations=c(0.05,0.1,0.2,0.3,0.4,0.5,1.0)
method=c("Uniform","P.I.E","Decision Tree")
rSquareMat=matrix(0,length(method),length(penetrations))
rownames(rSquareMat)=method
colnames(rSquareMat)=penetrations

#PIE
pieBreaks=c(0.0,583.9831796057117, 1973.3525390927846, 2367.5948374569775,
            2489.872751163762, 3261.271616457807, 4071.903598144841, 4834.836586015808,
            5531.1844928841365, 5743.1141418953775, 5965.289275894134, 6207.740716730275,
            6670.222224060167, 7025.154292404613, 7658.39571005447, 8040.834612543026,
            8554.283981491059, 8807.936731242416, 9591.841494223643, 10148.238120947959,
            11286.20075545962, 11637.04476230792, 12438.227750939186, 13328.802132638793)

par(mfrow=c(1,1))



#Function for different segmentation methods


rSquare<-function(distance_split,time1,time2,penetration,splits){
  ls_density=numeric()
  ls_speed=numeric()
  
  i=1
  
  for(name_dist in names(distance_split)){
    y<-distance_split[[name_dist]]
    c2<-cut(y$time_stamp,breaks =  seq(time1, time2, by = 1),labels = seq(time1, (time2-1), by = 1))
    time_split<-split(y,c2)
    density_vec=numeric()
    speed_vec=numeric()
    index=1
    for(name_time in names(time_split)){
      
      num_of_vehicles<-length(unique(time_split[[name_time]]$agent_id))*(1/penetration)
      density_vec[index]=num_of_vehicles*1000.0/(splits[i+1]-splits[i])
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
  fit=lm(ls_density~log(ls_speed))
 
  
  if((time1==4500 & time2==4620) & penetration==1.0){
   plot(ls_speed,ls_density,xlab="",ylab="",cex.axis = 1.50,cex.lab=1.5,xlim=c(0,20),ylim=c(0,250))
    points(ls_speed, predict(fit), col="red")
    mtext("Speed (m/s)", 1, 2.5,cex=1.5)
   mtext("Density (veh/km)", 2, 2.5,cex=1.5)
  }
  summary(fit)$r.squared 
  
}


sdSplits<-function(distance_split,time1,time2,penetration,splits){
  ls_density=numeric()
  ls_speed=numeric()
  sd_speed_list=list()
  
  i=1
  
  for(name_dist in names(distance_split)){
    y<-distance_split[[name_dist]]
    c2<-cut(y$time_stamp,breaks =  seq(time1, time2, by = 60),labels = seq(time1, (time2-60), by = 60))
    time_split<-split(y,c2)
    density_vec=numeric()
    speed_vec=numeric()
    sd_speed=numeric()
    index=1
    for(name_time in names(time_split)){
      num_of_vehicles<-length(unique(time_split[[name_time]]$agent_id))*(1/penetration)
      density_vec[index]=num_of_vehicles*1000.0/(splits[i+1]-splits[i])
      speed_vec[index]=mean(time_split[[name_time]]$speed)
      sd_speed[index]=sd(time_split[[name_time]]$speed)
      index=index+1
    }
    ls_density=c(ls_density,density_vec)
    ls_speed=c(ls_speed,speed_vec)
    i=i+1
    sd_speed_list[[name_dist]]=c(sd_speed_list[[name_dist]],sd_speed)
  }
  
  sd_speed_list
  
}






for(i in 1:1){
  
  query<-paste("select distance_along_road as distance,speed,time_stamp,agent_id from semsim_output WHERE iteration_count=47  AND time_stamp >=",times1[i], " AND time_stamp <=",times2[i]," AND distance_along_road<=13000", sep="")
  data<-dbGetQuery(con,query)
  set.seed(i)
  
  
  for(penetration in penetrations){
    samples=sample(unique(data$agent_id),length(unique(data$agent_id))*penetration,replace=FALSE)
    training <- subset(data, agent_id %in% samples)
    
    #Decision tree rpart
    
    modelFit <- rpart(speed ~ distance,data=training,control=rpart.control(maxdepth=14))
    #rpart1a <- as.party(modelFit)
    #plot(rpart1a)
    
    index=1
    decisonTreeSplit=numeric()
    for(split in modelFit$splits[,4]){
      decisonTreeSplit[index]=split
      index=index+1
    }
    
    decisonTreeSplit<-sort(decisonTreeSplit)
    decisonTreeSplit<-c(0.0,decisonTreeSplit)
    
    #CUTS PIE
    
    cutPIE<-cut(training$distance,breaks = pieBreaks,labels = pieBreaks[1:length(pieBreaks)-1])
    distance_split_pie<-split(training,cutPIE)
    statPIE=rSquare(distance_split_pie,times1[i],times2[i],penetration,pieBreaks)
    rSquareMat["P.I.E",toString(penetration)]=rSquareMat["P.I.E",toString(penetration)]+statPIE
    
    #CUTS DECISION TREE
    
    cutDT<-cut(training$distance,breaks = decisonTreeSplit,labels = decisonTreeSplit[1:length(decisonTreeSplit)-1])
    distance_split_dt<-split(training,cutDT)
    statDT=rSquare(distance_split_dt,times1[i],times2[i],penetration,decisonTreeSplit)
    rSquareMat["Decision Tree",toString(penetration)]=rSquareMat["Decision Tree",toString(penetration)]+statDT
    
    #Uniform cuts
    segment_length=1000
    segment_breaks=seq(0, 13000, by = segment_length)
    c1<-cut(training$distance,breaks = segment_breaks,labels = seq(0, (13000-segment_length), by = segment_length))
    distance_split<-split(training,c1)
    statUniform=rSquare(distance_split,times1[i],times2[i],penetration,segment_breaks)
    rSquareMat["Uniform",toString(penetration)]=rSquareMat["Uniform",toString(penetration)]+statUniform
    
   print(paste(times1[i],times2[i],penetration,statPIE,statDT,statUniform,sep=" "))
    
    
  }
    
  
}




rSquareMat=rSquareMat/5.0



      #SD Split
      query<-paste("select distance_along_road as distance,speed,time_stamp,agent_id from semsim_output WHERE iteration_count=50  AND time_stamp >=",4500, " AND time_stamp <=",4620," AND distance_along_road<=13000", sep="")
      training<-dbGetQuery(con,query)
      #Decision tree rpart
      
      modelFit <- rpart(speed ~ distance,data=training,control=rpart.control(maxdepth=14))
      #rpart1a <- as.party(modelFit)
      #plot(rpart1a)
      
      index=1
      decisonTreeSplit=numeric()
      for(split in modelFit$splits[,4]){
        decisonTreeSplit[index]=split
        index=index+1
      }
      
      decisonTreeSplit<-sort(decisonTreeSplit)
      decisonTreeSplit<-c(0.0,decisonTreeSplit)

      cutDT<-cut(training$distance,breaks = decisonTreeSplit,labels = decisonTreeSplit[1:length(decisonTreeSplit)-1])
      distance_split_dt<-split(training,cutDT)
      sdInSplits=sdSplits(distance_split_dt,4500,4620,1.0,decisonTreeSplit)
      avgSd=sapply(sdInSplits,mean)

#Close Database connection
dbDisconnect(con)

