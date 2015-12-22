library(RPostgreSQL)
library(ggplot2)
require(gridExtra)
library("reshape2")
library("plot3D")
require(tree)
library(caret)
library(partykit)
library(doParallel)
library(rpart)



drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')

time1=1000
time2=1150



#Pentration and sampling frequency

probePenetration=c(1,2,3,5,10)
samplingFreq=c(1,15,30,45,60,75,90,105,120)

errorMat<-matrix(0,length(probePenetration),length(samplingFreq))
rownames(errorMat)<-probePenetration
colnames(errorMat)<-samplingFreq



for(pen in probePenetration ){
  
    for(freq in samplingFreq){
      
      query<-paste("select distance_along_road as distance,speed,time_stamp from semsim_output WHERE iteration_count=47 AND  (agent_id%",pen,"=0) AND time_stamp >=",time1, " AND time_stamp <=",time2," AND time_stamp%",freq,"=0 AND distance_along_road<=13000", sep="")
      training<-dbGetQuery(con,query)
      
      #Fit decision tree
      
      modelFit <- rpart(speed ~ distance,data=training,control=rpart.control(maxdepth=14))
      pred=predict(modelFit)
      mse_rpart=mean((training$speed-pred)^2)
      
      
      
      index=1
      decisonTreeSplit=numeric()
      for(split in modelFit$splits[,4]){
        decisonTreeSplit[index]=split
        index=index+1
      }
      
      decisonTreeSplit<-sort(decisonTreeSplit)
      decisonTreeSplit<-c(0.0,decisonTreeSplit)
      
      
      #CUTS DECISION TREE
      cutDT<-cut(training$distance,breaks = decisonTreeSplit,labels = decisonTreeSplit[1:length(decisonTreeSplit)-1])
      distance_split_dt<-split(training,cutDT)
      print(query)
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
          num_of_vehicles<-length(time_split[[name_time]]$speed)*(1/pen)
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
      fit.decision.tree=lm(ls_density~log(ls_speed))
      errorMat[toString(pen),toString(freq)]<-summary(fit.decision.tree)$r.squared 
      
      
    }
  
}




rownames(errorMat)=c("100","50","33","20","10")
probePenetration=c(100,50,33,20,10)


persp3D(x=probePenetration,y=samplingFreq,z=errorMat, clab="R-Square",axes = TRUE, xlab = "Probe penetration (%)", ylab = "Sampling frequency (sec)",
        main = "", zlab="R-Square",  bty = "b2",theta=-65, phi=15, expand=0.55,
        shade = 0.2,ticktype="detailed",cex.axis = 1.10,cex.lab=1.1,xaxs = "i", yaxs = "i")

#default theta=-40






