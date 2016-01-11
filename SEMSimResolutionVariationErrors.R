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

times1=c(1000,3600,4000,4500,5000)
times2=times1+120




#Pentration and sampling frequency

probePenetration=c(0.1,0.2,0.3,0.4,0.5,1.0)
samplingFreq=c(1,15,30,45,60,75,90)

errorMat<-matrix(0,length(probePenetration),length(samplingFreq))
rownames(errorMat)<-probePenetration
colnames(errorMat)<-samplingFreq


      
      for(i in 1:5){
        time1=times1[i]
        time2=times2[i]
        
        query<-paste("select distance_along_road as distance,speed,time_stamp,agent_id from semsim_output WHERE iteration_count=47 AND time_stamp >=",time1, " AND time_stamp <=",time2," AND distance_along_road<=13000", sep="")
        data<-dbGetQuery(con,query)
        
        for(penetration in probePenetration ){
          
          for(freq in samplingFreq){
            
            set.seed(i)
            samples=sample(unique(data$agent_id),length(unique(data$agent_id))*penetration,replace=FALSE)
            training <- subset(data, agent_id %in% samples)
            
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
            #print(query)
            ls_density=numeric()
            ls_speed=numeric()
            
            i=1
            
            for(name_dist in names(distance_split_dt)){
              y<-distance_split_dt[[name_dist]]
              c2<-cut(y$time_stamp,breaks =  seq(time1, time2, by = freq),labels = seq(time1, (time2-freq), by = freq))
              time_split<-split(y,c2)
              density_vec=numeric()
              speed_vec=numeric()
              index=1
              for(name_time in names(time_split)){
                num_of_vehicles<-length(unique(time_split[[name_time]]$agent_id))*(1/penetration)
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
            errorMat[toString(penetration),toString(freq)]<- errorMat[toString(penetration),toString(freq)]+summary(fit.decision.tree)$r.squared 
            
            
          }
          
        }
        
      }



errorMat=errorMat/5.0



rownames(errorMat)=c("10","20","30","40","50","100")
probePenetration=c(0.1,0.2,0.3,0.4,0.5,1.0)


persp3D(x=probePenetration,y=samplingFreq,z=errorMat, clab="R-Square",axes = TRUE, xlab = "Probe penetration (%)", ylab = "Temporal Resolution (sec)",
        main = "", zlab="R-Square",  bty = "b2",theta=-55, phi=19, expand=0.55,
        shade = 0.2,ticktype="detailed",cex.axis = 1.50,cex.lab=1.5,xaxs = "i", yaxs = "i")

#default theta=-40



#Close Database connection
dbDisconnect(con)


#ROUGH WORK
df=data.frame(ls_speed,predict(fit.decision.tree))
flow=ls_speed*predict(fit.decision.tree)*3.6
plot(flow~predict(fit.decision.tree))

summary(fit.decision.tree)

#380.342-119.674log(speed)

predict1=380.342-119.674*log(ls_speed)

plot(ls_speed,ls_density,xlab="",ylab="",cex.axis = 1.50,cex.lab=1.5)
 points(ls_speed, predict(fit.decision.tree), col="red")
points(ls_speed, predict1, col="blue")
  mtext("Speed (m/s)", 1, 2.5,cex=1.5)
 mtext("Density (veh/km)", 2, 2.5,cex=1.5)


