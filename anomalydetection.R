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


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')

      times1=c(1600)
      times2=times1+120
      penetration=1.0

      pieModels=list()


        for(time in 1:1){
            time1=times1[time]
            time2=times2[time]
            
            query<-paste("select distance_along_road as distance,speed,time_stamp,agent_id from semsim_output WHERE iteration_count=51  AND time_stamp >=",time1, " AND time_stamp <=",time2," AND distance_along_road<=13000", sep="")
            training<-dbGetQuery(con,query)
            
            
            #Decision tree rpart
            set.seed(108)
            
            modelFit <- rpart(speed ~ distance,data=training,control=rpart.control(maxdepth=15))
            #rpart1a <- as.party(modelFit)
            #plot(rpart1a)
         
            index=1
            decisonTreeSplit=numeric()
            for(split in modelFit$splits[,4]){
                  decisonTreeSplit[index]=split
                  index=index+1
            }
            
            decisonTreeSplit<-sort(decisonTreeSplit)
            decisonTreeSplit<-c(0.0,decisonTreeSplit,max(training$distance))
            
            cutDT<-cut(training$distance,breaks = decisonTreeSplit,labels = decisonTreeSplit[1:length(decisonTreeSplit)-1])
            distance_split<-split(training,cutDT)            
           
            ls_density=numeric()
            ls_speed=numeric()
            speedList=list()
            
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
                        density_vec[index]=num_of_vehicles*1000.0/(decisonTreeSplit[i+1]-decisonTreeSplit[i])
                        speed_vec[index]=mean(time_split[[name_time]]$speed)
                        index=index+1
                  }
                  
                  good=complete.cases(speed_vec,density_vec)
                  speed_vec=speed_vec[good]
                  density_vec=density_vec[good]
                  
                  good=speed_vec>0
                  speed_vec=speed_vec[good]
                  density_vec=density_vec[good]
                  
                  
                  ls_density=c(ls_density,density_vec)
                  ls_speed=c(ls_speed,speed_vec)
                  i=i+1
                  
                  speedList[[name_dist]]=c(speedList[[name_dist]],speed_vec)
                  
            }
            
            good=complete.cases(ls_speed,ls_density)
            ls_speed=ls_speed[good]
            ls_density=ls_density[good]
            
            good=ls_speed>0
            ls_speed=ls_speed[good]
            ls_density=ls_density[good]
            fit=lm(ls_density~log(ls_speed))
            
            summary(fit)$r.squared 
            
            
            
            densityList=list()
            flowList=list()
            
            for(name in names(speedList)){
                  newData=data.frame(speedList[[name]])
                  colnames(newData)=c("ls_speed")
                  densityList[[name]]=predict(fit,newdata=newData)
            }
            
            for(name in names(speedList)){
                  flowList[[name]]=speedList[[name]]* densityList[[name]]*3.6
                  
            }
            
            flow1=lapply(flowList,mean)
            density1=lapply(densityList,mean)
            speed1=lapply(speedList,mean)
            
            
            d=data.frame(as.numeric(flow1),as.numeric(speed1),as.numeric(density1),row.names=names(speedList))
            colnames(d)=c("Flow","Speed","Density")
            pieModels[[toString(time)]]=d
            print(summary(d))
            print(paste("Finished model ",time,sep=""))
            
        }

      
      
      
      
      
      
      
      #Close Database connection
      dbDisconnect(con)
      
      
