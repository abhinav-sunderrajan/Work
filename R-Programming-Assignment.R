corr <- function(directory, threshold = 0) {
  corVec=numeric()
  index=1
  
  fileList = list.files(path=directory,pattern="*.csv")

  
    for(file in fileList){
      fileName=paste(directory,"/",file,sep="")
      monitorData=read.csv(fileName)
      good<-complete.cases(monitorData)
      monitorData=monitorData[good,]
      if(nrow(monitorData)>threshold){
        corVec[index]=cor(monitorData[["sulfate"]],monitorData[["nitrate"]])
        index=index+1
      }
      
    }    
  
  corVec
  }  
  



