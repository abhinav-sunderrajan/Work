pollutantmean<-function(directory, pollutant, id = 1:332){
  total=0.0
  n=0
  
  fileList = list.files(path=directory,pattern="*.csv")
  for(file in fileList){
    split=strsplit(file,split="\\.")
    monitorId=as.numeric(split[[1]][1])
    if(monitorId  %in% id){
      fileName=paste(directory,"/",file,sep="")
      monitorData=read.csv(fileName)
      good<-complete.cases(monitorData)
      monitorData=monitorData[good,]
      x<-monitorData[[pollutant]]
      total=total+sum(x)
      n=n+length(x)    
    }    
  }  
  
  if(n>0)
    total/n
  else
    0.0
}