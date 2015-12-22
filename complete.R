complete <- function(directory, id = 1:332) {
  fileList = list.files(path=directory,pattern="*.csv")
  ids=numeric()
  nobs=numeric()
  
  for(file in fileList){
    split=strsplit(file,split="\\.")
    monitorId=as.numeric(split[[1]][1])
    
    if(monitorId  %in% id){
      fileName=paste(directory,"/",file,sep="")
      monitorData=read.csv(fileName)
      good<-complete.cases(monitorData)
      monitorData=monitorData[good,]
      nobs=c(nobs,nrow(monitorData))
      ids=c(ids,monitorId)
      
    }    
    
  }  
  df=data.frame(ids,nobs)
  df=df[match(id, df$ids),]
  colnames(df) <- c("id", "nobs")
  df
  
}