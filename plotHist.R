df=read.csv("C:/Users/abhinav.sunderrajan/Desktop/MapMatch/PIE_TOA - 7-Aug-2014.csv")
summary(df)
par(mfrow=c(2,3))
for ( c in names(df) ){
  hist(df[[c]],main=sd(df[[c]], na.rm=TRUE) ,xlab=c,ylab="Freq")
}

