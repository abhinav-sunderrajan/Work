library(rgl)
df=read.csv("C:/Users/abhinav.sunderrajan/Desktop/piejurong_on_weekdays_byday.csv",sep="\t")
summary(df)
attach(df)
library(reshape2)
mat<-acast(df, DAY~TIME, value.var="STDDEV_SPEED")


z <- 3*mat       # Exaggerate the relief

x <-  1*(1:nrow(z))   # 10 meter spacing (S to N)
y <-  1*(1:ncol(z)) 

zlim <- range(y)
zlen <- zlim[2] - zlim[1] + 1

#jet.colors <-   # function from grDevices package
#colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#colorzjet <- jet.colors(zlen)  # 100 separate color 

colorlut <- rainbow(zlen) # height color lookup table

col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point

open3d()
material3d(col="black")
persp3d(x, y, z, aspect=c(3, 6, 1.5), col = colorzjet,xlab = "day", ylab = "time", zlab = "stddev speed")
surface3d(x,y,z,color=colorzjet, back="lines")
rgl.postscript("stddev.pdf","pdf")

