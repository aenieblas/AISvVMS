# density maps of AIS pings
inputTable <- expand.grid(source=c('VMS','AIS'),Year=c(2016,2017), Gear=c("LL", "PS","SV"))
# define where your data are
pathToData <- "final_data/"

df_data<-NULL
for (i in 1:dim(inputTable)[1]){
  data_csv<-read.csv(paste0(pathToData,inputTable$Gear[i],inputTable$source[i],inputTable$Year[i],'_processed.csv'))
  # df of lon,lat,and 1 for a ping
   df_data<-rbind(df_data,data.frame(lon=data_csv$lon,lat=data_csv$lat,ping=1,source=paste0(inputTable$source[i]),gear=inputTable$Gear[i]))
}

df_dataX<-df_data[,-5]

gridsize <- 1
ry<-range(as.numeric(df_data$lat),na.rm=T)# Extent of map along the y-axis (latitude)
rx<-range(as.numeric(df_data$lon),na.rm=T)# Extent of map along the x-axis (longitude)
r<-raster(extent(rx,ry),res=gridsize)
r[]<-0

library(raster)
library(RColorBrewer)
library(ggthemes)

colrs <- rev(colorRampPalette(brewer.pal(8, 'Spectral'))(100))

rst<-df_dataX[which(df_data$source=='VMS'),]
rst<-rst[,-4]
coordinates(rst) <- c("lon","lat")

# rasterize and sum the transmissions in each cell for VMS data
per <- rasterize(rst, r, 'ping',fun=sum) #the column name that you want to plot
q99<-quantile(per,probs=c(0.99),na.rm=T)
per[per>=q99]<-q99

rstB<-df_dataX[which(df_data$source=='AIS'),]
rstB<-rstB[,-4]
coordinates(rstB) <- c("lon","lat")

# rasterize and sum the transmissions in each cell for AIS data
perB <- rasterize(rstB, r, 'ping',fun=sum) #the column name that you want to plot
perB[perB>=q99]<-q99

# load the ports file
ports<-read.csv('table_ports_jp.csv',header=TRUE,sep=';')

# axis breaks and labels
swbrks <- seq(-50,50,20)
wsbrks <- seq(10,130,20)
swlbls <- unlist(lapply(swbrks, function(x) ifelse(x < 0, paste(x, "°S"), ifelse(x > 0, paste(x, "°N"),x))))
wslbls <- unlist(lapply(wsbrks, function(x) ifelse(x < 0, paste(x, "°W"), ifelse(x > 0, paste(x, "°E"),x))))

# 2x1 subplots
par(mfrow=c(2,1),mai=c(.5,0.5,0.3,0.3))
#subplot 1 - VMS
image.plot(per,zlim=c(0,q99),col=colrs,ylim=c(-45,29),xlim=c(5,130),axes=F,ylab='',xlab='')# VMS
map('worldHires',add=T,col='gray',fill=T,border='gray')
points(ports$longdec,ports$latdec,col='black',pch=20,cex=0.5)
legend('top',legend='A',bty='n')
axis(2,swbrks,swlbls)
axis(1,wsbrks,wslbls)
box(col='black')

# subplot 2 - AIS
image.plot(perB,zlim=c(0,q99),col=colrs,ylim=c(-45,29),xlim=c(5,130),axes=F,ylab='',xlab='')# AIS
map('worldHires',add=T,col='gray',fill=T,border='gray')
points(ports$longdec,ports$latdec,col='black',pch=20,cex=0.5)
legend('top',legend='B',bty='n')
axis(2,swbrks,swlbls)
axis(1,wsbrks,wslbls)
box(col='black')

## anomaly map
par(mfrow=c(1,1))
anom<-per-perB
image.plot(anom,zlim=c(-q99,q99),col=colrs,ylim=c(-45,29),axes=F,ylab='',xlab='')# AIS
map('worldHires',add=T,col='gray',fill=T,border='gray')
points(ports$longdec,ports$latdec,col='black',pch=20,cex=0.75)
axis(2,swbrks,swlbls)
axis(1,wsbrks,wslbls)
box(col='black')


