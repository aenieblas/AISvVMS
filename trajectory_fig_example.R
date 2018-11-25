## trajectory_fig_example.R
## 24/11/18, AE Nieblas
## DESCRIPTION: This script plots a series of points, then interpolates to a trajectory, then overlays a 0.5 deg grid to represent the methodology used 
## for trajectory analysis. (Figure 3 in AIS-Atlas)

library(raster)
require(rasterVis)

## load VMS data
setwd('final_data/AUG')
vms<-read.csv('LLVMS2016_processed.csv')

# find the positions of the first vessel in the vms timeseries (can be any vessel)
uv<-unique(vms$vessel)


## find just one month of data
um<-unique(vms$Date)
this_vessel<-vms[which(vms$vessel==uv[1] & vms$Date==um[2]),]

par(mfrow=c(1,3))
## points
plot(this_vessel$lon,this_vessel$lat,type='p',col="#2166AC",lwd=1,pch=16,axes=FALSE,xlab='',ylab='',ylim=c(-8.65,-7.5),xlim=c(47.75,48.41))
box()
legend('topleft',legend='A',bty='n',cex=1.5)

## points and lines
plot(this_vessel$lon,this_vessel$lat,type='p',col="#2166AC",lwd=1,pch=16,axes=FALSE,xlab='',ylab='',ylim=c(-8.65,-7.5),xlim=c(47.75,48.41))
lines(this_vessel$lon,this_vessel$lat,col="#4393C3",lwd=1,pch=16)
box()
legend('topleft',legend='B',bty='n',cex=1.5)


## grid
### Define the grid
gridsize <- 0.1
ry<-range(as.numeric(this_vessel$lat),na.rm=T)# Extent of map along the y-axis (latitude)
rx<-range(as.numeric(this_vessel$lon),na.rm=T)# Extent of map along the x-axis (longitude)
r<-raster(extent(rx,ry),res=gridsize)
r[]<-0

## count the number of records within each cell. this should be per vessel. 
rst <- data.frame(lon=as.numeric(this_vessel$lon),lat=as.numeric(this_vessel$lat), z=1)
# rst <- rbind(rst, data.frame(lon=as.numeric(log$lon),lat=as.numeric(log$lat), percent_cov=-10))
coordinates(rst) <- c("lon","lat")

rastermap <- rasterize(rst,r,'z',fun=sum)
plot(rastermap,main='', col=brewer.pal(11, 'RdBu'),axes=FALSE)
legend('topleft',legend='C',bty='n',cex=1.5)

