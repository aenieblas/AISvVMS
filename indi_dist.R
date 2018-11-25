## indi_dist.R
## AENIEBLAS, 26/8/18
## DESCRIPTION: comparing the differences in the distances/lengths of individual trajectories between AIS and VMS data
## AEN TO DO:  expand data frames for the inputTable, redo figures. 
##             add the logbook fishing locations on top of trajectory and gfw plots
library(ggplot2)
library(ggthemes)

library(pacman)
p_load("rstudioapi", "data.table","rgeos","rgdal", "plyr", "ggplot2", "stringr","maps","ggplot2","ggthemes")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
save_path='output/report/'

inputTable <- expand.grid(Year=c(2016,2017), Gear=c("LL", "PS"))

ais.df<-NULL
vms.df<-NULL
for (yg in 1:dim(inputTable)[1]){
  vms_dataset<-NULL
  ais_dataset<-NULL
  # load vms trajectories data
  vms_data <- read.csv(paste0("input/trajectories_aggregation/vms_",inputTable$Gear[yg],"_",inputTable$Year[yg],"_compare_ais.csv"))
  ## sort by mmsi, then date in order to reassign ais id_object to match
  vms<-vms_data[order(vms_data[,'mmsi'],vms_data[,'timestamp']),]
  
  # load ais trajectories data
  ais_data <- read.csv(paste0("input/trajectories_aggregation/ais_",inputTable$Gear[yg],"_",inputTable$Year[yg],"_compare_vms.csv"))
 
  ## assign same id_object to same mmsi as in vms for ais
  vms_mmsi_id<-data.frame(mmsi=unique(vms$mmsi),id=unique(vms$id))
    for (m in 1:dim(vms_mmsi_id)[1]){
   ais_data[which(ais_data$mmsi==vms_mmsi_id$mmsi[m]),'id']<-vms_mmsi_id$id[m]
    }
  ais<-ais_data[order(ais_data[,'mmsi'],ais_data[,'timestamp']),]
  
  # select and rename columns
  ais_dataset<-ais[,c('id','id_traj','timestamp','lat','lon','speed','timediff','distance','speedcalc','nnet_score','mmsi')]
  colnames(ais_dataset)<-c('id','id_traj','timestamp','lat','lon','speed','timediff','distance','speedcalc','nnet_score','mmsi')
  ais_dataset$gear<-inputTable$Gear[yg]
  ais_dataset$year<-inputTable$Year[yg]
  
  vms_dataset<-vms[,c('id','id_traj','timestamp','lat','lon','speed','timediff','distance','speedcalc','id_object','mmsi')]
  colnames(vms_dataset)<-c('id','id_traj','timestamp','lat','lon','speed','timediff','distance','speedcalc','vessel','mmsi')
  vms_dataset$gear<-inputTable$Gear[yg]
  vms_dataset$year<-inputTable$Year[yg]
  
  # remove old data files to free up space
  rm(vms_data,vms,ais_data,ais)
  
  vms.df<-rbind(vms.df,vms_dataset)
  ais.df<-rbind(ais.df,ais_dataset)
}
  
save(vms.df,file='/home/anne.elise.nieblas/VMSvAIS/VMSvAIS/Chloe/output/report/vms_trajectories')
save(ais.df,file='/home/anne.elise.nieblas/VMSvAIS/VMSvAIS/Chloe/output/report/ais_trajectories')

# questions: 
## VESSEL LEVEL
  # 1) how many trajectories for each vessel for the year of data?
  # 5) what is the total distance travelled by each vessel?
vms_vessel<-ddply(na.exclude(vms.df),.(id,gear,year), function(x) data.frame(no_traj=length(unique(x$id_traj)),dist=sum(x$distance)))
ais_vessel<-ddply(na.exclude(ais.df),.(id,gear,year), function(x) data.frame(no_traj=length(unique(x$id_traj)),dist=sum(x$distance)))
vms_vessel$source<-'VMS'
ais_vessel$source<-'AIS'

source_vessel<-rbind(vms_vessel,ais_vessel)
### NUMBER OF TRAJECTORIES PER VESSEL 
p<-ggplot(source_vessel,aes(x=id,y=no_traj,fill=source))
p<-p+geom_bar(stat='identity', position='dodge') +
  facet_grid(~gear,scales='free_x') +
  xlab('Vessel ID') +
  ylab('Number of trajectories') +
  theme_economist_white()
p

### TOTAL DISTANCE TRAVELLED BY EACH VESSEL
p<-ggplot(source_vessel,aes(x=id,y=dist/1000,fill=source))
p<-p+geom_bar(stat='identity', position='dodge') +
  facet_wrap(~gear,scales='free_x') +
  xlab('Vessel ID') +
  ylab('Distance (km)') +
  theme_economist_white()
p

## TRAJECTORY LEVEL
  # 2) what are the lengths of each trajectory?
  # 3) over what length of time are they calculated?
# sum(vms_vessel$no_traj)=3591
vtraj<-ddply(na.exclude(vms.df),.(id,id_traj,gear,year),function(x) data.frame(start=x$timestamp[1],
                                                               end=x$timestamp[dim(x)[1]], 
                                                               dist=sum(x$distance[2:length(x$distance)]),
                                                               timediff=sum(x$timediff[2:length(x$timediff)])),.progress='text')
atraj<-ddply(na.exclude(ais.df),.(id,id_traj,gear,year),function(x) data.frame(start=x$timestamp[1],
                                                                          end=x$timestamp[dim(x)[1]], 
                                                                          dist=sum(x$distance[2:length(x$distance)]),
                                                                          timediff=sum(x$timediff[2:length(x$timediff)])),.progress='text')

atraj$source<-'AIS'
vtraj$source<-'VMS'
source_traj<-as.data.frame(rbind(atraj,vtraj))
q99<-quantile(source_traj$dist,probs=0.99,na.rm=T)
# st_99<-source_traj[which(source_traj$dist<q99),]

### LENGTH OF EACH TRAJECTORY
p<-ggplot(source_traj,aes(dist/1000))
p<-p+geom_histogram() +
  facet_wrap(~source) + xlim(0,q99/1000) + ylim(0,80) +
  xlab('Trajectory length (km)') + ylab('Frequency') +
  theme_economist_white()
p

### LENGTH OF TIME OF EACH TRAJECTORY
p<-ggplot(source_traj,aes(timediff/(60*60*24)))
p<-p+geom_histogram() +
  facet_wrap(~source) + #xlim(0,q99/1000) + ylim(0,80) +
  xlab('Days') + ylab('Frequency') +
  theme_economist_white()
p


## POINT LEVEL
# 4) what's the time difference between each point within a trajectory? (transmission frequency)
vpts<-ddply(na.exclude(vms.df),.(id,id_traj),function(x) data.frame(mntrans=mean(x$timediff)))
apts<-ddply(na.exclude(ais.df),.(id,id_traj),function(x) data.frame(mntrans=mean(x$timediff)))

vpts$source<-'VMS'
apts$source<-'AIS'
pts.df<-rbind(vpts,apts)

q99pts<-quantile(pts.df$mntrans,probs=.9,na.rm=T)

p<-ggplot(pts.df,aes(mntrans/(60*60)))
p<-p+geom_histogram() +
  facet_wrap(~source,scales='free') + xlim(0,q99pts/(60*60)) +
  xlab('Hours') + ylab('Frequency') +
  theme_economist_white()
p

### MAP LEVEL
# 6) where do the nnet_score points match to the vms data?

for (yg in 1:length(inputTable)){
  ais.yg<-ais.df[which(ais.df$gear==inputTable$Gear[yg] & ais.df$year==inputTable$Year[yg]),]
  vms.yg<-vms.df[which(vms.df$gear==inputTable$Gear[yg] & vms.df$year==inputTable$Year[yg]),]
# plot/explore trajectories  
ais_vess<-unique(ais.yg$id)
for(v in 1:length(ais_vess)){
  vms_v<-vms.yg[which(vms.yg$id==ais_vess[v]),]
  ais_v<-ais.yg[which(ais.yg$id==ais_vess[v]),]
  
  vms_traj<-unique(vms_v$id_traj)
  ais_traj<-unique(ais_v$id_traj)
  
    # how big is the ais box
  ais_lat_range=diff(range(ais_v$lat,na.rm=T),na.rm=T)
  ais_lon_range=diff(range(ais_v$lon,na.rm=T),na.rm=T)
  ais_space<-sum(ais_lat_range,ais_lon_range)
  
  # how big is the vms box
  vms_lat_range=diff(range(vms_v$lat,na.rm=T),na.rm=T)
  vms_lon_range=diff(range(vms_v$lon,na.rm=T),na.rm=T)
  vms_space<-sum(vms_lat_range,vms_lon_range)
  
  fishing<-ais_v[which(ais_v$nnet_score==1),c('lon','lat')]
  # the bigger box sets the range of the plot
  if(vms_space>ais_space){
    plot(vms_v[,c('lon','lat')],col='red',type='l',main=paste0(ais_vess[v]))
    points(ais_v[,c('lon','lat')],col='black',pch=16,type='o')
    # points(fishing,col='cyan')
    map('worldHires',add=TRUE,fill=TRUE,col='gray')
    legend('topleft',c('vms','ais','gfw-fishing'),col=c('red','black','cyan'),pch=16)
    
    for(vt in 1:length(vms_traj)){
     lines(vms_v[which(vms_v$id_traj==vms_traj[vt]),c('lon','lat')],col='red',type='o') 
    }
    for(at in 1:length(ais_traj)){
      lines(ais_v[which(ais_v$id_traj==ais_traj[at]),c('lon','lat')],col=at,type='b')
    }
  }else{
    plot(ais_v[,c('lon','lat')],col='black',pch=16,type='o',main=paste0(ais_vess[v]))
    points(vms_v[,c('lon','lat')],col='red',type='l')
    # points(fishing,col='cyan')
    map('worldHires',add=TRUE,fill=TRUE,col='gray')
    legend('topleft',c('vms','ais','gfw-fishing'),col=c('red','black','cyan'),pch=16)
    for(vt in 1:length(vms_traj)){
      lines(vms_v[which(vms_v$id_traj==vms_traj[vt]),c('lon','lat')],col='red',type='o') 
    }
    for(at in 1:length(ais_traj)){
      lines(ais_v[which(ais_v$id_traj==ais_traj[at]),c('lon','lat')],col=at,type='b')
    }
  }
}
}
# ### PLOTTING INDIVIDUAL TRAJECTORIES
#   for(t in 1:length(unique(ais_v$id_traj))){
#   plot(vms_v[which(vms_v$id_traj==t),c('lon','lat')],col='red',type='l',main=paste0(vms_data$id_object[v],vms_v$id_traj[t]))
#   lines(ais_v[which(ais_v$id_traj==t),c('lon','lat')],type='o')
#   map('worldHires',add=TRUE,fill=TRUE,col='gray')
#   }
#   }
#   }
#   

  

  
