## GFWvLOGs.R

###################
### LIBRARIES   ###
###################
library(pacman)
p_load(raster, plyr, mapdata, RColorBrewer, lubridate, ggplot2, geosphere)

###
### Function to calculate true/false positives and negatives
matchFunction <- function(fileAIScsv, fileLogbookcsv){
  ### read the files
  llais_dupe_checked <- read.csv(fileAIScsv)
  lllog_dupe_checked <- read.csv(fileLogbookcsv)
  # select only logbook data were the vessel is in the ais data
  lllog_dupe_checked <- lllog_dupe_checked[which(lllog_dupe_checked$vessel %in% unique(llais_dupe_checked$vessel)),]
  # create the table with outputs
  table_out=NULL
  # add a column with only the day
  llais_dupe_checked$day <- as.character(strptime(llais_dupe_checked$time, format="%d/%m/%Y"))
  lllog_dupe_checked$day <- as.character(strptime(lllog_dupe_checked$time, format="%d/%m/%Y"))
  # get the true positives
  # select only
  ais_pos <- llais_dupe_checked[llais_dupe_checked$nnet_score==1,]
  ais_pos <- ais_pos[-which(is.na(ais_pos$nnet_score)==TRUE),]
  true_positives <- merge(lllog_dupe_checked, ais_pos, by=c("day", "vessel"))
  tot_true_positives <- ddply(na.exclude(true_positives), .(day, vessel), function(x) data.frame(vessel=unique(x$vessel),
                                                                                     day=unique(x$day),
                                                                                     lonLog = mean(x$lon.x),
                                                                                     latLog = mean(x$lat.x),
                                                                                     lonAIS = mean(x$lon.y),
                                                                                     latAIS = mean(x$lat.y)), .progress="text")
  table_out$truePos   <- dim(tot_true_positives)[1]
  
  ## FALSE POSITIVES: find the days where there is NO logbook data, but there IS GFW predictions
  ## should be the dim(ais_pos)-dim(true_positives)<- then find the unique vess/day/etc of the leftover dataset
  all_positives       <- merge(lllog_dupe_checked, ais_pos, by=c("day", "vessel"),all=T)
  false_positives     <- all_positives[which(is.na(all_positives$lon.x)==TRUE & is.na(all_positives$lat.x)==TRUE),]
  tot_false_positives <- ddply(false_positives, .(day, vessel), function(x) data.frame(vessel=unique(x$vessel),
                                                                                     day=unique(x$day),
                                                                                     lonLog = mean(x$lon.x),
                                                                                     latLog = mean(x$lat.x),
                                                                                     lonAIS = mean(x$lon.y),
                                                                                     latAIS = mean(x$lat.y)), .progress="text")
  
  # table_out$falsePos  <- dim(lllog_dupe_checked)[1]-dim(tot_true_positives)[1]# false negative??
  table_out$falsePos  <- dim(tot_false_positives)[1]
  llais_dupe_checked2 <- ddply(llais_dupe_checked, .(vessel, day), function(x) data.frame(negative=sum(x$nnet_score, na.rm=T)))
  llais_dupe_checked2 <- merge(llais_dupe_checked, llais_dupe_checked2, by=c("vessel", "day"))
  llais_dupe_checked2 <- llais_dupe_checked2[-which(is.na(llais_dupe_checked2$nnet_score)==TRUE),]
  ais_neg             <- llais_dupe_checked2[llais_dupe_checked2$negative==0,]
  false_negatives     <- merge(lllog_dupe_checked, ais_neg, by=c("day", "vessel"))
  tot_false_negatives <- ddply(false_negatives, .(day, vessel), function(x) data.frame(vessel=unique(x$vessel),
                                                                                       day=unique(x$day),
                                                                                       lonLog = mean(x$lon.x),
                                                                                       latLog = mean(x$lat.x),
                                                                                       lonAIS = mean(x$lon.y),
                                                                                       latAIS = mean(x$lat.y)), .progress="text")
  table_out$falseNeg  <- dim(tot_false_negatives)[1]
  table_out$trueNeg   <- length(unique(lllog_dupe_checked$vessel))*365 - dim(lllog_dupe_checked)[1] -  dim(tot_false_negatives)[1]
  all_possible_negs   <- length(unique(lllog_dupe_checked$vessel))*365 - dim(lllog_dupe_checked)[1]
  all_possible_pos    <- dim(lllog_dupe_checked)[1]
  ### Same but in percent
  # table_out$truePosPercent  <- table_out$truePos / (table_out$truePos+table_out$falsePos) *100
  # table_out$falsePosPercent <- table_out$falsePos / (table_out$truePos+table_out$falsePos) *100
  # table_out$falseNegPercent <- table_out$falseNeg / (table_out$falseNeg + table_out$trueNeg) *100
  # table_out$trueNegPercent  <- table_out$trueNeg / (table_out$falseNeg + table_out$trueNeg) *100
  
  # percent calculated using the T/F Pos(Neg) predicted by GFW over the total possible Pos/Negs using the log data 
  table_out$truePosPercent  <- table_out$truePos / all_possible_pos *100
  table_out$falsePosPercent <- table_out$falsePos / all_possible_pos *100
  table_out$falseNegPercent <- table_out$falseNeg / all_possible_negs *100
  table_out$trueNegPercent  <- table_out$trueNeg / all_possible_negs *100
  
  # over all the days of the study
  # table_out$truePosPercent  <- table_out$truePos / (length(unique(lllog_dupe_checked$vessel))*365) *100
  # table_out$falsePosPercent <- table_out$falsePos / (length(unique(lllog_dupe_checked$vessel))*365) *100
  # table_out$falseNegPercent <- table_out$falseNeg / (length(unique(lllog_dupe_checked$vessel))*365) *100
  # table_out$trueNegPercent  <- table_out$trueNeg / (length(unique(lllog_dupe_checked$vessel))*365) *100
  
  table_out <- as.data.frame(table_out)
  return(table_out)
}

### Output table
# Create the different combination of gears and years
inputTable <- expand.grid(Year=c(2016,2017), Gear=c("LL", "PS"))
# define where your data are
pathToAISData <- "final_data/"
pathToLOGData <- "final_data/"

# gear='LL';
# yr='16'
# ## 
# ais<-read.csv(paste0('final_data/AIS/',gear,'AIS',yr,'_processed.csv'))
# log<-read.csv(paste0('final_data/LOG/',gear,'LOG',yr,'_processed.csv'))

# loop over the matchFunction described above
matchTable <- NULL
for (i in 1:4){
  print(paste0("Processing: ", inputTable$Gear[i], " for year: ", inputTable$Year[i]))
  tmpOutput <- matchFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
                             paste0(pathToLOGData, inputTable$Gear[i], "LOG", as.character(inputTable$Year[i]), "_processed.csv" ))
  matchTable <- rbind(matchTable, cbind(inputTable[i,], tmpOutput))
}

match_df<-as.data.frame(matchTable[,c('Gear','Year','truePosPercent', 'falsePosPercent', 'falseNegPercent', 'trueNegPercent')])
colnames(match_df)<-c('gear','year','truePos','falsePos','falseNeg','trueNeg')
match_df$gryr<-paste0(matchTable$Gear,matchTable$Year)
match_df2<-melt(match_df,id.vars=c('gryr','gear','year'))
colnames(match_df2)=c('gryr','gear','year','match.class','value')

bp<- ggplot(match_df2, aes(x=gryr, y=value, fill=match.class))+
  geom_bar(width=1,stat='identity')+
  scale_fill_brewer(palette = "RdBu")#+ theme_economist()
bp



### Histogram of the hour of the day when catch is occurring
# Function for a fleet / year
hourFunction <- function(fileAIScsv){
  llais_dupe_checked <- read.csv(fileAIScsv)
  ais_pos            <- llais_dupe_checked[llais_dupe_checked$nnet_score==1,]
  ais_pos            <- ais_pos[-which(is.na(ais_pos$nnet_score)==TRUE),]
  df                 <- data.frame(time=as.POSIXct(strptime(ais_pos$time, format="%d/%m/%Y %H:%M:%S")))
  #Extract Time
  df$hour = hour(df$time) + minute(df$time)/60 + second(df$time)/3600
  #Create Bins
  bins=c(paste0(rep(c(paste0(0,0:9),10:23), each=4),".", c("00",25,50,75))[-1],"24:00")
  #Divide Data Into Bins
  df$bins = cut(df$hour, breaks=seq(0, 24, 0.25), labels=bins)
  #Reformat to Numeric
  df$bins <- as.numeric(as.character(df$bins))
  return(df)
}

hourTable <- NULL
for (i in 1:4){
  print(paste0("Processing: ", inputTable$Gear[i], " for year: ", inputTable$Year[i]))
  tmpOutput <- hourFunction(paste0(pathToData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ))
  hourTable <- rbind(hourTable, cbind(inputTable[i,], tmpOutput))
}

# Plot: Histgram
hourTable$gryr<-paste0(hourTable$Gear,hourTable$Year)
hourTable$plot<-NA
hourTable[which(hourTable$gryr=='LL2016'),'plot']<-'A'
hourTable[which(hourTable$gryr=='LL2017'),'plot']<-'B'
hourTable[which(hourTable$gryr=='PS2016'),'plot']<-'C'
hourTable[which(hourTable$gryr=='PS2017'),'plot']<-'D'
# ggplot(hourTable, aes(bins)) +
#   geom_histogram()+xlab("Hour of day")+ylab("Number of estimated catch from GFW") + 
#   facet_wrap(~Year+Gear, scales = "free")+theme_economist()
# ggsave(paste0(pathToData, "hourOfCatch.png"))
ggplot(hourTable, aes(bins)) +
  geom_histogram()+xlab("Hour of day")+ylab("Number of estimated catch from GFW") + 
  facet_wrap(~plot, scales = "free")+theme_economist_white()
ggsave(paste0(pathToData, "hourOfCatch.png"))


### MAP FUNCTIONS

mapDataGetFunction <- function(fileAIScsv, fileLogbookcsv){
  ### read the files
  llais_dupe_checked <- read.csv(fileAIScsv)
  lllog_dupe_checked <- read.csv(fileLogbookcsv)
  # select only logbook data were the vessel is in the ais data
  lllog_dupe_checked <- lllog_dupe_checked[which(lllog_dupe_checked$vessel %in% unique(llais_dupe_checked$vessel)),]
  # create the table with outputs
  table_out=NULL
  # add a column with only the day
  llais_dupe_checked$day <- as.character(strptime(llais_dupe_checked$time, format="%d/%m/%Y"))
  lllog_dupe_checked$day <- as.character(strptime(lllog_dupe_checked$time, format="%d/%m/%Y"))
  # get the true positives
  # select only
  ais_pos <- llais_dupe_checked[llais_dupe_checked$nnet_score==1,]
  ais_pos <- ais_pos[-which(is.na(ais_pos$nnet_score)==TRUE),]
  true_positives <- merge(lllog_dupe_checked, ais_pos, by=c("day", "vessel"))
  tot_true_positives <- ddply(true_positives, .(day, vessel), function(x) data.frame(vessel=unique(x$vessel),
                                                                                     day=unique(x$day),
                                                                                     lonLog = mean(x$lon.x),
                                                                                     latLog = mean(x$lat.x),
                                                                                     lonAIS = mean(x$lon.y),
                                                                                     latAIS = mean(x$lat.y)), .progress="text")
  
  
  llais_dupe_checked2 <- ddply(llais_dupe_checked, .(vessel, day), function(x) data.frame(negative=sum(x$nnet_score, na.rm=T)))
  llais_dupe_checked2 <- merge(llais_dupe_checked, llais_dupe_checked2, by=c("vessel", "day"))
  llais_dupe_checked2 <- llais_dupe_checked2[-which(is.na(llais_dupe_checked2$nnet_score)==TRUE),]
  ais_neg             <- llais_dupe_checked2[llais_dupe_checked2$negative==0,]
  false_negatives     <- merge(lllog_dupe_checked, ais_neg, by=c("day", "vessel"))
  tot_false_negatives <- ddply(false_negatives, .(day, vessel), function(x) data.frame(vessel=unique(x$vessel),
                                                                                       day=unique(x$day),
                                                                                       lonLog = mean(x$lon.x),
                                                                                       latLog = mean(x$lat.x),
                                                                                       lonAIS = mean(x$lon.y),
                                                                                       latAIS = mean(x$lat.y)), .progress="text")
  return(list(AIS=rbind(cbind(condition="TP", tot_true_positives), cbind(condition="FN", tot_false_negatives)), LOG=lllog_dupe_checked ))
}

mapFunction <- function(dataIn, condition,letter){
  dataUse <- dataIn$AIS[which(dataIn$AIS$condition==condition),]
  dataLOG <- dataIn$LOG
  gridsize <- 1
  ry<-range(as.numeric(dataIn$LOG$lat),na.rm=T)# Extent of map along the y-axis (latitude)
  rx<-range(as.numeric(dataIn$LOG$lon),na.rm=T)# Extent of map along the x-axis (longitude)
  r<-raster(extent(rx,ry),res=gridsize)
  r[]<-0
  
  ## count the number of records within each cell. this should be per vessel. 
  ## and the percentage should be over the full time spent in the cell 
  rst <- data.frame(lon= as.numeric(dataUse$lonLog), lat = as.numeric(dataUse$latLog),z=1)
  coordinates(rst) <- c("lon","lat")
  per <- rasterize(rst, r, 'z',fun=sum) #the column name that you want to plot
  rstLOG<-data.frame(lon=dataLOG$lon,lat=dataLOG$lat,z=1)
  coordinates(rstLOG)<-c("lon","lat")
  perLOG <- rasterize(rstLOG, r, 'z',fun=sum) #the column name that you want to plot
  q99<-quantile(perLOG,probs=0.99,na.rm=T)
  perLOG[perLOG>=q99]<-q99
  # density<-kde2d(rst[,1],rst[,2],n=100)
  # density[which(density$z==0),'z']<-NA
  
 
 # rst2<-data.frame(LL2016$AIS$lonAIS,LL2016$AIS$latAIS,z=1)
 # density2<-kde2d(rst2[,1],rst2[,2],n=100)
 library(RColorBrewer)
 colrs <- colorRampPalette(brewer.pal(8, 'Spectral'))(100)
 ports<-read.csv('table_ports_jp.csv',header=TRUE,sep=';')
 
 library(fields)
  image.plot(perLOG,col=colrs,zlim=c(3,q99),ylim=c(-40,10),xlim=c(20,90)) 
 map('worldHires',add=T,col='grey',border='grey',fill=T)
 points(ports$longdec,ports$latdec,col='black',pch=20,cex=1)
 legend('topleft',legend=letter[1],bty='n',cex=1.5)
 
 image.plot(per,col=colrs,zlim=c(3,q99),ylim=c(-40,10),xlim=c(20,90))
 map('worldHires',add=T,col='grey',fill=T,border='grey')
 points(ports$longdec,ports$latdec,col='black',pch=20,cex=1)
 legend('topleft',legend=letter[2],bty='n',cex=1.5)
 
 # colnames(rst2)<-c('lon','lat','z')
 # coordinates(rst2) <- c("lon","lat")
 # per <- rasterize(rst, r, 'z',fun=sum) #the column name that you want to plot
 # plot(per)
 # per2<-rasterize(rst2,r,'z',fun=sum)
 # plot(per2)
  # rst2 <- data.frame(lon= as.numeric(dataIn$LOG$lon), lat = as.numeric(dataIn$LOG$lat),z=1)
  # coordinates(rst2) <- c("lon","lat")
  # per2 <- rasterize(rst2, r, 'z') #the column name that you want to plot
  
  # par(mfrow=c(1,2))
  # par(mar=c(3,2,3,0))
  # plot(per2, main="Catch position", col= "dark red", legend=FALSE)
  # map("worldHires", add=T, fill=F)
  # par(mar=c(3,2,3,0))
  # plot(per,main=paste("AIS", condition), col= "dark blue", legend=FALSE)
  # map("worldHires", add=T, fill=F)
  
  # changing the background with the maps.
  # p<-ggplot(rst, aes(x=lon, y=lat,fill=z) )+
  # p<-p+stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+ #+ geom_point(colour = "white")
  # p<-p+stat_density2d(mapping= aes(alpha = ..level..), breaks=1e-6*seq(0,10,by=2),geom="contour", bins=4, size= 2)+
    # scale_alpha_continuous(limits=c(0,1e-5))
  # p
  # ggplot(rst, aes(x=lon, y=lat)) +  
    # geom_tile(data=rst, aes(x=lon, y=lat, fill=z), alpha=0.8) +
    
    # geom_raster()#+
    # stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
    # geom_map(data=monde, map=monde, aes(map_id=region), fill="black", color="black", size=0.1)+
    # geom_blank() + xlab('')+ylab('')+
    # scale_x_continuous(breaks = wsbrks, labels = wslbls, expand = c(0, 0),limits=c(25, 82)) +
    # scale_y_continuous(breaks = swbrks, labels = swlbls, expand = c(0, 0),limits=c(-35, 10.5)) +
    # 
    # # scale_x_continuous(breaks = wsbrks, labels = wslbls, expand = c(0, 0),limits=c(35, 125)) +
    # # scale_y_continuous(breaks = swbrks, labels = swlbls, expand = c(0, 0),limits=c(-40, 25)) +
    # theme(axis.text=element_text(size=12),
    #       text = element_text(color = "#22211d"),
    #       plot.background = element_rect(fill = "#f5f5f2", color = NA),
    #       panel.background = element_rect(fill = "#c5dae6", color = NA),
    #       legend.background = element_rect(fill = "#f5f5f2", color = NA),
    #       
    #       plot.caption = element_text( size=14, color = "#4e4d47", margin = margin(b = 0.5, r=0, unit = "cm") ),
    #       legend.title = element_text(size=14, face = "bold"),
    #       legend.text = element_text(size = 14),
    #       legend.position = "bottom")+
    # scale_fill_gradientn(colours=colrs)+coord_fixed(ratio=1)#+
  # p
}
### PLOT the map of the logbook data and where you got a true positive match between AIS and logbook
# Get the data for LL in 2016
i=1
LL2016 <- mapDataGetFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
                             paste0(pathToLOGData, inputTable$Gear[i], "LOG", as.character(inputTable$Year[i]), "_processed.csv" ))
# i=1
# LL2016 <- mapDataGetFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
                             # paste0(pathToLOGData, inputTable$Gear[i], "LOG", substr(as.character(inputTable$Year[i]),3,4), "_processed.csv" ))


i=2
LL2017 <- mapDataGetFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
                             paste0(pathToLOGData, inputTable$Gear[i], "LOG", as.character(inputTable$Year[i]), "_processed.csv" ))
# LL2017 <- mapDataGetFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
#                              paste0(pathToLOGData, inputTable$Gear[i], "LOG", substr(as.character(inputTable$Year[i]),3,4), "_processed.csv" ))
i=3
PS2016 <- mapDataGetFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
                             paste0(pathToLOGData, inputTable$Gear[i], "LOG", as.character(inputTable$Year[i]), "_processed.csv" ))
# i=4
# PS2017 <- mapDataGetFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
#                              paste0(pathToLOGData, inputTable$Gear[i], "LOG", substr(as.character(inputTable$Year[i]),3,4), "_processed.csv" ))
i=4
PS2017 <- mapDataGetFunction(paste0(pathToAISData, inputTable$Gear[i], "AIS", as.character(inputTable$Year[i]), "_processed.csv" ),
                             paste0(pathToLOGData, inputTable$Gear[i], "LOG", as.character(inputTable$Year[i]), "_processed.csv" ))

#Map True positives
par(mfrow=c(4,2))
mapFunction(LL2016, "TP",c('A','B'))
mapFunction(LL2017, "TP",c('C','D'))
mapFunction(PS2016, "TP",c('E','F'))
mapFunction(PS2017, "TP",c('G','H'))

#Map False Negatives
mapFunction(LL2016, "FN")
mapFunction(LL2017, "FN")
mapFunction(PS2016, "FN")
mapFunction(PS2017, "FN")


### Calculate the distance between AIS and logbook catch positions, true positives
histdistLogAIS <- function(dataIn, breaks, maxDist){
  dataIn$AIS$dist <- distVincentyEllipsoid(p1=data.frame(dataIn$AIS$lonLog, dataIn$AIS$latLog),
                                           p2= data.frame(dataIn$AIS$lonAIS, dataIn$AIS$latAIS),
                                           a=6378137, b=6356752.3142, f=1/298.257223563)/1000
  hist(dataIn$AIS$dist, breaks=breaks, xlim=c(0,maxDist), main="", xlab="Distance in km", ylab="Number of occurrence")
  print(quantile(dataIn$AIS$dist))
  return(dataIn$AIS$dist)
}
par(mfrow=c(2,2))
LL2016$AIS$dist <- histdistLogAIS(LL2016, 100, 200)
LL2017$AIS$dist <- histdistLogAIS(LL2017, 300, 200)
PS2016$AIS$dist <- histdistLogAIS(PS2016, 15, 400)
PS2017$AIS$dist <- histdistLogAIS(PS2017, 15, 400)

LL2016$AIS$gryr <- 'LL2016'
LL2017$AIS$gryr <- 'LL2017'
PS2016$AIS$gryr <- 'PS2016'
PS2017$AIS$gryr <- 'PS2017'

df_dist<-as.data.frame(rbind(LL2016$AIS[,c('gryr','dist')],
                             LL2017$AIS[,c('gryr','dist')],
                             PS2016$AIS[,c('gryr','dist')],
                             PS2017$AIS[,c('gryr','dist')]))
df_dist$plot<-NA
df_dist[which(df_dist$gryr=='LL2016'),'plot']<-'A'
df_dist[which(df_dist$gryr=='LL2017'),'plot']<-'B'
df_dist[which(df_dist$gryr=='PS2016'),'plot']<-'C'
df_dist[which(df_dist$gryr=='PS2017'),'plot']<-'D'
# Plot: Histgram
ggplot(df_dist, aes(dist)) +
  geom_histogram(binwidth=30)+ xlim(c(0,400))+facet_wrap(~plot, scales = "free")+theme_economist_white()+
  xlab("Distance (km)")+ylab("Count") 
ggsave(paste0(pathToLOGData, "TruePosDistances.png"))



########## plot the points where the distances are very far (LL)
dataM <- LL2017$AIS
iwh <- which(dataM$dist>200)
par(mfrow=c(1,1))
plot(dataM$lonAIS[iwh], dataM$latAIS[iwh], col="#D53E4F", pch=16,xlab='',ylab='')
legend("bottomright", pch=16, col=c("#D53E4F","#3288BD"), legend=c("Logbook","GFW-AIS"),border=FALSE,bty='n',cex=1)
points(dataM$lonLog[iwh], dataM$latLog[iwh], col="#3288BD", pch=16)
map("worldHires", add=T, fill=T,col='grey',border='grey')
abline(0,0,lty=5,col='darkgrey')

###############################################################################################
#### Estimate the impact of lagging the catch day on the match between AIS and logbook
### Function to add or substract "nbDay" to the catch date from log-book
lagFunction <- function(AISIn,LogIn,nbDay){
  lllog_dupe_checked_plusOneDay       <- LogIn
  lllog_dupe_checked_plusOneDay$time  <- strptime(lllog_dupe_checked_plusOneDay$time, format="%d/%m/%Y %H:%M:%S") + 3600*24*nbDay
  lllog_dupe_checked_plusOneDay$day   <- as.character(lllog_dupe_checked_plusOneDay$time)
  ais_pos                             <- ais[ais$nnet_score==1,]
  ais_pos                             <- ais_pos[-which(is.na(ais_pos$nnet_score)==TRUE),]
  ais_pos$day                         <- as.character(ais_pos$time)
  true_positives_plusOneDay           <- merge(lllog_dupe_checked_plusOneDay, ais_pos, by=c("day", "vessel"))
  tot_true_positives_plusOneDay       <- ddply(true_positives_plusOneDay, .(day, vessel), function(x) data.frame(vessel=unique(x$vessel),
                                                                                                                 day=unique(x$day),
                                                                                                                 lonLog = mean(x$lon.x),
                                                                                                                 latLog = mean(x$lat.x),
                                                                                                                 lonAIS = mean(x$lon.y),
                                                                                                                 latAIS = mean(x$lat.y)), .progress="text")
  return(dim(tot_true_positives_plusOneDay)[1])
}

gear='LL';
yr='16'
# ## 
ais<-read.csv(paste0('final_data/AIS/',gear,'AIS',yr,'_processed.csv'))
log<-read.csv(paste0('final_data/LOG/',gear,'LOG',yr,'_processed.csv'))

laggedMatchDatabase <- NULL
for (i in -10:10){laggedMatchDatabase <- rbind(laggedMatchDatabase, data.frame(day=i, truepositive=lagFunction(ais,log,i)))}
plot(laggedMatchDatabase)



#### for each year, gear, make a 1) density map of AIS only, histogram of anomalies, and regression plot
par(mfrow=c(3,2),ps=16)
condition='TP'
thisais<-LL2016$AIS[which(LL2016$AIS$condition==condition),]
thislog<-LL2016$LOG

thisais<-LL2017$AIS[which(LL2017$AIS$condition==condition),]
thislog<-LL2017$LOG
# LL2016$AIS <- LL2016$AIS[which(LL2016$AIS$condition==condition),]

gridsize <- .5
ry<-range(as.numeric(thislog$lat),na.rm=T)# Extent of map along the y-axis (latitude)
rx<-range(as.numeric(thislog$lon),na.rm=T)# Extent of map along the x-axis (longitude)
r<-raster(extent(rx,ry),res=gridsize)
r[]<-0
## count the number of records within each cell. this should be per vessel. 
## and the percentage should be over the full time spent in the cell 
rst <- data.frame(lon= as.numeric(thisais$lonLog), lat = as.numeric(thisais$latLog),z=1)
coordinates(rst) <- c("lon","lat")
# per <- rasterize(rst, r, 'z') #the column name that you want to plot
rst2<-data.frame(thislog$lon,thislog$lat,z=1)
colnames(rst2)<-c('lon','lat','z')
coordinates(rst2) <- c("lon","lat")

rf <- colorRampPalette(rev(brewer.pal(10,'Spectral')))  # make colors
colrs<-rf(100)

per <- rasterize(rst, r, 'z',fun=sum) #the column name that you want to plot
per2<-rasterize(rst2,r,'z',fun=sum)
anom=(per2-per)/(per2)*100 #log-ais/(log) # lots of good points, and about 50% of over-estimation
ais_log<-data.frame(pred=as.data.frame(per)[[1]],obs=as.data.frame(per2)[[1]],gy='LL16')
gfw_mod<-lm(ais_log$pred~ais_log$obs)
ss<-summary(gfw_mod)
q95<-quantile(per,probs=.95)


image.plot(per,zlim=c(0,10),col=colrs,ylim=c(-45,20),axes=T,ylab='')
map('worldHires',add=T,col='gray',border='gray',fill=T)

legend('topleft',legend='A',bty='n',cex=1.5)
legend('topleft',legend='B',bty='n',cex=1.5)
# image.plot(per2,zlim=c(0,80),main='LOG 2016',col=colrs,ylim=c(-45,20))
# map('worldHires',add=T,col='black',fill=T)

hist(anom,xlab='',main='',ylim=c(0,700))

legend('topleft',legend='C',bty='n',cex=1.5)
legend('topleft',legend='D',bty='n',cex=1.5)

plot(ais_log$obs,ais_log$pred,pch=16,type='p',xlab='Logbook observations',ylab='AIS predictions',cex=1)
abline(gfw_mod)
abline(1,1,col='red')
legend('bottomright',legend=paste0('r2=',round(ss$adj.r.squared,digits=2)),bty='n',cex=1.5)## 1degx1deg

legend('topleft',legend='E',bty='n',cex=1.5)
legend('topleft',legend='F',bty='n',cex=1.5)



##### EFFORT MEASUREMENTS
#### for each year, gear, make a 1) density map of AIS only, histogram of anomalies, and regression plot
par(mfrow=c(3,2),ps=16)
condition='TP'

thisais<-LL2016$AIS[which(LL2016$AIS$condition==condition),]
thislog<-LL2016$LOG

# thisais<-LL2017$AIS[which(LL2017$AIS$condition==condition),]
# thislog<-LL2017$LOG
# LL2016$AIS <- LL2016$AIS[which(LL2016$AIS$condition==condition),]

gridsize <- 5
ry<-range(as.numeric(thislog$lat),na.rm=T)# Extent of map along the y-axis (latitude)
rx<-range(as.numeric(thislog$lon),na.rm=T)# Extent of map along the x-axis (longitude)
r<-raster(extent(rx,ry),res=gridsize)
r[]<-0
## count the number of records within each cell. this should be per vessel. 
## and the percentage should be over the full time spent in the cell 
rstHOOKS <- data.frame(lon= as.numeric(thisais$lonLog), lat = as.numeric(thisais$latLog),z=1)
over20<-which(rstHOOKS$lat>(-20))
under20<-which(rstHOOKS$lat<(-20))
# replace z level with the average number of hooks found above and below 20S
rstHOOKS[over20,'z']<-3000
rstHOOKS[under20,'z']<-3670
coordinates(rstHOOKS) <- c("lon","lat")
# sum the number of hooks in each cell
effortAIS <- rasterize(rstHOOKS, r, 'z',fun=sum) #the column name that you want to plot
q95ais<-quantile(effortAIS,probs=.95)

rstLOG <- data.frame(lon= as.numeric(thislog$lon), lat = as.numeric(thislog$lat),z=as.numeric(thislog$Hooks))
coordinates(rstLOG) <- c("lon","lat")
effortLOG <- rasterize(rstLOG, r, 'z',fun=sum) #the column name that you want to plot
q95log<-quantile(effortLOG,probs=.95)

rstLOGN <- data.frame(lon= as.numeric(thislog$lon), lat = as.numeric(thislog$lat),z=1)
coordinates(rstLOGN) <- c("lon","lat")
NLOG <- rasterize(rstLOGN, r, 'z',fun=sum) #the column name that you want to plot
anomEFFORT<-(((effortLOG-effortAIS)/effortLOG)*100)
q95eff<-quantile(anomEFFORT,probs=c(0.05,.95))

eff<-data.frame(pred=as.data.frame(effortAIS)[[1]],obs=as.data.frame(effortLOG)[[1]],gy='LL16')
eff_mod<-lm(eff$pred~eff$obs)
ss<-summary(eff_mod)
# q95<-quantile(per,probs=.95)



image.plot(anomEFFORT,zlim=c(-100,100),col=colrs,ylim=c(-45,20))
map('worldHires',add=T,col='gray',border='gray',fill=T)
legend('topleft',legend='A',bty='n',cex=1.5)

hist(anomEFFORT$layer,main='')
legend('topleft',legend='C',bty='n',cex=1.5)


plot(eff$obs,eff$pred,pch=16,type='p',xlab='Logbook effort (hooks)',ylab='GFW effort (hooks)',cex=1)
abline(eff_mod)
abline(1,1,col='red')
legend('bottomright',legend=paste0('r2=',round(ss$adj.r.squared,digits=2)),bty='n',cex=1.5)## 1degx1deg
legend('topleft',legend='E',bty='n',cex=1.5)

############### 2017
thisais<-LL2017$AIS[which(LL2017$AIS$condition==condition),]
thislog<-LL2017$LOG
# LL2016$AIS <- LL2016$AIS[which(LL2016$AIS$condition==condition),]

gridsize <- 5
ry<-range(as.numeric(thislog$lat),na.rm=T)# Extent of map along the y-axis (latitude)
rx<-range(as.numeric(thislog$lon),na.rm=T)# Extent of map along the x-axis (longitude)
r<-raster(extent(rx,ry),res=gridsize)
r[]<-0
## count the number of records within each cell. this should be per vessel. 
## and the percentage should be over the full time spent in the cell 
rstHOOKS <- data.frame(lon= as.numeric(thisais$lonLog), lat = as.numeric(thisais$latLog),z=1)
over20<-which(rstHOOKS$lat>(-20))
under20<-which(rstHOOKS$lat<(-20))
# replace z level with the average number of hooks found above and below 20S
rstHOOKS[over20,'z']<-3000
rstHOOKS[under20,'z']<-3670
coordinates(rstHOOKS) <- c("lon","lat")
# sum the number of hooks in each cell
effortAIS <- rasterize(rstHOOKS, r, 'z',fun=sum) #the column name that you want to plot
q95ais<-quantile(effortAIS,probs=.95)

rstLOG <- data.frame(lon= as.numeric(thislog$lon), lat = as.numeric(thislog$lat),z=as.numeric(thislog$Hooks))
coordinates(rstLOG) <- c("lon","lat")
effortLOG <- rasterize(rstLOG, r, 'z',fun=sum) #the column name that you want to plot
q95log<-quantile(effortLOG,probs=.95)

rstLOGN <- data.frame(lon= as.numeric(thislog$lon), lat = as.numeric(thislog$lat),z=1)
coordinates(rstLOGN) <- c("lon","lat")
NLOG <- rasterize(rstLOGN, r, 'z',fun=sum) #the column name that you want to plot

anomEFFORT<-(((effortLOG-effortAIS)/effortLOG)*100)
q95eff<-quantile(anomEFFORT,probs=c(0.05,.95))

eff<-data.frame(pred=as.data.frame(effortAIS)[[1]],obs=as.data.frame(effortLOG)[[1]],gy='LL16')
eff_mod<-lm(eff$pred~eff$obs)
ss<-summary(eff_mod)
# q95<-quantile(per,probs=.95)

# image.plot(effortLOG,zlim=c(0,q95ais),col=colrs,ylim=c(-45,20),axes=T,ylab='')
# map('worldHires',add=T,col='gray',border='gray',fill=T)
# 
# image(effortAIS,zlim=c(0,q95ais),col=colrs,ylim=c(-45,20),axes=F,ylab='')
# axis(1,at=seq(30,80,by=10),label=seq(30,80,by=10))
# box("plot",col='black')
# map('worldHires',add=T,col='gray',border='gray',fill=T)

##

image(anomEFFORT,zlim=c(-100,100),col=colrs,ylim=c(-45,20),axes=F)
axis(1,at=seq(30,80,by=10),label=seq(30,80,by=10))
box("plot",col='black')
map('worldHires',add=T,col='gray',border='gray',fill=T)
legend('topleft',legend='B',bty='n',cex=1.5)

hist(anomEFFORT$layer,main='')
legend('topleft',legend='D',bty='n',cex=1.5)



plot(eff$obs,eff$pred,pch=16,type='p',xlab='Logbook effort (hooks)',ylab='GFW effort (hooks)',cex=1)
abline(eff_mod)
abline(1,1,col='red')
legend('bottomright',legend=paste0('r2=',round(ss$adj.r.squared,digits=2)),bty='n',cex=1.5)## 1degx1deg
legend('topleft',legend='F',bty='n',cex=1.5)

