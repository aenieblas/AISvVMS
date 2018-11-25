## preprocessing_all.R



## dataset_prep.R: standardizes column names and date/time formats
dataset_prep<-function(dataset,monitoring_system,colname_vessel,colname_mmsi,colname_time,colname_lon,colname_lat,colname_speed,create_ifempty){
  
  #' @name dataset_prep
  #' @title Format input data for the function dataset_preprocessing
  #' @description Format input data for the function dataset_preprocessing : changes column names with right column names for
  #' function dataset_preprocessing, standardizing the date/time format. Based on Chloe Dalleau's preprocessing_data_trajectories.R
  #' @param dataset dataframe with geolocalisation objects and containing: vessel name, mmsi, data dimensions,geolocalisation date time , latitude of the position in degree, for longitude of the position in degree, type = data.frame;
  #' @param colname_vessel column name for vessel name in the input data, type = character;
  #' @param colname_mmsi column name for mmsi in the input data, type = character;
  #' @param colname_time column name for time in the input data, type = character;
  #' @param colname_lat column name for latitude in the input data, type = character;
  #' @param colname_lon column name for longitude in the input data, type = character;
  #' @param colname_speed column name for the speed in the input data, type = character;
  #' @param create_ifempty character list of column names to create a column for if they are missing in the input data, type=character;
  #'
  #' @return Return a dataframe with the rigth column name for the function dataset_preprocessing, and remove the unused columns 
  #' 
  #' @author Anne-Elise Nieblas, Based on preprocessing_data_trajectories.R by Chloé Dalleau \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, aggregation, spatio-temporal resolution
  #'
  #' @usage dataset <- dataset_prep(vessel_map,dataset,list_dim_output,colname_vessel,colname_mmsi,colname_source,colname_time,colname_lon,colname_lat,create_ifempty)
  
  
  ### modify the dataset column name and create columns where requested
  if(length(which(create_ifempty=='vessel')==1)){dataset$vessel<-NA
  }else{colnames(dataset)[colnames(dataset)==colname_vessel] <- "vessel"}
  
  if(length(which(create_ifempty=='mmsi')==1)){dataset$mmsi<-NA
  }else{colnames(dataset)[colnames(dataset)==colname_mmsi] <- "mmsi"}
  
  if(length(which(create_ifempty=='time')==1)){dataset$time<-NA
  }else{colnames(dataset)[colnames(dataset)==colname_time] <- "time"}
  
  if(length(which(create_ifempty=='lon')==1)){dataset$lon<-NA
  }else{colnames(dataset)[colnames(dataset)==colname_lon] <- "lon"}
  
  if(length(which(create_ifempty=='lat')==1)){dataset$lat<-NA
  }else{colnames(dataset)[colnames(dataset)==colname_lat] <- "lat"}
  
  if(length(which(create_ifempty=='speed')==1)){dataset$speed<-NA
  }else{colnames(dataset)[colnames(dataset)==colname_speed] <- "speed"} 
  
  date_format_standard="%d/%m/%Y %H:%M:%OS"
  date_format="%Y-%m-%d %H:%M:%S"
  
  ## reformat date/time <- this step can take time, depending on the size of the dataset
  # if(monitoring_system=='AIS'){dataset$time<-dataset$time}else{
  if(is.na(colname_time)==FALSE & (length(grep(gsub('-','/',date_format),date_format_standard))==0)){
    
    library(flipTime)
    tt<-as.character(dataset$time)
    tt<-format(AsDateTime(tt,us.format=FALSE), date_format_standard)
    
    dataset$time<-tt
  }
  # }
  
  return(dataset)
}


## match the vessel names to the vessel map and delete the remaining NAs 
vessel_match<-function(dataset,vessel_map_file_path,monitoring_system){
  #' @name vessel_match
  #' @title match vessel names to official registry 
  #' @description match the vessel names to the official registry (i.e.,vessel map) and delete the remaining NAs 
  #' @param dataset dataframe with geolocalisation objects and containing: vessel name, mmsi,geolocalisation date time , latitude of the position in decimal degree, for longitude of the position in decimal degree, type = data.frame;
  #' @param vessel_map_file_path file path to the vessel map, in which variations of vessel names are mapped to the standard vessel name, type = character;
  #' @param monitoring_system the type of monitoring system of the dataset, i.e., 'LOG','VMS', or 'AIS', type = character;
  #'
  #' @return Return a dataframe with the standard vessel name and remove the remaining NAs from the vessel column 
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, aggregation, spatio-temporal resolution, vessel map
  #'
  #' @usage dataset<-vessel_match(dataset,vessel_map_file_path,monitoring_system)
  
  # load and read vessel map, which gives the different versions of a standard vessel name
  namemap<-read.csv(vessel_map_file_path,sep=',')
  
  #colnames of vesselmap must be "RawVesselName", and "VesselName"
  colnames(namemap)<-c('RawVesselName','VesselName')
  
  # using raw VMS data, there are several different versions of the standard vessel name. replace and add new column "vesselmap"
  if(monitoring_system=='VMS'){
    dataset$vesselmap<-NULL
    for(m in 1:dim(namemap)[1]){
      dataset[which(as.character(dataset$vessel)==as.character(namemap$RawVesselName[m])),'vesselmap']<-as.character(namemap$VesselName[m])
    }
  }
  
  # find rows where vessel is NA
  row_na<-which(is.na(dataset$vesselmap)==TRUE)
  print(paste('number of rows deleted because ',monitoring_system,' vessel not matching the vessel map ',length(row_na),' and total rows ',dim(dataset)[1],sep='',collapse=''))
  
  # remove rows with NA
  if(length(row_na)>0){dataset<-dataset[-row_na,]}
  return(dataset)
}


mmsi_match<-function(dataset,ais_dir,mmsi_map_file_path,monitoring_system){
  #' @name mmsi_match
  #' @title match mmsi names to official registry 
  #' @description match the mmsi names to the official registry (i.e.,mmsi map) and delete the remaining NAs. For AIS data,
  #' it adds a column with the matched vessel names. For VMS data, it adds a column with the matched mmsi
  #' @param dataset dataframe with geolocalisation objects and containing: vessel name and/or mmsi,geolocalisation 
  #' date time , latitude of the position in decimal degree, for longitude of the position in decimal degree, type = data.frame;
  #' @param vessel_map_file_path file path to the mmsi map, in which standard vessel names are mapped to the mmsi, type = character;
  #' @param monitoring_system the type of monitoring system of the dataset, i.e., 'LOG','VMS', or 'AIS', type = character;
  #'
  #' @return Return a dataframe with the standard vessel name and remove the remaining NAs from the vessel column 
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, aggregation, spatio-temporal resolution, vessel map
  #'
  #' @usage dataset<-mmsi_match(dataset,vessel_map_file_path,monitoring_system)
  
  map<-read.csv(mmsi_map_file_path)
  length(unique(na.exclude(map$VesselName)))
  
  dataset$vms_mmsi<-NULL
  for(m in 1:dim(map)[1]){
    dataset[which(as.character(dataset$vessel)==map$VesselName[m]),'vms_mmsi']<-as.character(map$MMSI[m])
  }
  print(paste0('Active VMS with MMSI :',length(unique(na.exclude(dataset$vms_mmsi)))))
  # if(monitoring_system=='AIS'){
  list_file<-list.files(ais_dir) ## AE: FIX THIS TO FINAL AIS FOLDER, or make as option to enter.
  id_plot <- as.factor(str_replace_all(list_file, ".csv", ""))
  id_plot <- as.factor(str_replace_all(id_plot, "track_", ""))
  mmsi_match_map<-match(map$MMSI,id_plot)
  # }
  
  map$match<-map$VesselName # need this?
  map$match[which(is.na(mmsi_match_map)==TRUE)]<-NA # need this?
  map[which(is.na(mmsi_match_map)==TRUE),'MMSI']<-NA
  namemap<-map
  
  ##add an mmsi column, not matched <- AE: not sure this is still useful for VMS

  
  if(monitoring_system=='AIS'){
    dataset$vessel<-NULL
        for(m in 1:dim(namemap)[1]){
          dataset[which(as.character(dataset$mmsi)==as.character(namemap$MMSI[m])),'vessel']<-as.character(namemap$VesselName[m])
        }
        row_na<-which(is.na(dataset$vessel)==TRUE)
      }else{
  
  dataset$mmsi<-NULL
  for(m in 1:dim(namemap)[1]){
    dataset[which(as.character(dataset$vessel)==as.character(namemap$match[m])),'mmsi']<-as.character(namemap$MMSI[m])
  }
  row_na<-which(is.na(dataset$mmsi)==TRUE)
      }
  
   if(length(row_na)>0){
    dataset<-dataset[-row_na,]
    }
  print(paste('number of rows DELETED because ',monitoring_system,' vessel not matching the vessel map mmsi ',length(row_na),' and total rows ',dim(dataset)[1],sep='',collapse=''))
  
  return(dataset)
}


overland<-function(dataset,create_column=TRUE,create_plot=FALSE,remove_points=FALSE){
  #' @name overland
  #' @title Define coordinates that lie overland
  #' @description Define coordinates of the input dataset that lie overland and add a new column where TRUE==LAND and FALSE==WATER
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #' @param create_column whether to add a column that indicates whether a point is over land or not, defaults to TRUE, type = boolean;
  #' @param create_plot whether to produce a map of the results, defaults to FALSE, type = boolean;
  #'
  #' @return Return a dataframe with a new column identifying if the point is overland 
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- overland(dataset_matched)
  
  
  library(maptools)
  data(wrld_simpl)
  # data(wrldHiRes)
  
  ## Create a SpatialPoints object
  set.seed(0)
  points<-cbind(dataset$lon,dataset$lat)
  pts <- SpatialPoints(points, proj4string=CRS(proj4string(wrld_simpl)))
  
  ## Find which points fall over land
  ii <- !is.na(over(pts, wrld_simpl)$FIPS)
  if (create_column==TRUE){
    dataset$overland<-ii
  }
  if(remove_points==TRUE){
    o_dim<-dim(dataset)[1]
    if(length(which(ii)==TRUE)>0){dataset<-dataset[-which(ii==TRUE),]}else{dataset<-dataset}
    print(paste('number of points over land that were deleted : ',length(which(ii==TRUE)),' out of ',o_dim,sep=''))
  }
  
  if(create_plot==TRUE){
    ## Check that it worked
    xlim<-range(dataset$lon,na.rm=T)
    ylim<-range(dataset$lat,na.rm=T)
    plot(wrld_simpl,xlim=xlim,ylim=ylim)
    points(pts, col=1+ii, pch=16,lwd=.5)
    points(pts[which(ii==TRUE)],pch=16,col='blue')
  }
  return(dataset)
  
}

# identifies a buffer of X meters around a port, and removes these points.
remove_ports<-function(dataset,width){
  #' @name remove_ports
  #' @title Define coordinates that lie within a buffer (width) around a defined port
  #' @description Defines and removes coordinates of the input dataset that lie within a buffer (width) around a defined port
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #' @param width the buffer around a port coordinates within which to search for points. units=meters, type = numeric;
  #'
  #' @return Return a dataframe with port coordinates removed
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- remove_ports(dataset,width=10000)

  library(raster)
  library(sp)
  ports<-read.csv('table_ports_jp.csv',header=TRUE,sep=';')
  
  rst<-data.frame(lon=ports$longdec,lat=ports$latdec)
  coordinates(rst) <- c("lon","lat")
  crs(rst)<-CRS('+proj=longlat +datum=WGS84')
  p_buff<-raster::buffer(rst, width=width)
  
  points<-data.frame(lon=dataset$lon,lat=dataset$lat)
  pts <- SpatialPoints(points,proj4string=CRS(proj4string(p_buff)))
  
  result <- as.integer(over(pts, p_buff))
  yes_port<-which(result==1)
  
  print(paste(length(yes_port),' positions removed that were ',width,' meters around a port out of ',dim(dataset)[1],sep=''))
  if(length(yes_port)>0){dataset_port_remove<-dataset[-yes_port,]}else{dataset_port_remove<-dataset}
  
}

impos_positions<-function(dataset){
  #' @name impos_positions
  #' @title Finds and removes impossible positions
  #' @description Finds and removes impossible positions (i.e., not on this earth) from a dataset
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #'
  #' @return Return a dataframe with impossible coordinates removed
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- impos_positions(dataset)
  
  toohighlat<-which(abs(as.numeric(dataset$lat))>=90)
  toohighlon<-which(abs(as.numeric(dataset$lon))>=180)
  print(paste('the number of too high latitudes that are removed ',length(toohighlat),sep=''))
  print(paste('the number of too high longitudes that are removed ',length(toohighlon),sep=''))
  
  if(length(toohighlat)>0){dataset<-dataset[-toohighlat,]}
  if(length(toohighlon)>0){dataset<-dataset[-toohighlon,]}
  return(dataset)
}

#check for duplicates. duplicate is everything same except mmsi
doop_check<-function(dataset,colname_list=c('mmsi')){
  #' @name doop_check
  #' @title Finds and removes duplicate entries
  #' @description Finds and removes duplicated entries from a dataset
  #' @param dataset dataframe ran thru dataset_prep.R containing: vesselmap, mmsi, time,lat and lon, type = data.frame;
  #' @param colname_list column name that should NOT be checked for duplicates, e.g. mmsi, type = character;
  #'
  #' @return Return a dataframe with duplicate entries removed
  #' 
  #' @author Anne-Elise Nieblas \email{anne.elise.nieblas@gmail.com}
  #' @keywords preprocessing, preparation, spatio-temporal resolution
  #'
  #' @usage dataset <- doop_check(dataset,colname_list=c('mmsi'))
  
  
  dataset<-ddply(dataset,.(vessel), function(x) x[order(strptime(x$time,"%d/%m/%Y %H:%M:%OS")),])
  no_mmsi_df<-dataset[,-which(names(dataset) %in% colname_list)]
  dupes_mmsi<-which(duplicated(no_mmsi_df)==TRUE)
  #remove dupes
  print(paste(length(dupes_mmsi),' rows removed as complete duplicates of other rows out of ',dim(dataset)[1],sep=''))
  if(length(dupes_mmsi)>0){dataset<-dataset[-dupes_mmsi,]}
  return(dataset)
}


### calculate speeds (distance/timediff) and removes outliers
## units to output: 'km/h','knots','m/s'
speed_calc<-function(dataset,units,gear,monitoring_system,effort=NA,too_close_dist=5){## speed = distance/time
  
  # make sure that dates are in order for each vessel
  dataset<-ddply(dataset,.(vessel), function(x) x[order(strptime(x$time,"%d/%m/%Y %H:%M:%OS")),])
  d_dim=dim(dataset)[1]
  library(flipTime)
  tt<-as.character(dataset$time)
  date_format_standard="%d/%m/%Y %H:%M:%OS"
  tt<-format(AsDateTime(tt,us.format=FALSE), date_format_standard)
  
  dataset$timestamp<-tt
  ## time
  #make two columns to compare times
  time_comp<-strptime(dataset$time[1:(dim(dataset)[1]-1)],"%d/%m/%Y %H:%M:%OS")
  time_comp2<-strptime(dataset$time[2:dim(dataset)[1]],"%d/%m/%Y %H:%M:%OS")
  timediff<-time_comp2-time_comp
  timediff<-c(NA,timediff)
  dataset$timediff<-timediff
  
  zerodiff<-which(dataset$timediff==0)
  print(paste0('there are ',length(zerodiff),' duplicate timestamps'))
  if(length(zerodiff)>0){
    # ## check that these are not in sequence
    for(z in 1:length(zerodiff)){tt[z]<-zerodiff[z]-zerodiff[(z+1)]}
    if(length(which(tt==(-1)))>0){print('SPEED CALC warning: zerodiffs are sequential. For logbooks, this means that there are >2 entries for one day.')}
    
    # if(monitoring_system!='LOG' | gear!='Purse seine'){
    # average the lat/lon of the zerodiff line with the previous line, and then delete the zerodiff line
    
    first<-zerodiff-1
    if(monitoring_system=='LOG'){
      # check that the catches are different and leave alone if they are.
      diff_catch<-NULL
      xxx=1
      for(z in 1:length(zerodiff)){
        diff_catch[xxx]=length(which(is.na(match(dataset[first[z],c('TotalCatch',effort)],dataset[zerodiff[z],c('TotalCatch',effort)]))==FALSE))
        # print(diff_catch[xxx])
        # if(diff_catch[xxx]>0){print(paste0(dataset[first[z],c('TotalCatch',effort)],' DOES EQUAL ',dataset[zerodiff[z],c('TotalCatch',effort)],collapse=','))}
        # if(diff_catch[xxx]==0){dataset<-dataset[-zerodiff[z],]}
        xxx=xxx+1
      }
      print(paste(length(which(diff_catch==2)),' of ',d_dim,' rows were deleted because the timediff'))
      
      dataset<-dataset[-which(diff_catch==2),]
    }else{
      
      for(z in 1:length(zerodiff)){ 
        dataset[first[z],'lat']<-mean(dataset[zerodiff[z],'lat'],dataset[first[z],'lat'],na.rm=TRUE)
        dataset[first[z],'lon']<-mean(dataset[zerodiff[z],'lon'],dataset[first[z],'lon'],na.rm=TRUE)
      }
      o_dim<-dim(dataset)[1]
      if(length(zerodiff)>0){dataset<-dataset[-zerodiff,]}
      print(paste(length(zerodiff),' of ',o_dim,' rows were deleted because the timediff from the previous row was 0, thus a duplicate (prob from
                  diff source, e.g. ARGOS v Inmarsat. Lats and longs of previous rows were averaged with deleted row.'))
      
    }
  }
  
  
  
  ## distance
  pt1<-cbind(dataset$lon[1:(dim(dataset)[1]-1)],dataset$lat[1:(dim(dataset)[1]-1)])
  pt2<-cbind(dataset$lon[2:(dim(dataset)[1])],dataset$lat[2:(dim(dataset)[1])])
  dataset$distance<-c(NA,pointDistance(pt1,pt2,lonlat=TRUE))
  
  
  # for high-frequency data (i.e. VMS and AIS) find distances <too_close_dist (default = 5 m)
  if(monitoring_system!='LOG'){
    too_close<-which(dataset$distance<too_close_dist)
    # remove distances too_close
    print(paste0(length(too_close),' positions removed as they were <',too_close_dist,'m from previous position'))
    if(length(too_close)>0){dataset<-dataset[-too_close,]}
  }
  
  # speed calculation: distance (m)/time (s)
  dataset$speedcalc<-NA
  if(units=='knots'){
    dataset$speedcalc<-(dataset$distance/dataset$timediff)*1.94384 # m/s converted to knots
  }
  if(units=='km/h'){
    dataset$speedcalc<-(dataset$distance/dataset$timediff)*3.6 # m/s converted to km/h
  }
  if(units=='m/s'){
    dataset$speedcalc<-dataset$distance/dataset$timediff
  }
  negspeed<-which(dataset$speedcalc<0)
  # correct for changes in time-stamp between end of records for one vessel and the start of another
  if(length(negspeed)==length(unique(dataset$vessel[negspeed]))){dataset[negspeed,c("distance","timediff","speedcalc")]<-NA}
  
  ## calculate and note speed outliers
  upperthreshold<-quantile(dataset$speedcalc,na.rm=TRUE)[4]+((quantile(dataset$speedcalc,na.rm=TRUE)[4]-quantile(dataset$speedcalc,na.rm=TRUE)[2])*1.5)
  lowerthreshold<-quantile(dataset$speedcalc,na.rm=TRUE)[2]-((quantile(dataset$speedcalc,na.rm=TRUE)[4]-quantile(dataset$speedcalc,na.rm=TRUE)[2])*1.5)
  
  if(gear=='Drifting longline' & units=='knots'){max_speed=13.5}
  if(gear=='Purse seine' & units=='knots'){max_speed=18}
  if(gear=='Supply vessel' & units=='knots'){max_speed=15}
  
  q999<-quantile(dataset$speedcalc,probs=0.99,na.rm=TRUE)
  tt<-which(dataset$speedcalc>q999)
  # hist(dataset$speedcalc[-tt],xlim=c(0,upperthreshold+1),breaks=10000,main='speed calcs <99,9%tile')
  # abline(v = max_speed, col = "blue", lwd = 2)
  # abline(v = upperthreshold, col = "red", lwd = 2)
  # legend('top',legend=c('gear threshold','IQR threshold'),col=c('blue','red'),pch=16)
  
  ######## remove outliers ######
  i_out<-which(dataset$speedcalc>max_speed)
  
  # i_out<-which(dataset$speedcalc>upperthreshold | dataset$speedcalc<lowerthreshold)
  # major_outliers<-dataset[which(dataset$speedcalc>upperthreshold | dataset$speedcalc<lowerthreshold),]
  # print(paste(length(i_out),' rows of ',o_dim,' have been deleted as they fall above the IQR upper: ',upperthreshold,' ',units,sep=''))
  if(monitoring_system!='LOG'){
    if(length(i_out)>0){dataset<-dataset[-i_out,]}
    print(paste(length(i_out),' rows have been deleted as they fall above the max speed for ',gear,': ',max_speed,' ',units,sep=''))
    
  }
  
  dataset$timestamp<-as.POSIXct(strptime(as.character(dataset$timestamp),
                                         format = "%d/%m/%Y %H:%M:%S"),
                                format = "%d/%m/%Y %H:%M:%S")
  
  # dataset<-dataset[-i_out,]
  return(dataset)
  
}


check_plot<-function(raw,processed,data_des,lims='full',monitoring_system){
  library(maptools)
  library(maps)
  library(mapdata)
  data(wrld_simpl)
  
  ports<-read.csv('table_ports_jp.csv',header=TRUE,sep=';')
  
  set.seed(0)
  
  if(monitoring_system=='VMS'){raw_points<-cbind(raw$Longitude,raw$Latitude)
  xlim<-range(raw$Longitude,na.rm=T)
  ylim<-range(raw$Latitude,na.rm=T)}
  if(monitoring_system=='LOG'){raw_points<-cbind(raw$LonDec,raw$LatDec)
  xlim<-range(raw$LonDec,na.rm=T)
  ylim<-range(raw$LatDec,na.rm=T)}
  if(monitoring_system=='AIS'){raw_points<-cbind(raw$lon,raw$lat)
  
  GFW_raw<-raw[which(raw$nnet_score==1),c('lon','lat')]
  GFW_processed<-processed[which(processed$nnet_score==1),c('lon','lat')]
  
  xlim<-range(raw$lon,na.rm=T)
  ylim<-range(raw$lat,na.rm=T)
  }
  
  processed_points<-cbind(processed$lon,processed$lat)
  
  rpts <- SpatialPoints(raw_points, proj4string=CRS(proj4string(wrld_simpl)))
  ppts<-SpatialPoints(processed_points, proj4string=CRS(proj4string(wrld_simpl)))
  
  if(lims=='full'){
    xlim<-xlim
    ylim<-ylim}
  if(lims=='seychelles'){
    xlim<-c(54.5,56)
    ylim<-c(-4,-5)
  }
  
  plot(wrld_simpl,xlim=xlim,ylim=ylim,main=data_des)
  points(ports[,c('longdec','latdec')],pch=15,col='green')
  points(rpts, pch=16,lwd=.5,col='blue')
  points(ppts, pch=16,lwd=.5,col='red')
  lines(wrld_simpl)
  # legend('bottom',legend=c('ais','vms','log'),col=c('red','black','blue'),pch=15,horiz=TRUE)
  legend('topleft',legend=c(paste('Raw: n=',dim(raw)[1],sep=''),paste('Processed: n=',dim(processed)[1],sep='')),
         col=c('blue','red'),pch=15)
  
  if(monitoring_system=='AIS'){
    points(GFW_raw,pch=16,col='cyan')
    points(GFW_processed,pch=16,col='pink')
  }
  
}

## make ais data frame from individual processed data:
create_ais_dataframe<-function(gearabbr,year,datatype='data',ais_dir){
  # define the directory where AIS data are located
  dir=paste0(ais_dir,'/',gearabbr,year,'/',datatype)
  # list the files within the directory
  files<-list.files(paste(getwd(),dir,sep='/'))
  # combine each separate file into one big data frame
  dataset<-do.call("rbind", lapply(paste(getwd(),dir,files,sep='/'), function(x) read.csv(x)))
  # save the data frame
  if(datatype=='data'){write.csv(dataset,file=paste0(ais_dir,'/',gearabbr,'AIS',year,'.csv'),row.names=FALSE) }
  if(datatype=='removals'){write.csv(dataset,file=paste0(ais_dir,'/','removals_summary_',gearabbr,'AIS',year,'.csv'),row.names=FALSE) }
  
  return(dataset)
}


#' 
#' create_ais_dataframe<-function(vessel_map,gear,wd){
#'   
#'   #' @name create_ais_dataframe
#'   #' @title Create dataframe with AIS input data 
#'   #' @description Create dataframe from multiple different AIS input files to then be used in dataset_prep.R. Assumes that AIS data are all in the same folder. 
#'   #' Assumes each file name is the mmsi.csv
#'   #' @param vessel_map dataframe of vessel information: vessel name, mmsi, gear type, other vessel information, type = data.frame;
#'   #' @param gear character string of the gear type, using the gear types from vessel_map, type = character;
#'   #' @param wd directory where files are stored, type = character;
#'   #'
#'   #' @return Return a dataframe of AIS data, including only those that have a matching mmsi in vessel_map
#'   #' 
#'   #' @author Anne-Elise Nieblas,  \email{anne.elise.nieblas@gmail.com}
#'   #' @keywords preprocessing, preparation, aggregation, spatio-temporal resolution
#'   #'
#'   #' @usage dataset<-create_ais_dataframe(vessel_map,gear,wd)
#'   
#'   
#'   # vessel_map<-vessel_map[vessel_map$GEAR==gear,]## AEN: not super generic
#'   vessel_map<-read.csv(vessel_map_file,sep=',',header=TRUE)
#'   
#'   
#'   validAIS<-unique(na.exclude(vessel_map$MMSI))
#'   files<-paste(validAIS,'.csv',sep='')
#'   
#'   # ## compile data frame, skip empty files (68octets)
#'   # dataset<-do.call("rbind", lapply(files, function(x) {if(file.size(x)>=69){
#'   #   read.csv(x)}
#'   # }))
#'   
#'   dataset<-do.call("rbind", lapply(paste(getwd(),wd,files,sep='/'), function(x) {if(file.size(paste(wd,x,sep='/'))>=69){
#'     read.csv(paste(wd,x,sep='/'))}
#'   }))
#'   
#'   return(dataset)
#' }
#' 

# # compiles summaries of what was removed by processing/filtering scripts
# check_removals_dataframe<-function(vessel_map,gear,wd){
#   
#   # vessel_map<-vessel_map[vessel_map$GEAR==gear,]## AEN: not super generic
#   # vessel_map<-read.csv(vessel_map_file,sep=',',header=TRUE)
#   # validAIS<-unique(na.exclude(vessel_map$MMSI))
#   # files<-paste(validAIS,'.csv',sep='')
#   files<-list.files(paste(getwd(),wd,paste0(gearabbr,year),sep='/'))
#   files<-files[-grep('NO_mmsi_removals_6',files)]
#   # files<-files[-grep(paste0('_',year),files)]
#   files<-files[-grep('data',files)]
#   dataset<-do.call("rbind", lapply(paste(getwd(),wd,paste0(gearabbr,year),files,sep='/'), function(x) read.csv(x)))
#   return(dataset)
# }

## metascript to process VMS and logbook data frames
processing_metascript<-function(raw_dataset, data_des='LLVMS17',
                                gear='Drifting longline',# |"Purse seine" 
                                ais_dir='AISdata',
                                monitoring_system='VMS', 
                                vessel_map_path=NA,
                                mmsi_map_path='seychelles_vessel_mapping_MMSI_ll.csv',
                                colname_vessel='VesselName',
                                colname_mmsi='MMSI',
                                colname_time='TimeStamp',
                                colname_lon='Longitude',
                                colname_lat='Latitude',
                                colname_speed='Speed',
                                create_ifempty=NA,
                                port_buffer_in_m=10000,
                                output_speed_units='knots',
                                colname_list_to_keep='mmsi',
                                effort=NA,year='2016',gearabbr=NA,
                                too_close_dist=5
){
  library(pacman)
  p_load('plyr','stringr')
  # source('preprocessing_all.R')
  ## 2017 - LONG LINES
  total_dim  <-dim(raw_dataset)[1]
  prep       <-dataset_prep(raw_dataset,
                            colname_vessel = colname_vessel,colname_mmsi=colname_mmsi,colname_time=colname_time,colname_lon=colname_lon,colname_lat=colname_lat,
                            colname_speed=colname_speed,create_ifempty=create_ifempty)
  if(!is.na(vessel_map_path)){ prep<-vessel_match(prep,vessel_map_path,'VMS')}
  # prep       <-mmsi_match(prep,ais_dir,mmsi_map_path,monitoring_system,gear,year,gearabbr)
  
  
  prep       <-doop_check(prep,colname_list=colname_list_to_keep)
  doop_dim   <-dim(prep)[1]; doops_rem<-total_dim-doop_dim
  
  prep       <-mmsi_match(prep,ais_dir,mmsi_map_path,monitoring_system)
  mmsi_dim<-dim(prep)[1]; mmsi_rem<-doop_dim-mmsi_dim
  
  prep       <-overland(prep,create_column = FALSE,remove_points=TRUE)
  # check removals
  overland_dim  <-dim(prep)[1]; overland_rem<-mmsi_dim-overland_dim
  
  prep       <-remove_ports(prep,width=port_buffer_in_m) 
  ports_dim  <-dim(prep)[1]; port_rem<-overland_dim-ports_dim
  
  prep       <-speed_calc(prep,units=output_speed_units,gear=gear,monitoring_system=monitoring_system,effort=effort,too_close_dist=5)
  speed_dim  <-dim(prep)[1]; speed_rem<-ports_dim-speed_dim
  
  test       <-impos_positions(prep)
  impos_dim  <-dim(prep)[1]; impos_rem<-speed_dim-impos_dim
  
  removals   <-data.frame(total=total_dim, mmsi=mmsi_rem,land=overland_rem,ports=port_rem,doops=doops_rem,
                          speed=speed_rem, impos=impos_rem)
  write.csv(removals,file=paste0('removals_',data_des,'.csv'),row.names=FALSE)
  check_plot(raw_dataset,prep,data_des=data_des,lims='full',monitoring_system=monitoring_system)
  
  # save, write
  processed  <-prep
  # save(processed,file=paste0(data_des,'_processed'))
  write.csv(processed,file=paste0(data_des,'_processed.csv'),row.names=FALSE)
  rm(prep)
  return(processed)
  # check_plot(processed,processed,data_des='LL VMS 2017',lims='full',monitoring_system='VMS')
}


## metascript to process individual AIS files
processing_AIS<-function(AIS_file, wd='AISdata',wdout='LL2016',year=2016, data_des='LLAIS16',
                         ais_dir='AISdata',
                         gear='Drifting longline',# |"Purse seine" 
                         monitoring_system='AIS', 
                         mmsi_map_path='seychelles_vessel_mapping_MMSI_ll.csv',
                         colname_vessel=NA,
                         colname_mmsi='mmsi',
                         colname_time='timestamp',
                         colname_lon='lon',
                         colname_lat='lat',
                         colname_speed='ais_speed',
                         create_ifempty=NA,
                         port_buffer_in_m=10000,
                         output_speed_units='knots',
                         too_close_dist=5,
                         colname_list_to_keep='mmsi',gearabbr=NA){
  library(stringr)
  library(flipTime)
  library(plyr)
  
  
  this_file_name <-str_replace_all(AIS_file, ".csv", "")
  this_file_name <-str_replace_all(this_file_name, "track_", "")
  raw_dataset    <-NULL
  prep           <-NULL
  raw_dataset    <-read.csv(paste(wd,AIS_file,sep='/'))
  
  if(dim(raw_dataset)[1]==0){print(paste0(AIS_file,' : file empty'))
    
  }else{
    
    total_dim    <-dim(raw_dataset)[1]
    prep         <-dataset_prep(raw_dataset,
                                colname_vessel = colname_vessel,colname_mmsi=colname_mmsi,
                                colname_time=colname_time,colname_lon=colname_lon,colname_lat=colname_lat,
                                colname_speed=colname_speed,create_ifempty=create_ifempty)
    
    # select the year of interest from within each file
    justyear     <- which(format(strptime(as.character(prep$time), "%d/%m/%Y"),"%Y")==year)
    prep         <- prep[justyear,]
    year_dim     <- dim(prep)[1]
    
    if(year_dim>0){
      # prep       <-vessel_match(prep,'seychelles_vessel_mapping_VMS_LL2017.csv','VMS')
      prep         <-mmsi_match(prep,ais_dir,mmsi_map_path,monitoring_system)
      mmsi_dim     <-dim(prep)[1]; mmsi_rem<-year_dim-mmsi_dim
      
      if(mmsi_dim>0){
        prep         <-overland(prep,create_column = FALSE,remove_points=TRUE)
        
        overland_dim <-dim(prep)[1]; overland_rem<-mmsi_dim-overland_dim
        
        if(overland_dim>0){
          prep         <-remove_ports(prep,width=port_buffer_in_m) 
          ports_dim    <-dim(prep)[1]; port_rem<-overland_dim-ports_dim
          
          prep         <-doop_check(prep,colname_list=colname_list_to_keep)
          doop_dim     <-dim(prep)[1]; doops_rem<-ports_dim-doop_dim
          
          prep         <-speed_calc(prep,units=output_speed_units,gear=gear,monitoring_system=monitoring_system,too_close_dist=too_close_dist)
          speed_dim    <-dim(prep)[1]; speed_rem<-doop_dim-speed_dim
          
          test         <-impos_positions(prep)
          impos_dim    <-dim(prep)[1]; impos_rem<-speed_dim-impos_dim
          
          ## combine and save all the removal information
          removals     <-data.frame(total=total_dim,year=year_dim,mmsi=mmsi_rem,land=overland_rem,ports=port_rem,
                                    doops=doops_rem,speed=speed_rem, impos=impos_rem)
          
          if(dir.exists(paste(wd,wdout,'removals',sep='/'))==FALSE){dir.create(paste(wd,wdout,'removals',sep='/'),recursive=TRUE)}
          
          write.csv(removals,file=paste(wd,wdout,'removals',paste0('removals_',this_file_name,'_',wdout,'.csv'),sep='/'),row.names=FALSE)
          
          ## check the plot of the original and filtered data points to see if sensible
          check_plot(raw_dataset[justyear,],prep,data_des=this_file_name,lims='full',monitoring_system=monitoring_system)
          
          # save, write filtered data
          processed    <-prep
          save(processed,file=paste0(this_file_name,'_processed_',year))
          if(dir.exists(paste(wd,wdout,'data',sep='/'))==FALSE){dir.create(paste(wd,wdout,'data',sep='/'),recursive=TRUE)}
          
          write.csv(processed,file=paste(wd,wdout,'data',paste0(this_file_name,'_processed_',wdout,'.csv'),sep='/'),row.names=FALSE)
          rm(prep)
          # return(processed)
          
        }else{print(paste0('all points transmitted are on land ',this_file_name,' ',wdout))}
        
      }else{print(paste0('mmsi does not match the mmsi map ',this_file_name,' ',wdout))
        removals   <-data.frame(total=total_dim,year=year_dim,mmsi=mmsi_rem)
        write.csv(removals,file=paste(wd,wdout,paste0('NO_mmsi_removals_',this_file_name,'_',wdout,'.csv'),sep='/'),row.names=FALSE)
      }
      
    }
  }
}


## make ais data frame from individual processed data:
create_ais_dataframe<-function(gearabbr,year,datatype='data'){
  # define the directory where AIS data are located
  dir=paste0('AISdata/',gearabbr,year,'/',datatype)
  # list the files within the directory
  files<-list.files(paste(getwd(),dir,sep='/'))
  # combine each separate file into one big data frame
  dataset<-do.call("rbind", lapply(paste(getwd(),dir,files,sep='/'), function(x) read.csv(x)))
  # save the data frame
  if(datatype=='data'){write.csv(dataset,file=paste0(gearabbr,'AIS',year,'.csv'),row.names=FALSE) }
  if(datatype=='removals'){write.csv(dataset,file=paste0('removals_summary_',gearabbr,'AIS',year,'.csv'),row.names=FALSE) }
  
  return(dataset)
}
