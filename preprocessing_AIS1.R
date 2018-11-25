####################### Pre-processing for AIS data
# Author : Chloe Dalleau, Geomatic engineer (IRD)
# Supervisor : Julien Barde (IRD)
# Date : 26/02/2018 
# email : chloe.dalleau@ird.fr
# keywords row bind, object identifier, trajectory identifier, time
#
# wps.des: id = preprocessing_AIS, title= Pre-processing for AIS data, abstract = Take csv files in a folder and row bind data. The input csv files have to have the same structures. A object identifier is added .A trajectory identifier is added according to time : if two consecitives geolocalisation have a time difference more than max_hours a new trajectory identifier is created.
# wps.in: id = folder_name, type = character, title = Folder path containing the csv files to row bind in a unique csv;
# wps.in: id = output_name, type = character, title =  File path for output data (with extension, like ".csv")
# wps.in: id = time_colname, type = character, title = Column name in the input files for date time
# wps.in: id = max_hours, type = integer, title = Maximal time difference between two consecutive points for trajectories identifiers in hours;
# wps.out: id = output_data, type = csv, title = CSV file containing the input files (row bind) with an object identifier and a trajectory identifier.

################ Packages
library(pacman)
p_load('rstudioapi','data.table','lubridate','stringr','SDLfilter','rgdal')
# all_packages <- c("rstudioapi", "data.table","lubridate","stringr","SDLfilter", "rgdal")
# for(package in all_packages){
#   # if (!require(package,character.only = TRUE)) {
#   #   install.packages(package)  
#   # }

#   require(package,character.only = TRUE)
# }
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# source('preprocessing_all.R')

year='2017'
gear='SV'
################ INPUT parameters
## folder name contening all the fine data: "vessels_ais_LL_2016" | "vessels_ais_LL_2017" | "vessels_ais_PS_2016" |"vessels_ais_PS_2017"
folder_name <- paste0("input/trajectories_aggregation/pre_processing/vessels_ais_",gear,"_",year)# ais 2016 and 2017 are together
## output file name : "ais_2016_LL_compare_vms.csv" | "ais_2017_LL_compare_vms.csv" | "ais_2016_PS_compare_vms.csv| "ais_2017_PS_compare_vms.csv"
output_name <- paste0("input/trajectories_aggregation/ais_",gear,"_",year,"_compare_vms.csv")
filter_name <- paste0("input/trajectories_aggregation/ais_",gear,"_",year,"_filters.csv")
## time column name in the input data
time_colname <- "timestamp"
## maximal time difference between two consecutive points for trajectories identifiers
max_hours <- 24


# To use the function trajectories_aggregation, used SDLfilter as preprocessing is recommended for boats
apply_SDLfilter = T
if (apply_SDLfilter==T){
  SDLfilter_stepdist = 0.005
  SDLfilter_steptime = 0
  SDLfilter_conditional = F
  SDLfilter_vmax = 38
  SDLfilter_maxvlp = 38
  SDLfilter_qi = 4
}


################ Pre-processing plot 
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
system("unzip world_shape_file.zip")
my_spdf=readOGR( dsn= getwd() , layer="TM_WORLD_BORDERS_SIMPL-0.3")
if(dir.exists("plot_AIS")==F){
  dir.create("plot_AIS")
}


################ Treatment
## Initialisation
list_files <- list.files(folder_name)
# list_files<-str_replace_all(list_files_fullname, "_processed_2016", "")
ais_fine_data <- NULL
filter_amount <-NULL
id_bat <- 0
## Loop to create objects and trajectories identifiers for each file in the folder
for (id in list_files){
  ## import data from the selected file
  file_path <- paste(folder_name,id, sep = "/")
  raw_data <- data.frame(read.csv(file_path))
  
  ## if this file is not empty
  if (dim(raw_data)[1]>0){
    id_bat <- id_bat+1
    cat(paste0("\n file ", id," ... "))
    data <- cbind(id=rep(id_bat, dim(raw_data)[1]), id_traj=NA, raw_data)
    
    data[time_colname] <- as_datetime(data[[time_colname]])
    # colnames(data)[ colnames(data)==time_colname] <- "time"
    prev_time <- as_datetime(c(data[[time_colname]][1], data[[time_colname]][1:(length(data[[time_colname]])-1)]), tz= "UTC")
    diff_time <- (data[[time_colname]] - prev_time)
    data <- data.table(data)
    
    ## store positions where times difference are more than max_hours
    positions <- c(1,which(diff_time>max_hours*3600),length(diff_time))
    ## Modify the trajectory identifier
    id_traj=0
    for (id_pos in 1:(length(positions)-1)){
      id_traj =id_traj+1
      data$id_traj[positions[id_pos]:positions[id_pos+1]] <- id_traj
    }
    
    list_dim <- c("id","id_traj",colnames(raw_data))
    data <- data[,list_dim, with=F]
    dimprefilter<-dim(data)[1]
    if (apply_SDLfilter==T){
      colnames(data)[ colnames(data)==time_colname] <- "DateTime"
      if (!("qi" %in% colnames(data))){
        data$qi <- SDLfilter_qi
      }
      data <- data.frame(data)
      data <- dupfilter(data, step.time = SDLfilter_steptime, step.dist = SDLfilter_stepdist, conditional = SDLfilter_conditional)
      data <- ddfilter(data, vmax=SDLfilter_vmax, maxvlp=SDLfilter_maxvlp)
      
      colnames(data)[ colnames(data)=="DateTime"] <- time_colname
      
      # id_plot <- str_replace_all(id, paste0("_processed_",year,".csv"), "")
      # 
      # file_path_plot <- paste0("plot_AIS/",id_plot,".jpeg")
      # jpeg(file_path_plot,width = 810,height = 560)
      # 
      # pourcent_remove <- 100-round((dim(data)[1]*100)/dim(raw_data)[1])
      # 
      # par(mar=c(5.5,5,2,1))
      # LatLong <- data.frame(Y=data$lat, X=data$lon)
      # coordinates(LatLong) <- ~X+Y
      # proj4string(LatLong) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      # sub_title <- paste0("raw data: black      filtered data : red       pourcentage of remove data : ", pourcent_remove,"%")
      # plot(LatLong, pch=21, col="black")
      # axis(1)
      # axis(2, las=2)
      # box()
      # title(main =id, sub=sub_title)
      # mtext("Longitude (degree)", side=1, line=2.5)
      # mtext("Latitude (degree)", side=2, line=3.5)
      # 
      # plot(my_spdf,border="grey", add=T)
      # 
      # # par(mar=c(6,4,2,2))
      # LatLong <- data.frame(Y=data$lat, X=data$lon)
      # coordinates(LatLong) <- ~X+Y
      # proj4string(LatLong) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      # points(LatLong, pch=21, col="red", bg="red")
      # 
      # dev.off() 
    }
    dimpostfilter<-dim(data)[1]
    
    data_output <- data.table(data)
    data_output <- data_output[,list_dim, with=F]
    
    ais_fine_data <- rbind(ais_fine_data,data_output)
    
    
    filts<-dimprefilter-dimpostfilter
    filter_amount<-rbind(filter_amount,cbind(filts,dimprefilter))
    
    
    
    output_filepath <- paste0(output_name)
    write.csv(filter_amount,filter_name,row.names=F)
    write.csv(ais_fine_data, output_filepath, row.names=F)
  }
  
  cat(paste0(" ok \n"))
}

if (length(list_files)<1){
  warning("Missing data.")
} else {
  cat("Data are stored in : ", output_name)
}

## check filter amounts
filrem<-as.data.frame(read.csv(filter_name))
o_dim<-sum(filrem$dimprefilter,na.rm=T)
nfilt<-sum(filrem$filts,na.rm=T)
print(paste0('filtered ',nfilt,' of ',o_dim,' records'))

