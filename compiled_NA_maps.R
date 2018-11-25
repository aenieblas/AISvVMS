# Compiled_NA_maps

## AE Nieblas, 24/8/2018
## DESCRIPTION: Calculate the proportion of times a cell is missing AIS data where there is VMS data over all the months of data available
## Based on : plot_comparison_AIS_VMS by Chloe Dalleau

library(pacman)
p_load("rstudioapi", "data.table","rgeos","rgdal", "plyr", "ggplot2", "stringr","maps")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
save_path='output/report/'

inputTable <- expand.grid(Year=c(2016,2017), Gear=c("LL", "PS","SV"))

for (yg in 1:dim(inputTable)[1]){
  
  ## import AIS data
  filename_ais <- paste0("seychelles_",inputTable$Gear[yg],"_",inputTable$Year[yg],"_ais_aggregated_trajectories_",inputTable$Year[yg],"_01_01_",inputTable$Year[yg],"_12_31_0_5deg_1month_SFA.csv")
  filepath_ais <- paste0("output/trajectories_aggregation/",filename_ais)
  dataset_ais <- read.csv(filepath_ais, header = T, sep = ",")
  
  ## import VMS data
  filename_vms <- paste0("seychelles_",inputTable$Gear[yg],"_",inputTable$Year[yg],"_vms_aggregated_trajectories_",inputTable$Year[yg],"_01_01_",inputTable$Year[yg],"_12_31_0_5deg_1month_SFA.csv")
  filepath_vms<- paste0("output/trajectories_aggregation/",filename_vms)
  dataset_vms <- read.csv(filepath_vms, header = T, sep = ",")
  
  
  ## Calculation of normalize distance and normalize surface by period
  # remove existing normalize distance and normalize surface
  list <- c("ndistance","nsurface")
  dataset_ais <- data.table(dataset_ais)
  dataset_vms <- data.table(dataset_vms)
  dataset_ais <- dataset_ais[, -list, with=F]
  dataset_vms <- dataset_vms[, -list, with=F]
  
  # split data by period
  split_data_ais <- split(dataset_ais, dataset_ais$time_start)
  split_data_vms <- split(dataset_vms, dataset_vms$time_start)
  # calculation of normalize distance by period
  ndistance_ais <- lapply(lapply(split_data_ais, "[[", "distance"), function(x) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  ndistance_vms <- lapply(lapply(split_data_vms, "[[", "distance"), function(x) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  # calculation of normalize surface by period
  nsurface_ais <- lapply(lapply(split_data_ais, "[[", "surface"), function(x) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  nsurface_vms <- lapply(lapply(split_data_vms, "[[", "surface"), function(x) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  
  cbind_split_data_ais <- mapply( cbind,split_data_ais,ndistance= ndistance_ais,nsurface=nsurface_ais , SIMPLIFY = F )
  cbind_split_data_vms <- mapply( cbind,split_data_vms,ndistance= ndistance_vms,nsurface=nsurface_vms , SIMPLIFY = F )
  
  # store data crs
  data_crs <- "+init=epsg:4326 +proj=longlat +datum=WGS84"
  
  # extract geometry data for plot extend
  geom_ais <- paste0("GEOMETRYCOLLECTION(",paste(dataset_ais$geom_wkt,collapse =","),")")
  geom_vms <- paste0("GEOMETRYCOLLECTION(",paste(dataset_vms$geom_wkt,collapse =","),")")
  sp_geom_ais <- readWKT(geom_ais)
  sp_geom_vms <- readWKT(geom_vms)
  sp_geom_all <- gUnion(sp_geom_ais,sp_geom_vms)
  proj4string(sp_geom_all)=CRS(data_crs)
  bbox_all <- gEnvelope(sp_geom_all)
  bbox_extent_all <- bbox_all@bbox
  latmin <- bbox_extent_all["y","min"]
  latmax <- bbox_extent_all["y","max"]
  lonmin <- bbox_extent_all["x","min"]
  lonmax <- bbox_extent_all["x","max"]
  
  all_period <- unique(c(names(split_data_ais),names(split_data_vms)))
  
  #colormap
  colrs <- rev(colorRampPalette(brewer.pal(8, 'Spectral'))(100))
  ## download world map
  monde <- map_data("world")
  data_all<-NULL
  for ( period in all_period){
    print(period)
    # period=all_period[this_month]
    
    # merge ais and vms data for a period
    data <- merge(cbind_split_data_ais[[period]],cbind_split_data_vms[[period]], by=c("geom_wkt","time_start","time_end"), all=T, suffixes=c("_ais","_vms"))
    
    # calculate the difference between AIS and VMS data for normalize distance and normalize surface
    data$diff_ndistance <- data$ndistance_vms-data$ndistance_ais
    data$diff_nsurface <- data$nsurface_vms-data$nsurface_ais
    data$na_ais <- is.na(data$ndistance_ais)
    data$na_vms <- is.na(data$ndistance_vms)
    data$na[which(data$na_ais==T & data$na_vms==F)] <- "AIS"
    data$na[which(data$na_vms==T & data$na_ais==F)] <- "VMS"
    data$na[which(data$na_vms==T & data$na_ais==T)] <- "AIS & VMS"
    data$na_ais1[which(data$na_ais==T & data$na_vms==F)] <- 1
    data$na_vms1[which(data$na_ais==T & data$na_vms==F)] <- 1
    
    data_all<-rbind(data_all,data)
  }
  
  data.poly<-NULL
  # prop<-NULL
  for (i in 1:length(unique(data_all$geom_wkt))){
    # find the polygons that are the same
    poly<-data_all[which(data_all$geom_wkt==unique(data_all$geom_wkt)[i]),]
    
    # find proportion of na's for each polygon for the 12 months of the time period.
    data.poly$prop_ais[i]<-sum(poly$na_ais1,na.rm=T)/dim(poly)[1]
    data.poly$prop_vms[i]<-sum(poly$na_ais1,na.rm=T)/dim(poly)[1]
    
    data.poly$time_start[i]<-unlist(lapply(poly$time_start[1], as.character))
    data.poly$time_end[i]<-unlist(lapply(poly$time_end[dim(poly)[1]], as.character))
    print(paste(inputTable$Gear[yg],inputTable$Year[yg],":",i," of ",length(unique(data_all$geom_wkt)),sep=' '))
    
  }
  
  data.poly.df<-as.data.frame(data.poly,stringsAsFactors=FALSE)
  data.poly.df$geom_wkt<-unique(data_all$geom_wkt)
  
  # extract geometry data
  geom <- paste0("GEOMETRYCOLLECTION(",paste(data.poly.df$geom_wkt,collapse =","),")")
  sp_geom <- readWKT(geom)
  proj4string(sp_geom)=CRS(data_crs)
  
  ## pre-processing for ggplot
  data.poly.df$id = rownames(data.poly.df)
  data.poly.df1 = fortify(sp_geom, region="id")
  data.df2 = join(data.poly.df1, data.poly.df, by="id")
  
  ## extract min and max date
  min_date <- min(as.Date(data.df2$time_start))
  max_date <- max(as.Date(data.df2$time_end))
  min_date_file_name <- str_replace_all(min_date, "-", "_")
  max_date_file_name <- str_replace_all(max_date, "-", "_")
  
  save(data.df2,file=paste0(save_path,"NA_data_",inputTable$Gear[yg],"_AIS_VMS_",min_date_file_name,"_",max_date_file_name))
}

min_date_file_name=paste0(inputTable$Year[yg],'_01_01')
max_date_file_name=paste0(inputTable$Year[yg],'_12_31')
load(paste0(save_path,"NA_data_",inputTable$Gear[yg],"_AIS_VMS_",min_date_file_name,"_",max_date_file_name))

ports<-read.csv('/home/anne.elise.nieblas/VMSvAIS/VMSvAIS/final_data/table_ports_jp.csv',header=TRUE,sep=';')

#### CREATE plot : Class of missing data
file_name <- paste0(save_path,"NA_data_",gear,"_AIS_",min_date_file_name,"_",max_date_file_name,".png")
png(file = file_name, width = 500, height = 250)
swbrks <- seq(-40,40,15)
wsbrks <- seq(10,130,20)
swlbls <- unlist(lapply(swbrks, function(x) ifelse(x < 0, paste0(x, "°S"), ifelse(x > 0, paste0(x, "°N"),x))))
wslbls <- unlist(lapply(wsbrks, function(x) ifelse(x < 0, paste0(x, "°W"), ifelse(x > 0, paste0(x, "°E"),x))))

print(
  ggplot() +  
    geom_map(data=monde, map=monde, aes(map_id=region), fill="grey", color="grey", size=0.1) +
    geom_polygon(data=data.df2, aes(x=long,y=lat,group=group,fill=prop_ais), size=0.1) +
    geom_point(data=ports,aes(x=longdec,y=latdec),colour='black')+
    coord_equal() + 
    xlab('')+ylab('')+
    scale_x_continuous(breaks = wsbrks, labels = wslbls, expand = c(0, 0),limits=c(15, 140)) +
    scale_y_continuous(breaks = swbrks, labels = swlbls, expand = c(0, 0),limits=c(-42, 37)) +
    # scale_x_continuous(breaks = wsbrks, labels = wslbls, expand = c(0, 0),limits=c(30, 85)) +
    # scale_y_continuous(breaks = swbrks, labels = swlbls, expand = c(0, 0),limits=c(-35, 30)) +
    
    # scale_x_continuous(breaks = wsbrks, labels = wslbls, expand = c(0, 0),limits=c(lonmin-5, lonmax+5)) +
    # scale_y_continuous(breaks = swbrks, labels = swlbls, expand = c(0, 0),limits=c(latmin-5, 10.5)) +
    theme(axis.text=element_text(size=12),
          text = element_text(color = "#22211d"),
          panel.background = element_rect(fill = "white", color = NA),
          
          plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.5, r=0, unit = "cm") ),
          legend.title = element_text(size=12, face = "bold"),
          legend.text = element_text(size = 12),
          legend.position = "bottom"
    ) +
    
    scale_fill_gradientn(colours=colrs, na.value="white",#guide=FALSE
                         limits=c(0,1),
                         guide = guide_legend(title = "proportion of missing AIS", keyheight = unit(5, units = "mm"), keywidth=unit(20, units = "mm"),
                                              nrow=1,label.position = "bottom", title.position = "top")
    )
)

dev.off()
}

