
####################### Plot AIS and VMS data
# Author : Chloe Dalleau, Geomatic engineer (IRD)
# Supervisors : Paul Taconet (IRD), Julien Barde (IRD)
# Date : 21/02/2018 
# 
# wps.des: id = plot_comparison_AIS_VMS, title = Plot comparison between AIS and VMS data;
# wps.in: id = filename_ais, type = character, title = Name of AIS input data (with extension, like : "boat_2017.csv") ;
# wps.in: id = filename_vms, type = character, title = Name of VMS input data (with extension, like : "boat_2017.csv") ;
# wps.out: id = plot, type = text/zip, title = Plot of normalize data for AIS and VMS for each period, Plot of the difference between AIS and VMS based on normalize data, Plot of missing data; 
#########################

# package 
all_packages <- c("rstudioapi", "data.table","rgeos","rgdal", "plyr", "ggplot2", "stringr","maps")
for(package in all_packages){
  if (!require(package,character.only = TRUE)) {
    install.packages(package)  
  }
  require(package,character.only = TRUE)
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(dir.exists("plot_comparison_AIS_VMS")==F){
  dir.create("plot_comparison_AIS_VMS")
}
gear="LL"
year="2017"
## import AIS data
filename_ais <- paste0("seychelles_",gear,"_",year,"_ais_aggregated_trajectories_",year,"_01_01_",year,"_12_31_0_5deg_1month_SFA.csv")
filepath_ais <- paste0("output/trajectories_aggregation/",filename_ais)
dataset_ais <- read.csv(filepath_ais, header = T, sep = ",")

## import VMS data
filename_vms <- paste0("seychelles_",gear,"_",year,"_vms_aggregated_trajectories_",year,"_01_01_",year,"_12_31_0_5deg_1month_SFA.csv")
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

setwd("plot_comparison_AIS_VMS")
all_period <- unique(c(names(split_data_ais),names(split_data_vms)))


for ( period in all_period){
  period=4
  # merge ais and vms data for a period
  data <- merge(cbind_split_data_ais[[period]],cbind_split_data_vms[[period]], by=c("geom_wkt","time_start","time_end"), all=T, suffixes=c("_ais","_vms"))
  
  # calculte the differance between AIS and VMS data for normalize distance and normalize surface
  data$diff_ndistance <- data$ndistance_vms-data$ndistance_ais
  data$diff_nsurface <- data$nsurface_vms-data$nsurface_ais
  data$na_ais <- is.na(data$ndistance_ais)
  data$na_vms <- is.na(data$ndistance_vms)
  data$na[which(data$na_ais==T & data$na_vms==F)] <- "AIS"
  data$na[which(data$na_vms==T & data$na_ais==F)] <- "VMS"
  data$na[which(data$na_vms==T & data$na_ais==T)] <- "AIS & VMS"
  
  # extract geometry data
  geom <- paste0("GEOMETRYCOLLECTION(",paste(data$geom_wkt,collapse =","),")")
  sp_geom <- readWKT(geom)
  proj4string(sp_geom)=CRS(data_crs)
  
  ## pre-processing for ggplot
  data$id = rownames(data)
  data.poly = fortify(sp_geom, region="id")
  data.df = join(data.poly, data, by="id")
  
  ## download world map
  monde <- map_data("world")
  
  ## extract min and max date
  min_date <- min(as.Date(data$time_start))
  max_date <- max(as.Date(data$time_end))
  min_date_file_name <- str_replace_all(min_date, "-", "_")
  max_date_file_name <- str_replace_all(max_date, "-", "_")
  
  #### CREATE plot : AIS & VMS normalize distance and surface
  all_var_norm <-  data.frame(var=c("ndistance_ais","ndistance_vms", "nsurface_ais","nsurface_vms"), var_name=c("distance","distance", "surface", "surface"),object_name=c("AIS","VMS", "AIS", "VMS"))
  for (id in 1:dim(all_var_norm)[1]){
    var <- as.character(all_var_norm[id,"var"])
    var_name <- as.character(all_var_norm[id,"var_name"])
    object_name <- as.character(all_var_norm[id,"object_name"])
    
    file_name <- paste0("noramlize_",gear,"_",var_name,"_",object_name,"_",min_date_file_name,"_",max_date_file_name,".png")
    png(file = file_name, width = 800, height = 700)
    
    print(
      ggplot() +  
        geom_map(data=monde, map=monde, aes(map_id=region), fill="#f4f8af", color="darkgrey", size=0.1) +
        xlim(lonmin-5, lonmax+5) +
        ylim(latmin-5, latmax+5) +
        geom_polygon(data=data.df, aes(x=long,y=lat,group=group,fill=get(var)),color="black", size=0.1) +
        coord_equal() + 
        theme_void() +
        labs(
          title = paste0("Normalize ",var_name," ",object_name," data \n (", min_date," - ", max_date,")"),
          caption = paste0("Data: SFA and Global Fishing Watch  |   Author: Anne-Elise Nieblas  |  Date :",Sys.Date())
        ) +
        theme(
          text = element_text(color = "#22211d"), 
          plot.background = element_rect(fill = "#f5f5f2", color = NA), 
          panel.background = element_rect(fill = "#c5dae6", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          
          plot.title = element_text(size= 28,face="bold",lineheight=.8, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.caption = element_text( size=15, color = "#4e4d47", margin = margin(b = 0.1, r=0, unit = "cm") ),
          legend.title = element_text(size=20, face = "bold"),
          legend.text = element_text(size = 18),
          legend.position = "bottom"
        ) +
        scale_fill_gradient2(
          low = "darkgreen", high = "darkred",   mid = "#FFD700", midpoint = 0.5, limits=c(0,1) , na.value="white",guide = guide_legend(title = paste("Normalize",var_name, object_name), keyheight = unit(5, units = "mm"), keywidth=unit(25, units = "mm"),  nrow=1,label.position = "bottom", title.position = "top")
        )
    )
    
    dev.off()
  }
  
  #### CREATE plot : Comparison AIS and VMS distance
  all_var_na <-  data.frame(colname=c("diff_ndistance", "diff_nsurface"), var_name=c("distance", "surface"))
  for (id in 1:dim(all_var_na)[1]){
    var <- as.character(all_var_na[id,1])
    var_name <- as.character(all_var_na[id,2])
    file_name <- paste0("comparison_AIS_VMS_normalize_",gear,"_",var_name,"_",min_date_file_name,"_",max_date_file_name,".png")
    png(file = file_name, width = 800, height = 700)
    
    print(
      ggplot() +  
        geom_map(data=monde, map=monde, aes(map_id=region), fill="#f4f8af", color="darkgrey", size=0.1) +
        xlim(lonmin-5, lonmax+5) +
        ylim(latmin-5, latmax+5) +
        geom_polygon(data=data.df, aes(x=long,y=lat,group=group,fill=get(var)),color="black", size=0.1) +
        coord_equal() + 
        theme_void() +
        labs(
          title = paste0("Normalize ",var_name," comparison between AIS and VMS data \n (", min_date," - ", max_date,")"),
          caption = paste0("Data: IRD and Global Fishing Watch  |   Author: ChloÃ© Dalleau  |  Date :",Sys.Date())
        ) +
        theme(
          text = element_text(color = "#22211d"), 
          plot.background = element_rect(fill = "#f5f5f2", color = NA), 
          panel.background = element_rect(fill = "#c5dae6", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          
          plot.title = element_text(size= 28,face="bold",lineheight=.8, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.caption = element_text( size=15, color = "#4e4d47", margin = margin(b = 0.1, r=0, unit = "cm") ),
          legend.title = element_text(size=20, face = "bold"),
          legend.text = element_text(size = 18),
          legend.position = "bottom"
        ) +
        scale_fill_gradient2(
          low = "darkred", high = "darkblue",   mid = "#82C46C", midpoint = 0, limits=c(-1,1) , na.value="white",labels=c("AIS"," ", "identical value"," ", "VMS"),guide = guide_legend(title = "Most represented object", keyheight = unit(5, units = "mm"), keywidth=unit(25, units = "mm"),  nrow=1,label.position = "bottom", title.position = "top")
        )
    )
    
    dev.off()
  }
  
  
  
  #### CREATE plot : Class of missing data
  file_name <- paste0("NA_data_",gear,"_AIS_VMS_",min_date_file_name,"_",max_date_file_name,".png")
  png(file = file_name, width = 800, height = 700)
  
  print(
    ggplot() +  
      geom_map(data=monde, map=monde, aes(map_id=region), fill="#f4f8af", color="darkgrey", size=0.1) +
      xlim(lonmin-5, lonmax+5) +
      ylim(latmin-5, latmax+5) +
      geom_polygon(data=data.df, aes(x=long,y=lat,group=group,fill=na),color="black", size=0.1) +
      coord_equal() + 
      theme_void() +
      labs(
        title = paste0("Class of missing data \n (", min_date," - ", max_date,")"),
        caption = paste0("Data: SFA and Global Fishing Watch  |   Author: Anne-Elise Nieblas  |  Date :",Sys.Date())
      ) +
      theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#c5dae6", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        
        plot.title = element_text(size= 28,face="bold",lineheight=.8, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.caption = element_text( size=15, color = "#4e4d47", margin = margin(b = 0.1, r=0, unit = "cm") ),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size = 18),
        legend.position = "bottom"
      ) +
      scale_fill_discrete(
        breaks=c("AIS","VMS","AIS & VMS"),na.value="white",guide = guide_legend(title = "Missing data", keyheight = unit(5, units = "mm"), keywidth=unit(25, units = "mm"),  nrow=1,label.position = "bottom", title.position = "top")
      )
  )
  
  dev.off()
  
  cat(paste0("\n Plots created for ",period,"."))
  
}

