# process_meta_total.R
## processing metascript sources preprocessing_all, which includes the following:
## 1) dataset_prep <- standardized column names and date/time formats
## 2) vessel_match <- matches vessel names to vessel map names
## 3) mmsi_match   <- matches vessel map names to mmsi
## 4) overland     <- removes points over land
## 5) remove_ports <- removes points x meters buffer around a port, defaults to 10000 m buffer, i.e. 10 km
## 6) doop_check   <- searches each column of a data frame, except those defined by 'column_list'. finds and removes duplicate rows
## 7) speed_check  <- calculates speed and removes the points over a threshold. (max speed per gear), and distances too close (default = 5 m)
## 8) check_plot   <- plots the original points with the remaining points overlaid on a map, including ports (green)
## 9) processing_metascript <- metascript for VMS and LOG that calls filter scripts 1-8
## 10) processing_AIS <- metascript for AIS that calls filter scripts 1-8. 
## 11) create_ais_dataframe <- make a dataframe by combining (rbind) all the individual AIS files




#setwd('final_data') #set to data folder. 
source('../Rscripts/preprocessing_all.R')
############################################### VMS ##################################################
## long lines, vms, 2016
# load raw data
llvms16_raw        <-read.csv('seyllvms2016.csv',sep=';',header=T)
### Change YUTUNA 212 to FULL ALWAYS 168
llvms16_raw$VesselName <- as.character(llvms16_raw$VesselName)
llvms16_raw[llvms16_raw$VesselName=='YUTUNA 212','VesselName'] <- 'FULL ALWAYS 168'

### Change UNO BEST to UNOBEST
llvms16_raw[llvms16_raw$VesselName=='UNO BEST','VesselName'] <- 'UNOBEST'

# process/filter the dataset for column names, date formats, vessel names, mmsi matches to AIS data, points over land, points around ports, duplicate entries, excessive speeds, too short distances
llvms16            <-processing_metascript(raw_dataset=llvms16_raw,data_des='LLVMS2016',gear='Drifting longline',monitoring_system='VMS',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ll.csv',
                                           ais_dir='../AISdata/seychelles_vessel_tracks_2016_2017',
                                           colname_vessel='VesselName',colname_mmsi=NA,colname_time='TimeStamp',
                                           colname_lon='Longitude',colname_lat='Latitude',colname_speed='Speed',
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',year='2016',gearabbr='LL',too_close_dist=5)

## long lines, vms, 2017
llvms17_raw        <-read.csv('seyllvms2017.csv',sep=';',header=T)
### Change YUTUNA 212 to FULL ALWAYS 168
llvms17_raw$VesselName <- as.character(llvms17_raw$VesselName)
llvms17_raw[llvms17_raw$VesselName=='YUTUNA 212','VesselName'] <- 'FULL ALWAYS 168'

### Change UNO BEST to UNOBEST
llvms17_raw[llvms17_raw$VesselName=='UNO BEST','VesselName'] <- 'UNOBEST'

llvms17            <-processing_metascript(llvms17_raw,data_des='LLVMS2017',gear='Drifting longline',monitoring_system='VMS',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ll.csv',
                                           ais_dir='../AISdata/seychelles_vessel_tracks_2016_2017',
                                           colname_vessel='VesselName',colname_mmsi='MMSI',colname_time='TimeStamp',
                                           colname_lon='Longitude',colname_lat='Latitude',colname_speed='Speed',
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',year='2017',gearabbr='LL',too_close_dist=5)
## purse seines, vms 2016

## raw data had incorrect vessel name, not on the map - intertuna 3, not intertuna tres
# psvms16_raw        <-read.csv('seypsvms2016.csv',sep=';',header=T)
# psvms16_raw$VesselName<-gsub('INTERTUNA 3','INTERTUNA TRES',as.character(psvms16_raw$VesselName))
# write.csv(psvms16_raw,'seypsvms2016_inter3_fix.csv')

# load raw data, with correction to intertuna tres
psvms16_raw        <-read.csv('seypsvms2016_inter3_fix.csv')
psvms16            <-processing_metascript(psvms16_raw,data_des='PSVMS2016',gear='Purse seine',monitoring_system='VMS',
                                           ais_dir='../AISdata',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ps.csv',
                                           colname_vessel='VesselName',colname_mmsi='MMSI',colname_time='TimeStamp',
                                           colname_lon='Longitude',colname_lat='Latitude',colname_speed='Speed',
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',year='2016',gearabbr='PS',too_close_dist=5)

## purse seines, vms 2017
psvms17_raw        <-read.csv('seypsvms2017.csv',sep=';',header=T)
psvms17            <-processing_metascript(psvms17_raw,data_des='PSVMS2017',gear='Purse seine',monitoring_system='VMS',
                                           ais_dir='../AISdata',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ps.csv',
                                           colname_vessel='VesselName',colname_mmsi='MMSI',colname_time='TimeStamp',
                                           colname_lon='Longitude',colname_lat='Latitude',colname_speed='Speed',
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',year='2017',gearabbr='PS',too_close_dist=5)

## support vessels, vms 2016
svvms16_raw       <-read.csv('seysvvms2016.csv',sep=';',header=T)
svvms16           <-processing_metascript(svvms16_raw,data_des='SVVMS2016',gear='Supply vessel',monitoring_system='VMS',
                                          ais_dir='../AISdata',
                                          vessel_map_path='seychelles_vessel_mapping_VESSEL_SV2016.csv',
                                          mmsi_map_path='seychelles_vessel_mapping_MMSI_sv.csv',
                                          colname_vessel = 'VesselName',colname_time='TimeStamp', colname_lon='Longitude',
                                          colname_lat = 'Latitude',colname_speed='Speed',create_ifempty = c('mmsi'),
                                          port_buffer_in_m=10000,output_speed_units='knots',
                                          colname_list_to_keep='mmsi',year='2016',gearabbr='SV',too_close_dist=5)

svvms17_raw       <-read.csv('seysvvms2017.csv',sep=';',header=T)
svvms17           <-processing_metascript(svvms17_raw,data_des='SVVMS2017',gear='Supply vessel',monitoring_system='VMS',
                                          ais_dir='../AISdata',
                                          vessel_map_path='seychelles_vessel_mapping_VESSEL_SV2017.csv',
                                          mmsi_map_path='seychelles_vessel_mapping_MMSI_sv.csv',
                                          colname_vessel = 'VesselName',colname_time='TimeStamp', colname_lon='Longitude',
                                          colname_lat = 'Latitude',colname_speed='Speed',create_ifempty = c('mmsi'),
                                          port_buffer_in_m=10000,output_speed_units='knots',
                                          colname_list_to_keep='mmsi',year='2017',gearabbr='SV',too_close_dist=5)

# ########################################### LOGS ###############################################
# ## long lines, log 2016
lllog16_raw        <-read.csv('seyll2016log.csv',sep=';',header=T)
lllog16            <-processing_metascript(lllog16_raw,data_des='LLLOG16',gear='Drifting longline',monitoring_system='LOG',
                                           ais_dir='../AISdata',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ll.csv',
                                           colname_vessel='VesselName',colname_mmsi=NA,colname_time='LogDate',
                                           colname_lon='LonDec',colname_lat='LatDec',colname_speed=NA,
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',effort='Hooks',gearabbr='LL')


## long lines, log, 2017
lllog17_raw        <-read.csv('seyll2017log.csv',sep=';',header=T)
lllog17            <-processing_metascript(lllog17_raw,data_des='LLLOG17',gear='Drifting longline',monitoring_system='LOG',
                                           ais_dir='../AISdata',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ll.csv',
                                           colname_vessel='VesselName',colname_mmsi=NA,colname_time='LogDate',
                                           colname_lon='LonDec',colname_lat='LatDec',colname_speed=NA,
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',effort='Hooks',gearabbr='LL')


## purse seine, log, 2016
pslog16_raw        <-read.csv('seyps2016log.csv',sep=';',header=T)
pslog16            <-processing_metascript(pslog16_raw,data_des='PSLOG16',gear='Purse seine',monitoring_system='LOG',
                                           ais_dir='../AISdata',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ps.csv',
                                           colname_vessel='VesselName',colname_mmsi=NA,colname_time='LogDate',
                                           colname_lon='LonDec',colname_lat='LatDec',colname_speed=NA,
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',effort='Hours',gearabbr='PS')


## purse seine, log, 2017
pslog17_raw        <-read.csv('seyps2017log.csv',sep=';',header=T)
pslog17            <-processing_metascript(pslog17_raw,data_des='PSLOG17',gear='Purse seine',monitoring_system='LOG',
                                           ais_dir='../AISdata',
                                           mmsi_map_path='seychelles_vessel_mapping_MMSI_ps.csv',
                                           colname_vessel='VesselName',colname_mmsi=NA,colname_time='LogDate',
                                           colname_lon='LonDec',colname_lat='LatDec',colname_speed=NA,
                                           create_ifempty=c('mmsi'),port_buffer_in_m=10000,output_speed_units='knots',
                                           colname_list_to_keep='mmsi',effort='Hours',gearabbr='PS')


################################### AIS ################################################
# setwd('..') # RELATIVE TO THE FINAL_DATA DIRECTORY AS SET ABOVE!

library(devtools)
install_github("Displayr/flipTime")

# list the AIS files, files names = mmsi
ais_files<-list.files('AISdata')
# remove file names that != mmsi (i.e., folder names)
ais_files<-ais_files[-grep('LL',ais_files)]
ais_files<-ais_files[-grep('PS',ais_files)]
ais_files<-ais_files[-grep('SV',ais_files)]
ais_files<-ais_files[-grep('JULY',ais_files)]
ais_files<-ais_files[-grep('empty',ais_files)]
# run a loop for each gear type (PS,LL) over the years of data (currently 2016,2017), for all the AIS files available in the directory
gearabbr=c('PS','LL','SV')
gear=c('Purse seine','Drifting longline','Supply vessel')
gearmap=c('ps','ll','sv')
monitoring_system='AIS'
for(g in 1:length(gearmap)){
  for(y in 2016:2017){
    for(f in 1:length(ais_files)){
      print(paste0(gear[g],' ',y,' ',ais_files[f]))
      processing_AIS(ais_files[f],
                     wd='AISdata',
                     ais_dir='AISdata',
                     wdout=paste0(gearabbr[g],y),
                     year=y,
                     data_des=paste0(gearabbr[g],monitoring_system,y),
                     gear=gear[g],
                     monitoring_system=monitoring_system,
                     mmsi_map_path=paste0('seychelles_vessel_mapping_MMSI_',gearmap[g],'.csv'),
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
                     colname_list_to_keep='mmsi')
    }
  }
}


ais_files_1118<-list.files('AISdata/seychelles_vessel_tracks_2016_2017/')
# remove file names that != mmsi (i.e., folder names)


for(g in 1){
  for(y in 2016:2017){
    for(f in 1:length(ais_files_1118)){
      print(paste0(gear[g],' ',y,' ',ais_files_1118[f]))
      processing_AIS(ais_files_1118[f],
                     wd='../AISdata/seychelles_vessel_tracks_2016_2017',
                     ais_dir='../AISdata/seychelles_vessel_tracks_2016_2017',
                     wdout=paste0(gearabbr[g],y),
                     year=y,
                     data_des=paste0(gearabbr[g],monitoring_system,y),
                     gear=gear[g],
                     monitoring_system=monitoring_system,
                     mmsi_map_path=paste0('seychelles_vessel_mapping_MMSI_',gearmap[g],'.csv'),
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
                     colname_list_to_keep='mmsi')
    }
  }
}





# make a data frame from individual AIS files
LLAIS2016_processed<-create_ais_dataframe(gearabbr='LL',year='2016',datatype='data',ais_dir='../AISdata/seychelles_vessel_tracks_2016_2017')
LLAIS2017_processed<-create_ais_dataframe(gearabbr='LL',year='2017',datatype='data',ais_dir='../AISdata/seychelles_vessel_tracks_2016_2017')
PSAIS2016_processed<-create_ais_dataframe(gearabbr='PS',year='2016',datatype='data',ais_dir='../AISdata')
PSAIS2017_processed<-create_ais_dataframe(gearabbr='PS',year='2017',datatype='data',ais_dir='../AISdata')

SVAIS2016_processed<-create_ais_dataframe(gearabbr='SV',year='2016',datatype='data',ais_dir='../AISdata')
SVAIS2017_processed<-create_ais_dataframe(gearabbr='SV',year='2017',datatype='data',ais_dir='../AISdata')

## make ais removals data frame to check for removals
LL2016_rem<-create_ais_dataframe(gearabbr='LL',year='2016',datatype='removals',ais_dir)
LL2017_rem<-create_ais_dataframe(gearabbr='LL',year='2017',datatype='removals',ais_dir)
PS2016_rem<-create_ais_dataframe(gearabbr='PS',year='2016',datatype='removals',ais_dir)
PS2017_rem<-create_ais_dataframe(gearabbr='PS',year='2017',datatype='removals',ais_dir)
SV2016_rem<-create_ais_dataframe(gearabbr='SV',year='2016',datatype='removals',ais_dir)
SV2017_rem<-create_ais_dataframe(gearabbr='SV',year='2017',datatype='removals',ais_dir)

