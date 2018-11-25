####################### Pre-processing VMS data
# Author : Chloe Dalleau, Geomatic engineer (IRD)
# Supervisor : Julien Barde (IRD)
# Date : 26/02/2018 
# email : chloe.dalleau@ird.fr
# keywords row bind, object identifier, trajectory identifier, time
#
# wps.des: id = AIS_rbind_without_correction, title= AIS files bind without correction, abstract = Take csv files in a folder and row bind data. The input csv files have to have the same structures. A object identifier is added .A trajectory identifier is added according to time : if two consecitives geolocalisation have a time difference more than max_hours a new trajectory identifier is created.
# wps.in: id = folder_name, type = character, title = Folder path containing the csv files to row bind in a unique csv;
# wps.in: id = output_name, type = character, title =  File path for output data (with extension, like ".csv")
# wps.in: id = time_colname, type = character, title = Column name in the input files for date time
# wps.in: id = max_hours, type = integer, title = Maximal time difference between two consecutive points for trajectories identifiers in hours;
# wps.out: id = output_data, type = csv, title = CSV file containing the input files (row bind) with an object identifier and a trajectory identifier.

################ Packages
all_packages <- c("rstudioapi", "data.table","lubridate","stringr")
for(package in all_packages){
  if (!require(package,character.only = TRUE)) {
    install.packages(package)  
  }
  require(package,character.only = TRUE)
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################ INPUT parameters
## folder name contening all the fine data
file_path_vms <- "input/trajectories_aggregation/pre_processing/vessels_vms_2016.csv"
vms_data <- read.csv(file_path_vms, sep = ",")
id_name <- "id_object"
output_filepath <- "input/trajectories_aggregation/vms_2016_compare_ais.csv" 
## import mapping c_bat /c_cfr
file_name_mapping <- "input/trajectories_aggregation/pre_processing/mapping_cbat_ccfr.csv"
mapping_cbat_ccfr <- read.csv(file_name_mapping, sep = ",")
## extract AIS vessels name
folder_name_ais_data <- "input/trajectories_aggregation/pre_processing/vessels_ais_2016"
list_files <- list.files(folder_name_ais_data)
if (length(list_files)<1){
  warning("Missing AIS data.")
}
file_name_ccfr_ais <- str_replace_all(list_files, ".csv", "")

################ Select the corresponding cbat
subset_mapping <- subset(mapping_cbat_ccfr, mapping_cbat_ccfr$c_cfr %in% file_name_ccfr_ais )
list_id_vms <- subset_mapping$c_bat

output_dataset <- subset(vms_data, vms_data[[id_name]] %in% list_id_vms)


write.csv(output_dataset, output_filepath, row.names=F)

if (length(list_files)<1){
  warning("No output data creates.")
} else {
  cat("Data are stored in : ", output_filepath)
}
