# AISvVMS
The AIS-VMS-Logbook comparison of the Seychelles deep water tuna fleet includes 3 steps: 
1) filter/preprocessing of the data; 
2) trajectory identification and aggregation; and
3) comparisons to the GFW-AIS fishing algorithm.

1. Filtering/preprocessing the raw VMS, AIS, and logbook data: process_meta_total.R
  a. This script sources the preprocessing_all.R script which is itself numerous functions to filter the data, including:
  
    i. dataset_prep  <- standardized column names and date/time formats
  
    ii. vessel_match <- matches vessel names to vessel map names
  
    iii. mmsi_match   <- matches vessel map names to mmsi
  
    iv. overland         <- removes points over land
  
    v. remove_ports <- removes points x meters buffer around a port, defaults to 10000 m buffer, i.e. 10 km
    
    vi. doop_check   <- searches each column of a data frame, except those defined by 'column_list'. finds and removes duplicate rows
    
    vii. speed_check  <- calculates speed and removes the points over a threshold. (max speed per gear), and distances too close (default = 5 m)
    
    viii. check_plot     <- plots the original points with the remaining points overlaid on a map, including ports (green)
    
    ix. processing_metascript <- metascript for VMS and LOG that calls filter scripts i-viii.
    
    x. processing_AIS <- metascript for individual AIS files that calls filter scripts i-viii. 
    
    xi. create_ais_dataframe <- make a dataframe by combining (via rbind) all the individual AIS files.
    
    xii. NOTE: careful of the path to the AISdata
  
  b. Raw files filtered as of November 2018 include Seychelles long line data for 2016 and 2017 and purse seine data for 2016 and 2017.

  c. These data are then used for all subsequent analyses. They are saved into the current active directory as a .csv file with the appropriate name, e.g. LL2016_processed.csv, indicating the processed 2016 longline data. The values of what are removed/filtered that appear in the console should be noted to enter into the ‘fleet_coverage.R’ script (bullet point #3 below)

2. Trajectory identification and aggregation

  a. These analyses are based on Chloe Dalleau’s scripts (https://github.com/cdalleau/geolocalisations_and_trajectories_aggregation)

    i. preprocessing_AIS1.R

    
      (1) Identifies trajectories and further filters AIS data
    
      (2) Final filtered AIS data frames with assigned trajectories to be exported and made available to SFA
    
      (3) Must change the “year” (2016, 2017) and “gear” (LL, PS, SV)
    
    ii. preprocessing_VMS1.R

      (1) Identifies trajectories

      (2) Final filtered VMS data frames with assigned trajectories to be exported and made available to SFA

      (3) Must change the “year” (2016, 2017) and “gear” (LL, PS, SV)

    iii. trajectory_aggregation.R for both VMS and AIS

      (1) Assigns trajectories to a grid,calculates the distance and surface area of trajectories, and aggregates data onto the grid.
  
      (2) Aggregated data frames to be exported and made available to SFA.

    iv. plot_comparison_AIS_VMS.R
      
      (1) Plots the aggregated trajectories, identifying where data are/not coincident, plots the distance and surface area of both VMS and AIS data, and plots a comparison/anomaly plot.

3. Summary figures/statistics
  
  a. fleet_coverage.R

    i. This script creates a figure summarising the values of what was filtered/removed from the raw data, as defined in the first step.

  b. density_map_pings_VMSvAIS.R

    i. A density map of where there are AIS and VMS records

  c. compiled_AIS_NA_maps.R

    i. This script finds the proportion of time that AIS data are available in the cell over the entire year. This gives a metric for AIS data coverage. 

  d. Indi_dist.R

    i. comparing the differences in the distances/lengths of individual trajectories between AIS and VMS data (Figure 9 of the Atlas chapter)





