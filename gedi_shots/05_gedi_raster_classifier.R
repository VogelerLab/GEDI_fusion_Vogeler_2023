###############################################################################
###                                                                         ###
### Turnkey GEDI raster classification script                               ###                            
### Re-Configured by Neal Swayze 5/4/2022                                   ###
### Updated by Jody Vogeler 8/30/22                                         ###
### POC: jody.vogeler@colostate.edu                                         ###
###############################################################################

###_________________________###
### INSTALL / LOAD PACKAGES ###
###_________________________###

#Import Packages
library(pacman)
pacman::p_load(raster, rgdal, sp, sf, mapview, dplyr, rgeos, randomForest, foreach, doParallel, terra)

###_________________________###
### SET DIRECTORY STRUCTURE ###
###_________________________###

### Establish Root Directory
rootDir <- ("K:/GEDI")
rootDir

run = "run03"

###____________________________###
### AUTO CONFIGURE DIRECTORIES ###
###____________________________###

### Master Directory Structure Outline
######################################
dir.create(file.path(rootDir, "01_gedi_2019"))
dir.create(file.path(rootDir, "02_gedi_2020"))
dir.create(file.path(rootDir, "03_gedi_gee_tiles"))
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run))
dir.create(file.path(rootDir, "05_gedi_predicted_maps", run))

### 01_gedi_2019 Master Directory Structure Outline
###################################################
dir.create(file.path(rootDir, "01_gedi_2019/01_study_area_input"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files/01_GEDI_downloads_2A"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files/01_GEDI_downloads_2B"))
dir.create(file.path(rootDir, "01_gedi_2019/03_gedi_filtered_data_csv"))
dir.create(file.path(rootDir, "01_gedi_2019/04_gedi_filtered_data_shp"))
dir.create(file.path(rootDir, "01_gedi_2019/05_gedi_filtered_data_random_samples"))
dir.create(file.path(rootDir, "01_gedi_2019/06_gedi_summary_plots"))
dir.create(file.path(rootDir, "01_gedi_2019/07_gedi_converted_las_files"))

### 02_gedi_2020 Master Directory Structure Outline
###################################################
dir.create(file.path(rootDir, "02_gedi_2020/01_study_area_input"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files/01_GEDI_downloads_2A"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files/01_GEDI_downloads_2B"))
dir.create(file.path(rootDir, "02_gedi_2020/03_gedi_filtered_data_csv"))
dir.create(file.path(rootDir, "02_gedi_2020/04_gedi_filtered_data_shp"))
dir.create(file.path(rootDir, "02_gedi_2020/05_gedi_filtered_data_random_samples"))
dir.create(file.path(rootDir, "02_gedi_2020/06_gedi_summary_plots"))
dir.create(file.path(rootDir, "02_gedi_2020/07_gedi_converted_las_files"))

### 03_gedi_gee_tiles Master Directory Structure Outline
########################################################
dir.create(file.path(rootDir, "03_gedi_gee_tiles",run,"01_gedi_2016_ortho_tiles"))
in_tile_2016 <- file.path(rootDir, "03_gedi_gee_tiles",run,"01_gedi_2016_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles",run,"02_gedi_2017_ortho_tiles"))
in_tile_2017 <- file.path(rootDir, "03_gedi_gee_tiles",run,"02_gedi_2017_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles",run,"03_gedi_2018_ortho_tiles"))
in_tile_2018 <- file.path(rootDir, "03_gedi_gee_tiles",run,"03_gedi_2018_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles",run,"04_gedi_2019_ortho_tiles"))
in_tile_2019 <- file.path(rootDir, "03_gedi_gee_tiles",run,"04_gedi_2019_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles",run,"05_gedi_2020_ortho_tiles"))
in_tile_2020 <- file.path(rootDir, "03_gedi_gee_tiles",run,"05_gedi_2020_ortho_tiles")

### 04_gedi_rf_modeling Master Directory Structure Outline
##########################################################
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run))
rf_model_results_dir <- file.path(rootDir, "04_gedi_rf_modeling", run)
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run, "01_extracted_pixel_values"))
extracted_pixel_value_dir <- file.path(rootDir, "04_gedi_rf_modeling", run, "01_extracted_pixel_values")
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run, "02_merged_pixel_values"))
merged_pixel_values_dir <- file.path(rootDir, "04_gedi_rf_modeling", run,"02_merged_pixel_values")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"03_input_model_variables"))
predictor_datasets_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"03_input_model_variables")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"04_rf_model_objects"))
rf_model_object_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"04_rf_model_objects")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"05_rf_model_importance_plots"))
rf_importance_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"05_rf_model_importance_plots")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"06_rf_model_results"))
rf_result_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"06_rf_model_results")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"07_rf_model_plot_comparisons"))
rf_plot_comp_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"07_rf_model_plot_comparisons")

### 05_gedi_predicted_maps Master Directory Structure Outline
#############################################################
dir.create(file.path(rootDir, "05_gedi_predicted_maps/study_area_shp_5070"))
study_area_dir <- file.path(rootDir, "05_gedi_predicted_maps/study_area_shp_5070")

###############################################################################
####################  DEFINE CLASSIFICATION FUNCTION  #########################
###############################################################################

### Get the list of top three randomForest models for each gedi metric
setwd(rf_result_dir)
top_models <- read.csv("top_rf_model_object_names.csv")
list_rf_models <- setNames(split(top_models, seq(nrow(top_models))), rownames(top_models))
list_rf_models

classify_them_tiles <- function(input_tile_dir){
  
  ### Get the first random forest model from the list of rf models
  for (i in seq(list_rf_models)) {
    
    user_start_time <- Sys.time() #Set the starting time for each model iteration
    
    ### Get the first RF model in the list
    setwd(rf_model_object_dir)
    desired_model_name = list_rf_models[[i]]
    desired_rf_model <- readRDS(desired_model_name[1,])
    desired_rf_model

    ### Get the list of rasters from the desired input directory
    setwd(input_tile_dir)
    raster_list <- list.files(input_tile_dir, pattern = "tif$", full.names = F)
    raster_list
    
    ### Get the input tile directory information
    dir_info <- unlist(strsplit(input_tile_dir, split = "/")) #Split the name into seperate characters
    dir_info <- dir_info[length(dir_info)]
    dir_info <- unlist(strsplit(dir_info, split = "_"))
    dir_info <- dir_info[3]
    dir_info
    
    ### Get the input model information
    model_info <- tools::file_path_sans_ext(list_rf_models[[i]])
    model_info
    
    ### Conditionally creating directories for different models and years based on extracted input tile dir and model information
    if (dir_info == "2016"){
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2016"))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2016", model_info))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps", run,"2016", model_info, "tiles"))
      out_tile_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2016", model_info, "tiles")
      variable_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2016", model_info)}
    
    if (dir_info == "2017"){
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2017"))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2017", model_info))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2017", model_info, "tiles"))
      out_tile_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2017", model_info, "tiles")
      variable_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2017", model_info)}
    
    if (dir_info == "2018"){
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2018"))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2018", model_info))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2018", model_info, "tiles"))
      out_tile_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2018", model_info, "tiles")
      variable_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2018", model_info)}
    
    if (dir_info == "2019"){
       dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2019"))
       dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2019", model_info))
       dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2019", model_info, "tiles"))
       out_tile_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2019", model_info, "tiles")
       variable_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2019", model_info)}

    if (dir_info == "2020"){
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2020"))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2020", model_info))
      dir.create(file.path(rootDir, "05_gedi_predicted_maps",run,"2020", model_info, "tiles"))
      out_tile_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2020", model_info, "tiles")
      variable_dir <- file.path(rootDir, "05_gedi_predicted_maps",run,"2020", model_info)}
      
    out_tile_dir
    variable_dir
    
    ### Get the variables and their IncNodePurity values, sort descending,
    variable_importance <- as.data.frame(desired_rf_model$importance)
    variable_importance <- cbind((as.data.frame(rownames(variable_importance))), variable_importance)
    colnames(variable_importance) <- c("variable", "IncNodePurity")
    rownames(variable_importance) <- NULL
    variable_importance <- variable_importance[order(-variable_importance$IncNodePurity),]
    rownames(variable_importance) <- NULL
    variable_importance <- as.data.frame(t(variable_importance))
    variable_importance_name <- paste0(dir_info, "_", model_info, "_variable_importance", ".csv")
    variable_importance_name
    variable_importance
    
    ### write out sorted variable importance csv to base parent directory for the given model
    setwd(variable_dir)
    write.csv(variable_importance, file = variable_importance_name)
     
    ###############################################################################################
    ### CLASSIFY THE RASTER TILES FOR THE INPUT RANDOM FOREST MODEL IN PARALLEL, WRITE TO DRIVE ###
    ###############################################################################################
    
    ### Initiate processing cluster for parallel processing
    cl <- makeCluster(12)
    registerDoParallel(cl)
    
    ### Run for each loop to classify all of the tiles for a given model, output to new folder
    final_processing_time = foreach(i=1:length(raster_list), .combine = rbind, .inorder = FALSE) %dopar% {
  
      ### Load required packages
      library(raster)
      library(randomForest)
      
      ### Create a list of the input rasters in the input_tile_dir folder
      start_time <- Sys.time()
      setwd(input_tile_dir)
      raster_list <- list.files(input_tile_dir, pattern = "tif$", full.names = F)
      raster_list
      
      ### Get the raster tile as a raster stack (all bands)
      raster_layer <- stack(raster_list[[i]])
      raster_layer
      
      ### Get the output name for each tile
      out_name <- tools::file_path_sans_ext(raster_list[[i]])
      out_name <- paste0(out_name, "_classified.tif")
      out_name
      
      ### Generate new classified raster of the desired gedi metric
      classified <- raster::predict(model=desired_rf_model, object = raster_layer, type = "response", progress = 'window')
      
      ### Write out the classified raster tiles to disk
      setwd(out_tile_dir)
      raster::writeRaster(classified, out_name, overwrite=TRUE)
    
      ### Calculate processing time for the tile
      end_time <- Sys.time()
      time_taken <- paste0(round(as.numeric(difftime(time1 = end_time, time2 = start_time, units = "mins")), 3))
      
      ### Remove unnecessary files and clean up the for each loop
      rm(raster_layer, out_name, classified, start_time, end_time)
      removeTmpFiles()
      
      time_taken
      
      ### Return processing time to the foreach loop
      return(time_taken)
      
    }
    
    stopCluster(cl)
    
    final_processing_time 
    
    ### Calculate final processing time and write to disk
    final_processing_time = as.data.frame(sapply(final_processing_time , function(x) gsub("\"", "", x)))
    colnames(final_processing_time) = "processing_time"
    final_processing_time$processing_time = as.numeric(final_processing_time$processing_time)
    final_processing_time_mins = sum(final_processing_time$processing_time)
    final_processing_time_mins
    setwd(variable_dir)
    write.csv(final_processing_time_mins, file = "02_machine_classification_processing_time_minutes.csv")
    
    #############################################################################################
    ### MERGE THE CLASSIFIED RASTER TILES TOGETHER, MASK TO DESIRED STATES AND WRITE TO DRIVE ###
    #############################################################################################
    
    cat(" Merging the tiles and writing merged raster to disk....")
    
    ### Get a list of classified tiles
    setwd(out_tile_dir)
    raster_list = list.files(out_tile_dir, pattern = "tif$", full.names=TRUE)
    raster_list
    
    ### Set up temporary directory
    dir.create(file.path(out_tile_dir, "temp_dir"))
    temp_dir = file.path(out_tile_dir, "temp_dir")
    rasterOptions(tmpdir = temp_dir)
    rasterOptions()
    
    ### Modify the list to be the actual rasters, not just character strings
    final_raster_list <- lapply(1:length(raster_list), function(x) {raster::stack(raster_list[x])})
    final_raster_list
    
    ### Define the merging function (mean pixel averaging), then mosaic the raster tiles together in memory
    final_raster_list$fun <- mean
    raster_mosaic <- do.call(raster::mosaic, final_raster_list)
    raster_mosaic
    
    ### Mosaic the raster tiles into a single file
    setwd(variable_dir)
    raster_out_name = paste0(model_info, "_merged_", dir_info, ".tif")
    raster_out_name
    raster::writeRaster(raster_mosaic, filename = raster_out_name, overwrite = TRUE)
  
    ### Clean up memory
    unlink(temp_dir, recursive=TRUE)
    rm(raster_mosaic)
    gc()

    # ### Calculate total user processing time for the tile, aka time from start to finish of script in users perspective
    # user_end_time = Sys.time()
    # time_taken <- paste0(round(as.numeric(difftime(time1 = user_end_time, time2 = user_start_time, units = "hours")), 3))
    # setwd(variable_dir)
    # write.csv(time_taken, file = "03_total_user_processing_time_hours.csv")
    
    }
  
}

### Run the function on 2016 tiles
classify_them_tiles(in_tile_2016)

### Run the function on 2017 tiles
classify_them_tiles(in_tile_2017)

### Run the function on 2018 tiles
classify_them_tiles(in_tile_2018)

### Run the function on 2019 tiles
classify_them_tiles(in_tile_2019)

### Run the function on 2020 tiles
classify_them_tiles(in_tile_2020)

