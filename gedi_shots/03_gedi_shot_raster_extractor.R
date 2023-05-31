###############################################################################
###                                                                         ###
### GEDI Processing Script 3:                                               ###
### Polygon Value Extraction Script                                         ###
### Original framework by Neal Swayze 5/3/2022                              ###
### Updated by Jody Vogeler 8/30/22                                         ###
### POC: jody.vogeler@colostate.edu                                         ###
###############################################################################

###_________________________###
### INSTALL / LOAD PACKAGES ###
###_________________________###

library(pacman)
p_load(parallel, foreach, data.table, tidyverse, exactextractr, sp, sf, raster, rgeos, rgdal, mapview, doParallel)

###_______________________________###
### CONFIGURE DIRECTORY STRUCTURE ###
###_______________________________###

### Set your input directory
rootDir <- ("K:/GEDI/")
run = "run03"
###__________________________________________________###
### Creating a set of processing folders for outputs ###
###    MODIFICATION OF LINES BELOW NOT REQURIRED     ###
###__________________________________________________###

### Master Directory Structure Outline
dir.create(file.path(rootDir, "01_gedi_2019"))
dir.create(file.path(rootDir, "02_gedi_2020"))
dir.create(file.path(rootDir, "03_gedi_gee_tiles"))
dir.create(file.path(rootDir, "04_gedi_rf_modeling"))
dir.create(file.path(rootDir, "05_gedi_predicted_maps"))

### 01_gedi_2019 Master Directory Structure Outline
dir.create(file.path(rootDir, "01_gedi_2019/01_study_area_input"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files/01_GEDI_downloads_2A"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files/01_GEDI_downloads_2B"))
dir.create(file.path(rootDir, "01_gedi_2019/03_gedi_filtered_data_csv"))
dir.create(file.path(rootDir, "01_gedi_2019/04_gedi_filtered_data_shp"))
dir.create(file.path(rootDir, "01_gedi_2019/05_gedi_filtered_data_random_samples"))
sampled_footprint_2019_dir <- file.path(rootDir, "01_gedi_2019/05_gedi_filtered_data_random_samples")
dir.create(file.path(rootDir, "01_gedi_2019/06_gedi_summary_plots"))
dir.create(file.path(rootDir, "01_gedi_2019/07_gedi_converted_las_files"))

### 02_gedi_2020 Master Directory Structure Outline
dir.create(file.path(rootDir, "02_gedi_2020/01_study_area_input"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files/01_GEDI_downloads_2A"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files/01_GEDI_downloads_2B"))
dir.create(file.path(rootDir, "02_gedi_2020/03_gedi_filtered_data_csv"))
dir.create(file.path(rootDir, "02_gedi_2020/04_gedi_filtered_data_shp"))
dir.create(file.path(rootDir, "02_gedi_2020/05_gedi_filtered_data_random_samples"))
sampled_footprint_2020_dir <- file.path(rootDir, "02_gedi_2020/05_gedi_filtered_data_random_samples")
dir.create(file.path(rootDir, "02_gedi_2020/06_gedi_summary_plots"))
dir.create(file.path(rootDir, "02_gedi_2020/07_gedi_converted_las_files"))

### 03_gedi_gee_tiles Master Directory Structure Outline
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run,"01_gedi_2016_ortho_tiles"))
in_tile_2016 <- file.path(rootDir, "03_gedi_gee_tiles", run,"01_gedi_2016_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run, "02_gedi_2017_ortho_tiles"))
in_tile_2017 <- file.path(rootDir, "03_gedi_gee_tiles", run, "02_gedi_2017_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run,"03_gedi_2018_ortho_tiles"))
in_tile_2018 <- file.path(rootDir, "03_gedi_gee_tiles", run, "03_gedi_2018_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run, "04_gedi_2019_ortho_tiles"))
in_tile_2019 <- file.path(rootDir, "03_gedi_gee_tiles", run, "04_gedi_2019_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run, "05_gedi_2020_ortho_tiles"))
in_tile_2020 <- file.path(rootDir, "03_gedi_gee_tiles", run, "05_gedi_2020_ortho_tiles")

### 04_gedi_rf_modeling Master Directory Structure Outline
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run, "01_extracted_pixel_values"))
extracted_pixel_value_dir <- file.path(rootDir, "04_gedi_rf_modeling", run, "01_extracted_pixel_values")
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run, "02_merged_pixel_values"))
merged_pixel_values_dir <- file.path(rootDir, "04_gedi_rf_modeling", run,"02_merged_pixel_values")

###___________________________________________________________________________________________________###
### READ IN THE MINIMUM DISTANCE THINNED GEDI SAMPLES FOR EACH YEAR, BUFFER BY FOOTPRINT SIZE (12.5m) ###
###___________________________________________________________________________________________________###

### 2019 GEDI Data
setwd(sampled_footprint_2019_dir)
gedi_2019_spdf <- fread("min_distance_thinned_2A_2B_GEDI_shots_2019-06-01_2019-09-30.csv")
gedi_2019_spdf <- as.data.frame(gedi_2019_spdf)
gedi_2019_spdf <- SpatialPointsDataFrame(cbind(gedi_2019_spdf$coords.x1,gedi_2019_spdf$coords.x2), data=gedi_2019_spdf) #Converting dataframe to spatial points dataframe
proj4string(gedi_2019_spdf) = CRS("+init=epsg:5070") #Assign Coordinate System
gedi_2019_spdf_buff <-  gBuffer(gedi_2019_spdf, width=12.5, byid=TRUE, capStyle="ROUND", joinStyle = "ROUND", quadsegs = 20)
gedi_2019_spdf_buff

### 2020 GEDI Data
setwd(sampled_footprint_2020_dir)
gedi_2020_spdf <- fread("min_distance_thinned_2A_2B_GEDI_shots_2020-06-01_2020-09-30.csv")
gedi_2020_spdf <- as.data.frame(gedi_2020_spdf)
gedi_2020_spdf <- SpatialPointsDataFrame(cbind(gedi_2020_spdf$coords.x1,gedi_2020_spdf$coords.x2), data=gedi_2020_spdf) #Converting dataframe to spatial points dataframe
proj4string(gedi_2020_spdf) = CRS("+init=epsg:5070") #Assign Coordinate System
gedi_2020_spdf_buff <-  gBuffer(gedi_2020_spdf, width=12.5, byid=TRUE, capStyle="ROUND", joinStyle = "ROUND", quadsegs = 20)
gedi_2020_spdf_buff

###____________________________________________________________________________________________________________###
### EXTRACT SPECTRAL VALUES FROM 2019 RASTER PREDICTOR TILES, RETURN MERGED 2019 SPECTRAL DATASET FOR MODELING ###
###____________________________________________________________________________________________________________###

### Get the output GEE predictor tiles for 2019
setwd(in_tile_2019)
list_2019 <- list.files(in_tile_2019, pattern = "tif$", full.names = F)
list_2019

### Auto detect the cores and register parallel session
cores = detectCores()
cores = cores/2
cl <- makeCluster(cores)
registerDoParallel(cl)

### Parallel extraction of spectral data
extracted_pixels_2019 <- foreach(i=1:length(list_2019), .combine =rbind, .inorder = FALSE, .errorhandling="remove") %dopar% {

          ### Get the nessesary packages
          library(raster)
          library(rgdal)
          library(sp)
          library(sf)
          library(rgeos)
          library(exactextractr)
          
          ### Get the first raster from the list
          rasterOptions(maxmemory = 1e+14, chunksize = 1e+14, todisk = FALSE)
          setwd(in_tile_2019)
          raster_layer <- stack(list_2019[[i]]) #Read in the desired raster object as a raster stack
          canProcessInMemory(raster_layer)
          
          ### Get the extent of the raster layer
          raster_extent <- extent(raster_layer) #Get the raster layer extent 
          raster_extent <- as(raster_extent, 'SpatialPolygons') #Convert extent to spatial polygons object
          crs(raster_extent) <- CRS("+init=epsg:5070") #Give it a CRS
          raster_extent

          ### Crop the polygon object to the extent of the raster file
          crop_spdf_buff <- crop(gedi_2019_spdf_buff, raster_extent)
          crop_spdf_buff
          
          ### Extract average pixel values for the intersection of GEDI polygons in the full raster layer
          extracted_values <- exactextractr::exact_extract(raster_layer, crop_spdf_buff, append_cols = TRUE, 'median')
          rm(raster_layer, raster_extent, raster_buff, crop_spdf_buff)
          gc()
          return(extracted_values)
          
        }

### Stop the processing cluster
stopCluster(cl)

### Check the extracted spectral dataset
str(extracted_pixels_2019)

### Write out results to disk
setwd(extracted_pixel_value_dir)
fwrite(extracted_pixels_2019, file = "extracted_predictor_pixels_2019.csv", sep = ",")

### Intermediate Processing Step - read in the filtered data
setwd(extracted_pixel_value_dir)
extracted_pixels_2019 <- fread("extracted_predictor_pixels_2019.csv")

### Subset the data to only contain desired information for RF modeling - drop NAs, remove naming convention
extracted_pixels_2019 <- na.omit(extracted_pixels_2019)
names(extracted_pixels_2019)
extracted_pixels_2019 <- subset(extracted_pixels_2019, select = -c(shot_number, lon_lowestmode, lat_lowestmode, elev_highestreturn, elev_lowestmode, coords.x1, coords.x2))
names(extracted_pixels_2019)
colnames(extracted_pixels_2019) <- gsub("median.", "", colnames(extracted_pixels_2019)) #Drop the mean. prefix from each of the predictor columns
names(extracted_pixels_2019)

### Write out the filtered, RF modeling ready dataset to the 
setwd(merged_pixel_values_dir)
fwrite(extracted_pixels_2019, file = "rf_ready_extracted_predictor_data_2019.csv", sep = ",")

###____________________________________________________________________________________________________________###
### EXTRACT SPECTRAL VALUES FROM 2020 RASTER PREDICTOR TILES, RETURN MERGED 2020 SPECTRAL DATASET FOR MODELING ###
###____________________________________________________________________________________________________________###

### Get the output GEE predictor tiles for 2020
setwd(in_tile_2020)
list_2020 <- list.files(in_tile_2020, pattern = "tif$", full.names = F)
list_2020

### Auto detect the cores and register parallel session
cores = detectCores()
cores = cores/2
cl <- makeCluster(cores)
registerDoParallel(cl)

### Parallel extraction of spectral data
extracted_pixels_2020 <- foreach(i=1:length(list_2020), .combine =rbind, .inorder = FALSE, .errorhandling="remove") %dopar% {
  
  ### Get the nessesary packages
  library(raster)
  library(rgdal)
  library(sp)
  library(sf)
  library(rgeos)
  library(exactextractr)
  
  ### Get the first raster from the list
  rasterOptions(maxmemory = 1e+14, chunksize = 1e+14, todisk = FALSE)
  setwd(in_tile_2020)
  raster_layer <- stack(list_2020[[i]]) #Read in the desired raster object as a raster stack
  canProcessInMemory(raster_layer)
  
  ### Get the extent of the raster layer
  raster_extent <- extent(raster_layer) #Get the raster layer extent 
  raster_extent <- as(raster_extent, 'SpatialPolygons') #Convert extent to spatial polygons object
  crs(raster_extent) <- CRS("+init=epsg:5070") #Give it a CRS
  raster_extent
  
  ### Crop the polygon object to the inward buffered raster layer extent
  crop_spdf_buff <- crop(gedi_2020_spdf_buff, raster_extent) #Crop the gedi footprints to the inward buffered raster extent
  crop_spdf_buff
  
  ### Extract average pixel values for the intersection of GEDI polygons in the full raster layer
  extracted_values <- exactextractr::exact_extract(raster_layer, crop_spdf_buff, append_cols = TRUE, 'median')
  rm(raster_layer, raster_extent, raster_buff, crop_spdf_buff)
  gc()
  return(extracted_values)
  
}

### Stop the processing cluster
stopCluster(cl)

### Check the extracted spectral dataset
str(extracted_pixels_2020)

### Write out results to disk
setwd(extracted_pixel_value_dir)
fwrite(extracted_pixels_2020, file = "extracted_predictor_pixels_2020.csv", sep = ",")

### Intermediate Processing Step - read in the filtered data
setwd(extracted_pixel_value_dir)
extracted_pixels_2020 <- fread("extracted_predictor_pixels_2020.csv")

### Subset the data to only contain desired information for RF modeling - drop NAs, remove naming convention
extracted_pixels_2020 <- na.omit(extracted_pixels_2020)
extracted_pixels_2020 <- subset(extracted_pixels_2020, select = -c(shot_number, lon_lowestmode, lat_lowestmode, elev_highestreturn, elev_lowestmode, coords.x1, coords.x2))
colnames(extracted_pixels_2020) <- gsub("median.", "", colnames(extracted_pixels_2020)) #Drop the mean. prefix from each of the predictor columns
names(extracted_pixels_2020)

### Write out the filtered, RF modeling ready dataset to the 
setwd(merged_pixel_values_dir)
fwrite(extracted_pixels_2020, file = "rf_ready_extracted_predictor_data_2020.csv", sep = ",")
