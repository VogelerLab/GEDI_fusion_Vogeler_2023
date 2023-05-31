###########################################################
###                                                     ###
### MASTER GEDI PREDICTOR STACK TILES SCRIPT            ###
### AUTOMATICALLY COMBINES RASTER TILES FOR INPUT YEAR  ###
### Original framework by N. Swayze;                    ###
### Additional modifications and upkeep by J. Vogeler   ###
### POC: jody.vogeler@colostate.edu                     ###
###########################################################

###________________________###
### Load required packages ###
###________________________###

library(raster)
library(terra)
library(foreach)
library(doParallel)

###____________________________________________________________________________________________###
### DEFINE MASTER FUNCTION TO CONFIGURE DIRECTORIES, THEN STACK PREDICTOR TILES FOR INPUT YEAR ###
###____________________________________________________________________________________________###

run = "run03" # For multiple runs, set a run number here

stack_rasters = function(desired_year){
  
  #_____________________________#
  # CONFIGURE INPUT DIRECTORIES #
  #_____________________________#
  # These Landsat, Sentinel-1, LCMS disturbance, topography and bioclimatic predictors were created in GEE and exported for local modeling
  
  setwd("K:/GEDI_predictor_tiles")
  rootdir <- file.path("K:/GEDI_predictor_tiles")
  rootdir
  
  dir.create(file.path(rootdir, "topo"))
  topo_input <- file.path(rootdir, "topo")
  
  dir.create(file.path(rootdir, "sar_2016"))
  sar_2016_input <- file.path(rootdir, "sar_2016")
  
  dir.create(file.path(rootdir, "sar_2017"))
  sar_2017_input <- file.path(rootdir, "sar_2017")
  
  dir.create(file.path(rootdir, "sar_2018"))
  sar_2018_input <- file.path(rootdir, "sar_2018")
  
  dir.create(file.path(rootdir, "sar_2019"))
  sar_2019_input <- file.path(rootdir, "sar_2019")
  
  dir.create(file.path(rootdir, "sar_2020"))
  sar_2020_input <- file.path(rootdir, "sar_2020")
  
  dir.create(file.path(rootdir, "landtrendr_2016"))
  landtrender_2016_input = file.path(rootdir, "landtrendr_2016")
  
  dir.create(file.path(rootdir, "landtrendr_2017"))
  landtrender_2017_input = file.path(rootdir, "landtrendr_2017")
  
  dir.create(file.path(rootdir, "landtrendr_2018"))
  landtrender_2018_input = file.path(rootdir, "landtrendr_2018")
  
  dir.create(file.path(rootdir, "landtrendr_2019"))
  landtrender_2019_input = file.path(rootdir, "landtrendr_2019")
  
  dir.create(file.path(rootdir, "landtrendr_2020"))
  landtrender_2020_input = file.path(rootdir, "landtrendr_2020")
  
  dir.create(file.path(rootdir, "disturbance"))
  disturbance_input = file.path(rootdir, "disturbance")
  
  dir.create(file.path(rootdir, "bioclimatic"))
  bioclimatic_input = file.path(rootdir, "bioclimatic")
  
  #______________________________#
  # CONFIGURE OUTPUT DIRECTORIES #
  #______________________________#
  
  out_dir = file.path("K:/GEDI/")
  out_dir
  
  dir.create(file.path(out_dir, "03_gedi_gee_tiles", run))
  
  dir.create(file.path(out_dir, "03_gedi_gee_tiles", run,"01_gedi_2016_ortho_tiles"))
  out_tile_2016 <- file.path(out_dir, "03_gedi_gee_tiles", run, "01_gedi_2016_ortho_tiles")
  
  dir.create(file.path(out_dir, "03_gedi_gee_tiles", run, "02_gedi_2017_ortho_tiles"))
  out_tile_2017 <- file.path(out_dir, "03_gedi_gee_tiles", run, "02_gedi_2017_ortho_tiles")
  
  dir.create(file.path(out_dir, "03_gedi_gee_tiles", run, "03_gedi_2018_ortho_tiles"))
  out_tile_2018 <- file.path(out_dir, "03_gedi_gee_tiles", run, "03_gedi_2018_ortho_tiles")
  
  dir.create(file.path(out_dir, "03_gedi_gee_tiles", run, "04_gedi_2019_ortho_tiles"))
  out_tile_2019 <- file.path(out_dir, "03_gedi_gee_tiles", run, "04_gedi_2019_ortho_tiles")
  
  dir.create(file.path(out_dir, "03_gedi_gee_tiles", run, "05_gedi_2020_ortho_tiles"))
  out_tile_2020 <- file.path(out_dir, "03_gedi_gee_tiles", run, "05_gedi_2020_ortho_tiles")
  
  #______________________________________#
  # READ IN THE DIFFERENT TILES AS LISTS #
  #______________________________________#
  
  ### Get the different lists of tiles to read in
  topo_list = list.files(topo_input, pattern = ".tif")
  topo_list
  
  sar_2016_list = list.files(sar_2016_input, pattern = ".tif")
  sar_2016_list
  
  sar_2017_list = list.files(sar_2017_input, pattern = ".tif")
  sar_2017_list
  
  sar_2018_list = list.files(sar_2018_input, pattern = ".tif")
  sar_2018_list
  
  sar_2019_list = list.files(sar_2019_input, pattern = ".tif")
  sar_2019_list
  
  sar_2020_list = list.files(sar_2020_input, pattern = ".tif")
  sar_2020_list
  
  lantrendr_2016_list = list.files(landtrender_2016_input, pattern = ".tif")
  lantrendr_2016_list
  
  lantrendr_2017_list = list.files(landtrender_2017_input, pattern = ".tif")
  lantrendr_2017_list
  
  lantrendr_2018_list = list.files(landtrender_2018_input, pattern = ".tif")
  lantrendr_2018_list
  
  lantrendr_2019_list = list.files(landtrender_2019_input, pattern = ".tif")
  lantrendr_2019_list
  
  lantrendr_2020_list = list.files(landtrender_2020_input, pattern = ".tif")
  lantrendr_2020_list
  
  disturbance_list = list.files(disturbance_input, pattern = ".tif")
  disturbance_list
  
  #ecoregion_list = list.files(ecoregion_input, pattern = ".tif")
  #ecoregion_list
  
  bioclimatic_list = list.files(bioclimatic_input, pattern = ".tif")
  bioclimatic_list
  
  #______________________________#
  # START THE PROCESSING CLUSTER #
  #______________________________#
  
  cl = makeCluster(10)
  registerDoParallel(cl)
  
  #______________________________________________________________#
  # CONDITIONALLY TILE THE INPUT PREDICTOR RASTERS FOR EACH YEAR #
  #______________________________________________________________#
  
  foreach(i=1:length(topo_list)) %dopar% {
    
    ### Load Packages
    library(terra)
    library(raster)
    
    ### Get the output file name
    file_name = topo_list[[i]]
    file_name
    
    ### Read in the topo tile
    setwd(topo_input)
    topo_tile = rast(file_name)
    topo_tile
    
    ### Read in the disturbance tile
    setwd(disturbance_input)
    desired_dist = disturbance_list[[i]]
    disturbance_tile = raster(desired_dist)
    disturbance_tile[is.na(disturbance_tile[])] <- 36 
    disturbance_tile = rast(disturbance_tile)
    disturbance_tile
    
    ### Read in the bioclimatic tile
    setwd(bioclimatic_input)
    desired_bioclimatic = bioclimatic_list[[i]]
    bioclimatic_tile = rast(desired_bioclimatic)
    bioclimatic_tile
    
    ### Read in the desired tiles for each year
    if(desired_year == 2016){
      
      setwd(sar_2016_input)
      desired_sar = sar_2016_list[[i]]
      sar_tile = rast(desired_sar)
      
      setwd(landtrender_2016_input)
      desired_landtrendr = lantrendr_2016_list[[i]]
      landtrendr_tile = rast(desired_landtrendr)}
    
    ### Read in the desired tiles for each year
    if(desired_year == 2017){
      
      setwd(sar_2017_input)
      desired_sar = sar_2017_list[[i]]
      sar_tile = rast(desired_sar)
      
      setwd(landtrender_2017_input)
      desired_landtrendr = lantrendr_2017_list[[i]]
      landtrendr_tile = rast(desired_landtrendr)}
    
    ### Read in the desired tiles for each year
    if(desired_year == 2018){
      
      setwd(sar_2018_input)
      desired_sar = sar_2018_list[[i]]
      sar_tile = rast(desired_sar)
      
      setwd(landtrender_2018_input)
      desired_landtrendr = lantrendr_2018_list[[i]]
      landtrendr_tile = rast(desired_landtrendr)}
    
    ### Read in the desired tiles for each year
    if(desired_year == 2019){
      
      setwd(sar_2019_input)
      desired_sar = sar_2019_list[[i]]
      sar_tile = rast(desired_sar)
      
      setwd(landtrender_2019_input)
      desired_landtrendr = lantrendr_2019_list[[i]]
      landtrendr_tile = rast(desired_landtrendr)}
    
    ### Read in the desired tiles for each year
    if(desired_year == 2020){
      
      setwd(sar_2020_input)
      desired_sar = sar_2020_list[[i]]
      sar_tile = rast(desired_sar)
      
      setwd(landtrender_2020_input)
      desired_landtrendr = lantrendr_2020_list[[i]]
      landtrendr_tile = rast(desired_landtrendr)}
    
    ### Bind the desired predictors together
    final_predictor_tile = c(sar_tile, topo_tile, landtrendr_tile, disturbance_tile, bioclimatic_tile)
    names(final_predictor_tile)
    
    names(final_predictor_tile) = c("s1_vv_median", "s1_vh_median", "vh_vv_ratio", "norm_diff", "rvi",
    "elevation", "slope", "aspect", "eastness", "northness", 
    "topo_diversity_srtm", "chili_srtm", "mTPI_srtm", "landform_srtm", 
    "blue", "green", "red", "nir", "swir1", "swir2", "ndvi", "nbr", "evi",
    "b5b4r", "tcb", "tcg", "tcw", "time_since_disturbance", "cmd", "dd5", "map", "mat", "mcmt")
    
    final_predictor_tile
    
    rm(sar_tile, topo_tile, landtrendr_tile, disturbance_tile, bioclimatic_tile)
    gc()
    
    ### Write out the merged tiles to the disk
    if(desired_year == 2016){
      setwd(out_tile_2016)
      terra::writeRaster(final_predictor_tile, file = file_name, overwrite = TRUE)}
    
    ### Write out the merged tiles to the disk
    if(desired_year == 2017){
      setwd(out_tile_2017)
      terra::writeRaster(final_predictor_tile, file = file_name, overwrite = TRUE)}
    
    ### Write out the merged tiles to the disk
    if(desired_year == 2018){
      setwd(out_tile_2018)
      terra::writeRaster(final_predictor_tile, file = file_name, overwrite = TRUE)}
    
    ### Write out the merged tiles to the disk
    if(desired_year == 2019){
      setwd(out_tile_2019)
      terra::writeRaster(final_predictor_tile, file = file_name, overwrite = TRUE)}
    
    ### Write out the merged tiles to the disk
    if(desired_year == 2020){
      setwd(out_tile_2020)
      terra::writeRaster(final_predictor_tile, file = file_name, overwrite = TRUE)}
    
    ### Clean up again
    rm(final_predictor_tile)
    gc()
    
    }

  ### Stop the processing cluster
  stopCluster(cl)
  
}

###________________________________________###
### RUN THE FUNCTION FOR THE DESIRED YEARS ###
###________________________________________###

stack_rasters(desired_year = 2016)

stack_rasters(desired_year = 2017)

stack_rasters(desired_year = 2018)

stack_rasters(desired_year = 2019)

stack_rasters(desired_year = 2020)

