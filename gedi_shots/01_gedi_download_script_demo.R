###################################################################################
###                                                                             ###
### Turnkey GEDI Downloading and Processing Script                              ###
### rGEDI_engine_V1.1                                                           ###
### Originally Re-Configured by N. Swayze 5/3/2021 with input from P. Fekety    ###
### Continued modification and upkeep by J. Vogeler                             ###
### POC: jody.vogeler@colostate.edu                                             ###
###                                                                             ###
###################################################################################

###_________________________###
### INSTALL / LOAD PACKAGES ###
###_________________________###

### Run pacman to load all required packages
library(pacman)
pacman::p_load(rgdal, mapview, sp, sf, raster, data.table, dplyr, foreach, doParallel, 
               rlas, ggplot2, httr, getPass, sys, curl, jsonlite, hdf5r, rGEDI)

###______________________###
### INSTRUCTIONS FOR USE ###
###______________________###

### Step 1: Download and install aria2c to your machine from https://github.com/aria2/aria2/releases/tag/release-1.35.0 -> aria2-1.35.0-win-64bit-build1.zip

### Step 2: Change line 42 to reflect the location of the aria2c.exe executable ie ("C:\\Users\\neal\\aria2-1.35.0-win-64bit-build1\\aria2c.exe") 

### Step 3: Create a folder on your desired storage drive as a base directory (example : "GEDI_processing"), this is your rootDir. Update line 59 to reflect this

### Step 4: Run LINES 49-51 of this script to generate a study_area_input subfolder in your root directory

### Step 5: Create a shapefile of your desired study area in WGS 1984 (ESPG: 4326) and move the shapefile to your root directory -> 01_study_area_input folder

### Step 6: Update LINE 54 to reflect the name of the shapefile in the 01_study_area_input folder

### Step 7: You will need to generate a .netrc file if you dont have one, the script will prompt you. Create an account at: urs.earthdata.nasa.gov

###_______________________________###
### STUDY AREA AND DATE SELECTION ###
###_______________________________###

### POINT THIS LINE AT YOUR ARIA 2 BATCH DOWNLOAD EXECUTABLE IN THE ARIA2 FOLDER
aria_exe <- "C:\\Users\\neal\\aria2-1.35.0-win-64bit-build1\\aria2c.exe"

### ESTABLISH YOUR ROOT DIRECTORY
rootDir <- ("D:/test_gedi_rouge_river")
setwd(rootDir)

###_______________________________###
### AUTOGENERATE BASE DIRECTORIES ###
###_______________________________###

dir.create(file.path(rootDir, "01_gedi_data"))
base_dir = file.path(rootDir, "01_gedi_data")
dir.create(file.path(rootDir, "02_predictor_raster_tiles"))
raster_tile_dir = file.path(rootDir, "02_predictor_raster_tiles")
dir.create(file.path(rootDir, "03_extracted_spectral_data"))
spectral_data_dir = file.path(rootDir, "03_extracted_spectral_data")
dir.create(file.path(rootDir, "04_random_forest_modeling"))
rf_model_dir = file.path(rootDir, "04_random_forest_modeling")

### GENERATE A DIRECTORY FOR YOUR STUDY AREA SHAPEFILE
dir.create(file.path(base_dir, "/01_study_area_input"))
study_area_input <- file.path(base_dir, "/01_study_area_input/")
setwd(study_area_input)

### INPUT THE NAME OF YOUR DESIRED STUDY AREA SHAPEFILE
study_area <- readOGR("test_rouge_river.shp")
study_area <- spTransform(study_area, CRS("+init=epsg:4326"))
viewExtent(study_area, map.type = "Esri.WorldImagery")

### SET DESIRED DATE TIMEFRAME FOR GEDI FOOTPRINT EXTRACTION
daterange=c("2019-08-30","2019-09-30")

### DEFINE THE BEAMS THAT YOU WANT TO KEEP FROM THE DATA (FULL POWER BEAMS INCLUDED)
target_beams <- c("BEAM0101", "BEAM0110", "BEAM1000", "BEAM1011")

### DEFINE THE NUMBER OF SHOTS TO RETAIN FOR RANDOM/SPATIAL/STRATIFIED SAMPLING
num_samples <- 100000

### Define the number of cores to use when processing GEDI granules (Use a fast SSD or you will be sad)
num_cores <- 14

### Starting Time 
start_time <- Sys.time()

###__________________________________________________###
### Creating a set of processing folders for outputs ###
###    MODIFICATION OF LINES BELOW NOT REQURIRED     ###
###__________________________________________________###

### CREATE DATA DOWNLOAD DIRECTORIES
dir.create(file.path(base_dir, "/02_download_files"))
dir.create(file.path(base_dir, "/02_download_files/01_GEDI_downloads_2A"))
gedi_downloads_2A <- file.path(base_dir, "/02_download_files/01_GEDI_downloads_2A")
dir.create(file.path(base_dir, "/02_download_files/01_GEDI_downloads_2B"))
gedi_downloads_2B <- file.path(base_dir, "/02_download_files/01_GEDI_downloads_2B")

### CREATE PROCESSING DIRECTORIES
gedi_processing_stats_output <- file.path(base_dir)
dir.create(file.path(base_dir, "/03_gedi_filtered_data_csv"))
gedi_processed_output <- file.path(base_dir, "/03_gedi_filtered_data_csv")
dir.create(file.path(base_dir, "/04_gedi_filtered_data_shp"))
gedi_processed_output_shp <- file.path(base_dir, "/04_gedi_filtered_data_shp")
dir.create(file.path(base_dir, "/05_gedi_filtered_data_random_samples"))
gedi_processed_output_samples <- file.path(base_dir, "/05_gedi_filtered_data_random_samples")
dir.create(file.path(base_dir, "/06_gedi_summary_plots"))
gedi_summary_plots <- file.path(base_dir, "/06_gedi_summary_plots")
dir.create(file.path(base_dir, "/07_gedi_converted_las_files"))
gedi_las_outputs <- file.path(base_dir, "/07_gedi_converted_las_files")

###_________________________###
### PROCESSING STARTS BELOW ###
###_________________________###

### EXTRACTING COORDINATES FROM STUDY AREA SHAPEFILE
study_coords <- extent(study_area)
study_coords <- coordinates(study_coords)
study_coords <- as.data.frame(study_coords)
colnames(study_coords) <- c("long", "lat")
study_coords_spdf <- SpatialPointsDataFrame(cbind(study_coords$long,study_coords$lat), data=study_coords)
proj4string(study_coords_spdf) = CRS("+init=epsg:4326")
mapview(study_coords_spdf, map.type = "Esri.WorldImagery", xcol = "long", ycol = "lat")

### AUTOMATICALLY CONFIGURING BOUNDING BOX FOR GEDIFINDER FUNCTION 
lower_left_long <- study_coords$long[1]
lower_left_lat <- study_coords$lat[1]
upper_right_long <- study_coords$long[3]
upper_right_lat <- study_coords$lat[3] 
bbox <- paste0(lower_left_long, ",", lower_left_lat, ",", upper_right_long, ",", upper_right_lat)

###_____________________________###
### DEFINE GEDI FINDER FUNCTION ### 
###_____________________________###

gedifinder_custom <- function(product, bbox) {
  
  # Define the base CMR granule search url, including LPDAAC provider name and max page size (2000 is the max allowed)
  cmr <- "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id="
  
  # Set up dictionary where key is GEDI shortname + version and value is CMR Concept ID
  concept_ids <- list('GEDI01_B.002'='C1908344278-LPDAAC_ECS', 
                      'GEDI02_A.002'='C1908348134-LPDAAC_ECS', 
                      'GEDI02_B.002'='C1908350066-LPDAAC_ECS')
  
  # CMR uses pagination for queries with more features returned than the page size
  page <- 1
  bbox <- sub(' ', '', bbox)  # Remove any white spaces
  granules <- list()          # Set up a list to store and append granule links to
  
  # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number
  cmr_response <- GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page))
  cmr_response
  
  # Verify the request submission was successful
  if (cmr_response$status_code==200){
    
    # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number, format return as a list
    cmr_response <- content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry
    
    # If 2000 features are returned, move to the next page and submit another request, and append to the response
    while(length(cmr_response) %% 2000 == 0){
      page <- page + 1
      cmr_response <- c(cmr_response, content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry)
    }
    
    # CMR returns more info than just the Data Pool links, below use for loop to go through each feature, grab DP link, and add to list
    for (i in 1:length(cmr_response)) {
      granules[[i]] <- cmr_response[[i]]$links[[1]]$href
    }
    
    # Return the list of links
    return(granules)
  } else {
    
    # If the request did not complete successfully, print out the response from CMR
    print(content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$errors)
  }
}

###___________________________________________________________________________###
### DEFINE FUNCTION TO FILTER THE DETECTED GEDI GRANULES TO DESIRED TIMEFRAME ###
###___________________________________________________________________________###

filter_date <- function(granules) {
  final_granules <- list() # Create empty list for final granules
  start_date <- daterange[1] #Get the start date from the daterange object
  end_date <- daterange[2] #Get the end date from the daterange object
  ### Loop to check if the dates for each granule are within the input date range
  for(i in seq(granules)){
    list_id <- granules[[i]] # Get the first granule in the list
    list_id_info <- unlist(strsplit(list_id, split = "/")) #Split it up 
    list_id_info <- list_id_info[8] #Get the granule date
    list_id_info <- gsub("\\.", "-", list_id_info) #change the period to a -
    result <- (list_id_info <= end_date) && (list_id_info >= start_date)
    #Conditional statement to append the granule file to the final granule list if it is within the daterange
    if (isTRUE(result)) {final_granules <- append(final_granules, list_id)}
    if (isFALSE(result)) {print("Date out of bounds")}
  }
  
  final_granules
  return(final_granules)
}

###________________________________________________________________________###
### USE THE GEDIFINDER FUNCTION TO SEARCH FOR GRANULES OVER THE STUDY AREA ###
###________________________________________________________________________###

### QUERY FOR 2A products
granules <- gedifinder_custom(product="GEDI02_A.002", bbox)
granules <- filter_date(granules)
print(sprintf("%s Version 2 2A granules found.", length(granules)))
outName = "desired_granules_2A.txt"
setwd(base_dir)
write.table(granules, outName, row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')

### QUERY FOR 2B products
granules <- gedifinder_custom(product="GEDI02_B.002", bbox)
granules <- filter_date(granules)
print(sprintf("%s Version 2 2B granules found.", length(granules)))
outName = "desired_granules_2B.txt"
setwd(base_dir)
write.table(granules, outName, row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')

###______________________________###
### CONFIGURE PATH TO NETRC FILE ###
###______________________________###

# Retrieve user directory (for netrc file)
usr <- file.path(Sys.getenv("USERPROFILE")) 

# If no user profile exists, use home directory
if (usr == "") {usr = Sys.getenv("HOME")}    

# Path to netrc file
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep) 

###_____________________________________________###
### AUTOMATICALLY QUERY IF YOU HAVE .NETRC FILE ###
###_____________________________________________###

### If you do not have a  .netrc file with your Earthdata Login credentials stored in your home dir,
### below you will be prompted for your NASA Earthdata Login Username and Password and a netrc file
### will be created to store your credentials (home dir). Create an account at: urs.earthdata.nasa.gov

if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov):")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}
netrc <- read.table(netrc)

### Grab Username
username <- as.data.frame(netrc$V2)
username <- username[2,]
username <- noquote(as.character(username))
username

### Grab Password
password <- as.data.frame(netrc$V2)
password <- password[3,]
password <- noquote(as.character(password))
password

###____________________________________________________________________________###
### Batch Downloading Method - THIS WILL DOWNLOAD DESIRED GRANULES IN PARALLEL ###
###____________________________________________________________________________###

### Generate and execute command for batch downloading of 2A GEDI granules
aria_command_2A = paste0(aria_exe, " --max-concurrent-downloads=5 --auto-file-renaming=false --file-allocation=none --http-user=", username, " --http-passwd=", password, " --dir ", file.path(base_dir, "02_download_files", "01_GEDI_downloads_2A"), " -i ", file.path(base_dir, "desired_granules_2A.txt")," --force-save")
shell(aria_command_2A)

### Generate and execute command for batch downloading of 2B GEDI granules
aria_command_2B = paste0(aria_exe, " --max-concurrent-downloads=5 --auto-file-renaming=false --file-allocation=none --http-user=", username, " --http-passwd=", password," --dir ", file.path(base_dir, "02_download_files", "01_GEDI_downloads_2B"), " -i ", file.path(base_dir, "desired_granules_2B.txt")," --force-save")
shell(aria_command_2B)

###_______________________________________________________________________________________###
### DEFINE CUSTOM FUNCTIONS FOR READING IN GEDI GRANULES, AND COMPULE DATA FROM .H5 FILES ###
###_______________________________________________________________________________________###

### Define read level 2A Function from .h5 files
readlevel2A_custom = function (level2Apath) {
  level2a_h5 <- hdf5r::H5File$new(level2Apath, mode = "r")
  level2a <- new("gedi.level2a", h5 = level2a_h5)
  return(level2a)}

### Define read level 2B Function from .h5 files
readLevel2B_custom = function (level2Bpath) {
  level2b_h5 <- hdf5r::H5File$new(level2Bpath, mode = "r")
  level2b <- new("gedi.level2b", h5 = level2b_h5)
  return(level2b)}

### Define custom processing function to get desired 2A data
getLevel2A_custom = function (level2a) {
  level2a <- level2a@h5
  groups_id <- grep("BEAM\\d{4}$", gsub("/", "", 
                                        hdf5r::list.groups(level2a, recursive = F)), value = T)
  rh.dt <- data.table::data.table()
  pb <- utils::txtProgressBar(min = 0, max = length(groups_id), 
                              style = 3)
  i.s = 0
  for (i in groups_id) {
    i.s <- i.s + 1
    utils::setTxtProgressBar(pb, i.s)
    level2a_i <- level2a[[i]]
    if (any(hdf5r::list.datasets(level2a_i) == "shot_number")) {
      if (length(level2a_i[["rh"]]$dims) == 2) {
        rh = t(level2a_i[["rh"]][, ])
      }
      else {
        rh = t(level2a_i[["rh"]][])
      }
      rhs <- data.table::data.table(beam <- rep(i, length(level2a_i[["shot_number"]][])), 
                                    shot_number = level2a_i[["shot_number"]][], 
                                    degrade_flag = level2a_i[["degrade_flag"]][], 
                                    quality_flag = level2a_i[["quality_flag"]][], 
                                    deltatime = level2a_i[["delta_time"]][], 
                                    sensitivity = level2a_i[["sensitivity"]][], 
                                    solar_elevation = level2a_i[["solar_elevation"]][], 
                                    lat_lowestmode = level2a_i[["lat_lowestmode"]][], 
                                    lon_lowestmode = level2a_i[["lon_lowestmode"]][], 
                                    elev_highestreturn = level2a_i[["elev_highestreturn"]][], 
                                    elev_lowestmode = level2a_i[["elev_lowestmode"]][], 
                                    rh)
      rh.dt <- rbind(rh.dt, rhs)
    }
  }
  colnames(rh.dt) <- c("beam", "shot_number", "degrade_flag", "quality_flag", "delta_time", "sensitivity", "solar_elevation", 
                       
                       "lat_lowestmode", "lon_lowestmode", "elev_highestreturn", "elev_lowestmode", 
                       
                       paste0("rh", seq(0, 100)))
  close(pb)
  return(rh.dt)
}

### Define custom processing function to get desired 2B data
getLevel2B_custom = function(level2b){
  
  ### Function to get the full set of 2B data, not including Z profiles
  #####################################################################
  level2b<-level2b@h5
  groups_id<-grep("BEAM\\d{4}$",gsub("/","", hdf5r::list.groups(level2b, recursive = F)), value = T)
  m.dt<-data.table::data.table()
  pb <- utils::txtProgressBar(min = 0, max = length(groups_id), style = 3)
  i.s=0
  var.map = data.table::data.table(t(data.frame(list(
    # COL_NAMES              # H5_ADDRESS
    c("shot_number",         "shot_number"),
    c("algorithmrun_flag",   "algorithmrun_flag"),
    c("degrade_flag",        "geolocation/degrade_flag"),
    c("l2b_quality_flag",    "l2b_quality_flag"),
    c("stale_return_flag",   "stale_return_flag"),
    c("surface_flag",        "surface_flag"),
    c("solar_elevation",     "geolocation/solar_elevation"),
    c("delta_time",          "geolocation/delta_time"),
    c("sensitivity",         "sensitivity"),
    c("lat_lowestmode",      "geolocation/lat_lowestmode"),
    c("lon_lowestmode",      "geolocation/lon_lowestmode"),
    c("elev_highestreturn",  "geolocation/elev_highestreturn"),
    c("elev_lowestmode",     "geolocation/elev_lowestmode"),
    c("local_beam_elevation","geolocation/local_beam_elevation"),
    c("fhd_normal",          "fhd_normal"),
    c("pgap_theta",          "pgap_theta"),
    c("rh100",               "rh100"),
    c("pai",                 "pai"),
    c("rhov",                "rhov"),
    c("rhog",                "rhog"),
    c("omega",               "omega"),
    c("cover",               "cover")
  ))))
  
  colnames(var.map) = c("COL_NAMES", "H5_ADDRESS")
  
  for ( i in groups_id){
    i.s<-i.s+1
    utils::setTxtProgressBar(pb, i.s)
    level2b_i<-level2b[[i]]
    m <-data.table::data.table(beam=rep(i,length(level2b_i[["shot_number"]][])))
    
    for (row_index in 1:nrow(var.map)) {
      colname = var.map$COL_NAMES[row_index]
      h5.address = var.map$H5_ADDRESS[row_index]
      m[[colname]] <- level2b_i[[h5.address]][]
    }
    
    m.dt<-rbind(m.dt,m)
    colnames(m.dt)<-c("beam", var.map$COL_NAMES)
    main_data_table <- m.dt
    
  }
  
  close(pb)
  main_data_table
  
  ### New function to get the cover profiles (modified the PAI to get this function running)
  ##########################################################################################
  groups_id<-grep("BEAM\\d{4}$", gsub("/","", hdf5r::list.groups(level2b, recursive = F)), value = T)
  groups_id
  m.dt<-data.table::data.table()
  m.dt
  pb <- utils::txtProgressBar(min = 0, max = length(groups_id), style = 3)
  i.s=0
  
  for ( i in groups_id){
    i.s<-i.s+1
    utils::setTxtProgressBar(pb, i.s)
    level2b_i<-level2b[[i]]
    m<-data.table::data.table(
      beam<-rep(i,length(level2b_i[["shot_number"]][])),
      shot_number=level2b_i[["shot_number"]][],
      height_lastbin=level2b_i[["geolocation/height_lastbin"]][],
      height_bin0=level2b_i[["geolocation/height_bin0"]][],
      cover_z=t(level2b_i[["cover_z"]][,1:level2b_i[["cover_z"]]$dims[2]]))
    m.dt<-rbind(m.dt,m)
    level2b_i[["cover_z"]][,1:level2b_i[["cover_z"]]$dims[2]]
    
  }
  
  colnames(m.dt)<-c("beam","shot_number","height_lastbin","height_bin0",paste0("cover_z_",seq(0,30*5,5)[-31],"_",seq(5,30*5,5),"m"))
  cover_data_table <- m.dt
  close(pb)
  
  ### Function to get the PAI profiles
  ####################################
  groups_id<-grep("BEAM\\d{4}$",gsub("/","", hdf5r::list.groups(level2b, recursive = F)), value = T)
  m.dt<-data.table::data.table()
  pb <- utils::txtProgressBar(min = 0, max = length(groups_id), style = 3)
  i.s=0
  for ( i in groups_id){
    i.s<-i.s+1
    utils::setTxtProgressBar(pb, i.s)
    level2b_i<-level2b[[i]]
    m<-data.table::data.table(
      beam<-rep(i,length(level2b_i[["shot_number"]][])),
      shot_number=level2b_i[["shot_number"]][],
      height_lastbin=level2b_i[["geolocation/height_lastbin"]][],
      height_bin0=level2b_i[["geolocation/height_bin0"]][],
      pai_z=t(level2b_i[["pai_z"]][,1:level2b_i[["pai_z"]]$dims[2]]))
    m.dt<-rbind(m.dt,m)
  }
  colnames(m.dt)<-c("beam","shot_number","height_lastbin",
                    "height_bin0",paste0("pai_z",seq(0,30*5,5)[-31],"_",seq(5,30*5,5),"m"))
  
  pai_data_table <- m.dt
  close(pb)
  
  ### Function to get the PAVD profiles
  #####################################
  groups_id<-grep("BEAM\\d{4}$",gsub("/","", hdf5r::list.groups(level2b, recursive = F)), value = T)
  m.dt<-data.table::data.table()
  pb <- utils::txtProgressBar(min = 0, max = length(groups_id), style = 3)
  i.s=0
  for ( i in groups_id){
    i.s<-i.s+1
    utils::setTxtProgressBar(pb, i.s)
    level2b_i<-level2b[[i]]
    m<-data.table::data.table(
      beam<-rep(i,length(level2b_i[["shot_number"]][])),
      shot_number=level2b_i[["shot_number"]][],
      pavd_z=t(level2b_i[["pavd_z"]][,1:level2b_i[["pavd_z"]]$dims[2]]))
    m.dt<-rbind(m.dt,m)
  }
  colnames(m.dt)<-c("beam","shot_number",paste0("pavd_z",seq(0,30*5,5)[-31],"_",seq(5,30*5,5),"m"))
  pavd_data_table <- m.dt
  close(pb)
  
  ### Select the desired data from each data table
  cover_data_table <- dplyr::select(cover_data_table, subset = -c(beam, height_lastbin, height_bin0))
  pai_data_table <- dplyr::select(pai_data_table, subset = -c(beam, height_lastbin, height_bin0))
  pavd_data_table <- dplyr::select(pavd_data_table, subset = -c(beam))
  
  ### Master merge the data into a single dataframe by shot number
  master_dt <- merge(main_data_table, cover_data_table, by = "shot_number")
  master_dt <- merge(master_dt, pai_data_table, by = "shot_number")
  master_dt <- merge(master_dt, pavd_data_table, by = "shot_number")
  master_dt
  
  ### Clean up afteryourself and save memory
  rm(m.dt, main_data_table, cover_data_table, pai_data_table, pavd_data_table)
  gc()
  
  return(master_dt)
}

###______________________________________________________________###
### PROCESSING AND FILTERING LEVEL 2A DATA - OUTPUT TO DATAFRAME ###
###______________________________________________________________###

### Create temporary directory for storing 2A data  
dir.create(file.path(base_dir, "/03_gedi_filtered_data_csv/01_gedi_2A_temp"))
gedi_processed_output_2A <- file.path(base_dir, "/03_gedi_filtered_data_csv/01_gedi_2A_temp")

### Generating a list of granule files to process from the GEDI downloads folder
setwd(gedi_downloads_2A)
list <- list.files(setwd(gedi_downloads_2A), pattern = "h5$")
list

### Initiate the processing cluster
cl <- makeCluster(num_cores)
registerDoParallel(cl)

### Process the granules in parallel, return .csv for each granule to be merged later
foreach(i=1:length(list)) %dopar% {
  
  library(rGEDI)
  library(hdf5r)  
  library(dplyr)
  library(data.table)
  library(sp)
  library(raster)
  
  setwd(gedi_downloads_2A) #set the working directory to the folder of downloaded 2A data
  granule_name <- list[[i]]
  out_name <- tools::file_path_sans_ext(granule_name)
  out_name <- paste0(out_name, ".csv")
  file_1 = readLevel2A(granule_name)# Read in the first file from the list
  file_1 = getLevel2A_custom(file_1) #Convert the .h5 file to a dataframe in R memory for Plant area index profiles
  file_1 = subset(file_1, select = c(shot_number, beam, degrade_flag, quality_flag, sensitivity, solar_elevation, lat_lowestmode, lon_lowestmode, rh0, rh5, rh10, rh15, rh20, rh25, rh30, rh35, rh40, rh45, rh50, rh55, rh60, rh65, rh70, rh75, rh80, rh85, rh90, rh95, rh96, rh97, rh98, rh99, rh100) ) # subset data down to what we need
  file_1 = na.omit(object = file_1) #Omit missing values from the dataframe, some gedi data is missing coordinate data
  file_1$shot_number = as.character(file_1$shot_number) #Convert shot number from integer 64 to character  
  file_1 = filter(file_1, ( (solar_elevation < 0) & (degrade_flag == 0) & (quality_flag == 1) & (beam %in% target_beams) & (sensitivity >= 0.8) ) ) #Conditional filtering
  num_rows <- nrow(file_1)
  num_rows
  
  if(num_rows > 0){
    file_1 = SpatialPointsDataFrame(cbind(file_1$lon_lowestmode,file_1$lat_lowestmode), data=file_1) #Converting dataframe to spatial points dataframe
    file_1 = crop(file_1, study_area) #cropping the dataframe to the desired study area
    file_1 = as.data.frame(file_1) #Converting cropped spatial points dataframe back to dataframe for further processing
    setwd(gedi_processed_output_2A) #Set the working directory to the temporary processing folder
    fwrite(file_1, file = out_name, sep = ",")
    rm(file_1) #remove file_1 for next iteration
    gc() #clear R memory for deleted files 
  }
  
  if(num_rows < 1){
    setwd(gedi_processed_output_2A)
    empty_df <- data.frame()
    write.csv(empty_df, file = out_name)
    rm(file_1)
    gc
  }
  
  out_name
  
}

stopCluster(cl)

### Setting up an empty storage dataframe .csv file to append level 2A data to
storage_dataframe_2A <- data.frame()                          

### Generating a list of processed 2A .csv granule files
setwd(gedi_processed_output_2A)
list <- list.files(setwd(gedi_processed_output_2A), pattern = "csv$")
list

### For loop to bind list of 2A granule csv files together
for(i in seq(list)) {
  csv_id <- list[[i]]
  granule_data <- fread(csv_id)
  num_row <- nrow(granule_data)
  if (num_row > 1){
    storage_dataframe_2A <- rbind(storage_dataframe_2A, granule_data)
    rm(granule_data)
    gc()
  }
  
}

### Write out the 2A merged GEDI data to a .csv file, remove temp directory
storage_dataframe_2A
setwd(gedi_processed_output)
fwrite(storage_dataframe_2A, "GEDI_2A_raw.csv", row.names=FALSE)
unlink(gedi_processed_output_2A, recursive = TRUE) #Remove the files in the temporary directory

###______________________________________________________________###
### PROCESSING AND FILTERING LEVEL 2B DATA - OUTPUT TO DATAFRAME ###
###______________________________________________________________###

### Create temporary directory for storing 2B data 
dir.create(file.path(base_dir, "/03_gedi_filtered_data_csv/01_gedi_2B_temp"))
gedi_processed_output_2B <- file.path(base_dir, "/03_gedi_filtered_data_csv/01_gedi_2B_temp")

### Generating a list of granule files to process from the GEDI donwloads folder
setwd(gedi_downloads_2B)
list <- list.files(setwd(gedi_downloads_2B), pattern = "h5$")
list

### Initiate the processing cluster
cl <- makeCluster(num_cores)
registerDoParallel(cl)

### Process the granules in parallel, return .csv for each granule to be merged later
foreach(i=1:length(list)) %dopar% {
  
  library(rGEDI)
  library(dplyr)
  library(data.table)
  library(sp)
  library(raster)
  
  setwd(gedi_downloads_2B) #set the working directory to the folder of downloaded 2B data
  granule_name <- list[[i]]
  out_name <- tools::file_path_sans_ext(granule_name)
  out_name <- paste0(out_name, ".csv")
  input_file = readLevel2B(list[[i]]) # Read in the first file from the list
  file_1 <- getLevel2B_custom(input_file) #Run custom function to get main 2B, cover Z, pai z, and pavd Z data
  file_1 = na.omit(object = file_1) #Omit missing values from the dataframe, some gedi data is missing coordinate data
  file_1$delta_time = as.Date(  as.POSIXct(file_1$delta_time,  tz = "", origin = '2018-01-01')) #converting delta time (s) to date (year-month-day)
  file_1 = filter(file_1, ( (solar_elevation < 0) & (l2b_quality_flag == 1) & (degrade_flag == 0) & (beam %in% target_beams) & (sensitivity >= 0.8) ) ) #Conditional filtering
  num_rows <- nrow(file_1)
  
  if(num_rows > 0){
    file_1 = SpatialPointsDataFrame(cbind(file_1$lon_lowestmode,file_1$lat_lowestmode), data=file_1) #Converting dataframe to spatial points dataframe
    file_1 = crop(file_1, study_area) #cropping the dataframe to the desired study area
    file_1 = as.data.frame(file_1) #Converting cropped spatial points dataframe back to dataframe for further processing
    setwd(gedi_processed_output_2B) #Set the working directory to the temporary processing folder
    fwrite(file_1, file = out_name, sep = ",")
    rm(file_1) #remove file_1 for next iteration
    gc() #clear R memory for deleted files 
  }
  
  if(num_rows < 1){
    setwd(gedi_processed_output_2B)
    empty_df <- data.frame()
    write.csv(empty_df, file = out_name)
    rm(file_1)
    gc
  }
  
  granule_name
  
}

stopCluster(cl)

### Setting up an empty storage dataframe for appending all data to
storage_dataframe_2B <- data.frame()

### Generating a list of processed 2B .csv granule files
setwd(gedi_processed_output_2B)
list <- list.files(setwd(gedi_processed_output_2B), pattern = "csv$")
list

### For loop to bind list of 2B granule csv files together
for(i in seq(list)) {
  csv_id <- list[[i]]
  granule_data <- fread(csv_id)
  num_row <- nrow(granule_data)
  if (num_row > 1){
    storage_dataframe_2B <- rbind(storage_dataframe_2B, granule_data)
    rm(granule_data)
    gc()
  }
  
}

### Write out the 2B merged GEDI data to a .csv file, remove temp directory
storage_dataframe_2B
setwd(gedi_processed_output)
fwrite(storage_dataframe_2B, "GEDI_2B_raw.csv", row.names=FALSE)
unlink(gedi_processed_output_2B, recursive = TRUE) #Remove the files in the temporary directory

###____________________________________________________________________________###
### Reading in the two temporary un-filtered storage data frames into R Memory ###
###____________________________________________________________________________###

### Read in the 2A dataframe, subset to remove unneeded columns, check headers and rows and file size in memory
setwd(gedi_processed_output)
storage_dataframe_2A <- fread("GEDI_2A_raw.csv")
storage_dataframe_2A
storage_dataframe_2A <- subset(storage_dataframe_2A, select = -c(lat_lowestmode, lon_lowestmode, beam, degrade_flag, quality_flag, sensitivity, solar_elevation, coords.x1, coords.x2) )
storage_dataframe_2A$shot_number = as.character(storage_dataframe_2A$shot_number)
gc()
print(object.size(x=lapply(ls(), get)), units="Mb")

### Read in the 2B dataframe, subset to remove unneeded columns, reformat column type, check headers and rows and file size in memory 
setwd(gedi_processed_output)
storage_dataframe_2B <- fread("GEDI_2B_raw.csv")
storage_dataframe_2B <- subset(storage_dataframe_2B, select = -c(coords.x1, coords.x2, rh100) )
storage_dataframe_2B$delta_time = as.Date(storage_dataframe_2B$delta_time)
storage_dataframe_2B$shot_number = as.character(storage_dataframe_2B$shot_number)
gc()
print(object.size(x=lapply(ls(), get)), units="Mb")

###___________________________________________________________________###
### Merging the subset 2A and 2B data frames into a single data frame ###
###___________________________________________________________________###

### Checking the 2A and 2B dataframes
names(storage_dataframe_2A)
names(storage_dataframe_2B)
nrow_2A<-nrow(storage_dataframe_2A)
nrow_2B<-nrow(storage_dataframe_2B)

### Merging the two dataframes together conditionally, matching based on shot_number column and dropping any mismatches
gedi_data_2A_2B <- merge(storage_dataframe_2B, storage_dataframe_2A, by="shot_number")
nrow(gedi_data_2A_2B)
gedi_data_2A_2B <- na.omit(gedi_data_2A_2B)
nrow(gedi_data_2A_2B)
rm(storage_dataframe_2A, storage_dataframe_2B)
gc()

names(gedi_data_2A_2B)

### Subset the z profile metric
cover_z_10_plus = subset(gedi_data_2A_2B, select = c(cover_z_10_15m, cover_z_15_20m, cover_z_20_25m, cover_z_25_30m, 
                                                     cover_z_30_35m, cover_z_35_40m, cover_z_40_45m, cover_z_45_50m, 
                                                     cover_z_50_55m, cover_z_55_60m, cover_z_60_65m, cover_z_65_70m, 
                                                     cover_z_70_75m, cover_z_75_80m, cover_z_80_85m, cover_z_85_90m, 
                                                     cover_z_90_95m, cover_z_95_100m, cover_z_100_105m, cover_z_105_110m,
                                                     cover_z_110_115m, cover_z_115_120m, cover_z_120_125m, cover_z_125_130m, 
                                                     cover_z_130_135m, cover_z_135_140m, cover_z_140_145m, cover_z_145_150m))

cover_z_20_plus = subset(gedi_data_2A_2B, select = c(cover_z_20_25m, cover_z_25_30m, cover_z_30_35m, cover_z_35_40m, 
                                                     cover_z_40_45m, cover_z_45_50m, cover_z_50_55m, cover_z_55_60m, 
                                                     cover_z_60_65m, cover_z_65_70m, cover_z_70_75m, cover_z_75_80m, 
                                                     cover_z_80_85m, cover_z_85_90m, cover_z_90_95m, cover_z_95_100m, 
                                                     cover_z_100_105m, cover_z_105_110m, cover_z_110_115m, cover_z_115_120m, 
                                                     cover_z_120_125m, cover_z_125_130m, cover_z_130_135m, cover_z_135_140m,
                                                     cover_z_140_145m, cover_z_145_150m))

cover_z_40_plus = subset(gedi_data_2A_2B, select = c(cover_z_40_45m, cover_z_45_50m, cover_z_50_55m, cover_z_55_60m, 
                                                     cover_z_60_65m, cover_z_65_70m, cover_z_70_75m, cover_z_75_80m, 
                                                     cover_z_80_85m, cover_z_85_90m, cover_z_90_95m, cover_z_95_100m, 
                                                     cover_z_100_105m, cover_z_105_110m, cover_z_110_115m, cover_z_115_120m, 
                                                     cover_z_120_125m, cover_z_125_130m, cover_z_130_135m, cover_z_135_140m,
                                                     cover_z_140_145m, cover_z_145_150m))

pai_z_10_plus = subset(gedi_data_2A_2B, select = c(pai_z10_15m, pai_z15_20m, pai_z20_25m, pai_z25_30m, 
                                                   pai_z30_35m, pai_z35_40m, pai_z40_45m, pai_z45_50m, 
                                                   pai_z50_55m, pai_z55_60m, pai_z60_65m, pai_z65_70m, 
                                                   pai_z70_75m, pai_z75_80m, pai_z80_85m, pai_z85_90m, 
                                                   pai_z90_95m, pai_z95_100m, pai_z100_105m, pai_z105_110m,
                                                   pai_z110_115m, pai_z115_120m, pai_z120_125m, pai_z125_130m, 
                                                   pai_z130_135m, pai_z135_140m, pai_z140_145m, pai_z145_150m))

pai_z_20_plus = subset(gedi_data_2A_2B, select = c(pai_z20_25m, pai_z25_30m, pai_z30_35m, pai_z35_40m, pai_z40_45m, pai_z45_50m, 
                                                   pai_z50_55m, pai_z55_60m, pai_z60_65m, pai_z65_70m, pai_z70_75m, pai_z75_80m,
                                                   pai_z80_85m, pai_z85_90m, pai_z90_95m, pai_z95_100m, pai_z100_105m, pai_z105_110m, 
                                                   pai_z110_115m, pai_z115_120m, pai_z120_125m, pai_z125_130m, pai_z130_135m, pai_z135_140m, 
                                                   pai_z140_145m, pai_z145_150m))

pai_z_40_plus = subset(gedi_data_2A_2B, select = c(pai_z40_45m, pai_z45_50m, pai_z50_55m, pai_z55_60m, pai_z60_65m, pai_z65_70m, 
                                                   pai_z70_75m, pai_z75_80m,pai_z80_85m, pai_z85_90m, pai_z90_95m, pai_z95_100m, 
                                                   pai_z100_105m, pai_z105_110m, pai_z110_115m, pai_z115_120m, pai_z120_125m, 
                                                   pai_z125_130m, pai_z130_135m, pai_z135_140m, pai_z140_145m, pai_z145_150m))

pavd_z_10_plus = subset(gedi_data_2A_2B, select = c(pavd_z10_15m, pavd_z15_20m, pavd_z20_25m, pavd_z25_30m, 
                                                    pavd_z30_35m, pavd_z35_40m, pavd_z40_45m, pavd_z45_50m, 
                                                    pavd_z50_55m, pavd_z55_60m, pavd_z60_65m, pavd_z65_70m, 
                                                    pavd_z70_75m, pavd_z75_80m, pavd_z80_85m, pavd_z85_90m, 
                                                    pavd_z90_95m, pavd_z95_100m, pavd_z100_105m, pavd_z105_110m,
                                                    pavd_z110_115m, pavd_z115_120m, pavd_z120_125m, pavd_z125_130m, 
                                                    pavd_z130_135m, pavd_z135_140m, pavd_z140_145m, pavd_z145_150m))

pavd_z_20_plus = subset(gedi_data_2A_2B, select = c(pavd_z20_25m, pavd_z25_30m, pavd_z30_35m, pavd_z35_40m, pavd_z40_45m, pavd_z45_50m, 
                                                    pavd_z50_55m, pavd_z55_60m, pavd_z60_65m, pavd_z65_70m, pavd_z70_75m, pavd_z75_80m,
                                                    pavd_z80_85m, pavd_z85_90m, pavd_z90_95m, pavd_z95_100m, pavd_z100_105m, pavd_z105_110m, 
                                                    pavd_z110_115m, pavd_z115_120m, pavd_z120_125m, pavd_z125_130m, pavd_z130_135m, pavd_z135_140m, 
                                                    pavd_z140_145m, pavd_z145_150m))

pavd_z_40_plus = subset(gedi_data_2A_2B, select = c(pavd_z30_35m, pavd_z35_40m, pavd_z40_45m, pavd_z45_50m, 
                                                    pavd_z50_55m, pavd_z55_60m, pavd_z60_65m, pavd_z65_70m, pavd_z70_75m, pavd_z75_80m,
                                                    pavd_z80_85m, pavd_z85_90m, pavd_z90_95m, pavd_z95_100m, pavd_z100_105m, pavd_z105_110m, 
                                                    pavd_z110_115m, pavd_z115_120m, pavd_z120_125m, pavd_z125_130m, pavd_z130_135m, pavd_z135_140m, 
                                                    pavd_z140_145m, pavd_z145_150m))

### Add up rows across z bin profiles to generate summarized z bin metrics for cover, pai, and pavd
cover_z_10_plus = rowSums(cover_z_10_plus[,1:ncol(cover_z_10_plus)])
cover_z_10_plus

cover_z_20_plus = rowSums(cover_z_20_plus[,1:ncol(cover_z_20_plus)])
cover_z_20_plus

cover_z_40_plus = rowSums(cover_z_40_plus[,1:ncol(cover_z_40_plus)])
cover_z_40_plus

pai_z_10_plus = rowSums(pai_z_10_plus[,1:ncol(pai_z_10_plus)])
pai_z_10_plus

pai_z_20_plus = rowSums(pai_z_20_plus[,1:ncol(pai_z_20_plus)])
pai_z_20_plus

pai_z_40_plus = rowSums(pai_z_40_plus[,1:ncol(pai_z_40_plus)])
pai_z_40_plus

pavd_z_10_plus = rowSums(pavd_z_10_plus[,1:ncol(pavd_z_10_plus)])
pavd_z_10_plus

pavd_z_20_plus = rowSums(pavd_z_20_plus[,1:ncol(pavd_z_20_plus)])
pavd_z_20_plus

pavd_z_40_plus = rowSums(pavd_z_40_plus[,1:ncol(pavd_z_40_plus)])
pavd_z_40_plus

### Bind the new calculated columns to the final dataset
gedi_data_2A_2B = cbind(gedi_data_2A_2B, 
                        cover_z_10_plus, cover_z_20_plus, cover_z_40_plus,
                        pai_z_10_plus, pai_z_20_plus, pai_z_40_plus, 
                        pavd_z_10_plus, pavd_z_20_plus, pavd_z_40_plus)
names(gedi_data_2A_2B)

### Calculate the number of mismatched shots that were dropped, export informational result to text file
diff_2A_2B <- nrow_2A - nrow_2B
diff_2A_2B <- abs(diff_2A_2B)
setwd(gedi_processing_stats_output)
cat(diff_2A_2B, "mismatched shots dropped when merging spatially subset 2A and 2B Files", file="01_Number of Mismatched GEDI Shots.txt")

### Calculate the final number of shots after conditional filtering, export informational result to text file
filtered_gedi_data_rows <- nrow(gedi_data_2A_2B)
setwd(gedi_processing_stats_output)
cat(filtered_gedi_data_rows, "GEDI shot observations after conditional filtering", file="02_Number of Final Filtered GEDI Shots.txt")

###________________________________________________________________________________________###
### Converting merged 2A_2B dataframe to spatial points, exporting csv and ESRI shapefiles ###
###________________________________________________________________________________________###

### Generating a custom output name for the shapefile object and csv object
date_start <- noquote(as.character(daterange[1]))
date_end <- noquote((as.character(daterange[2])))
out_shp_name <- paste0("GEDI_2A_2B_merged_filtered_", date_start, "_", date_end, ".shp")
out_csv_name <- paste0("GEDI_2A_2B_merged_filtered_", date_start, "_", date_end, ".csv")

### Converting to spatial points dataframe, reprojecting to EPSG 5070
gedi_data_2A_2B_spdf <- SpatialPointsDataFrame(cbind(gedi_data_2A_2B$lon_lowestmode,gedi_data_2A_2B$lat_lowestmode), data=gedi_data_2A_2B)
proj4string(gedi_data_2A_2B_spdf) = CRS("+init=epsg:4326")
gedi_data_2A_2B_spdf <- spTransform(gedi_data_2A_2B_spdf, CRS("+init=epsg:5070")) #Reproject to 5070
proj4string(gedi_data_2A_2B_spdf) = CRS("+init=epsg:5070")
gedi_data_2A_2B_spdf

### writing out CSV file for storage
gedi_data_2A_2B = as.data.frame(gedi_data_2A_2B_spdf)
setwd(gedi_processed_output)
fwrite(gedi_data_2A_2B, file = out_csv_name, sep = ",")
rm(gedi_data_2A_2B)
gc()

### Writing out the simple features file as a shapefile
#out_shp <- st_as_sf(gedi_data_2A_2B_spdf)
#setwd(gedi_processed_output_shp)
#st_write(out_shp, out_shp_name, append=FALSE)

###_________________________________________________________________________________________________________###
###  Intermediate step here - Read in the merged 2A and 2B dataset, convert to SPDF, reproject to EPSG 5070 ###
###_________________________________________________________________________________________________________###

### Generate the input csv name using the start and end dates
date_start <- noquote(as.character(daterange[1]))
date_end <- noquote((as.character(daterange[2])))
out_csv_name <- paste0("GEDI_2A_2B_merged_filtered_", date_start, "_", date_end, ".csv")

### Reading back in the GEDI dataframe as an intermediate step
setwd(gedi_processed_output)
gedi_data_2A_2B <- fread(out_csv_name)
gedi_data_2A_2B$shot_number <- as.character(gedi_data_2A_2B$shot_number)
gedi_data_2A_2B$delta_time = as.Date(gedi_data_2A_2B$delta_time)
head(gedi_data_2A_2B)
names(gedi_data_2A_2B)

###_____________________________________________________________________________________________________###
### Subset GEDI data down to desired columns only, randomly sample observations from the entire dataset ###
###_____________________________________________________________________________________________________###

### Conditional Filtering to retain desired response variables for randomForest Modeling
subset_gedi_data_2A_2B_spdf <- subset(gedi_data_2A_2B, select = -c(beam, algorithmrun_flag, degrade_flag, l2b_quality_flag, 
                                                                   stale_return_flag, surface_flag, solar_elevation, delta_time,
                                                                   local_beam_elevation, sensitivity, pgap_theta, rhov, rhog, omega))
head(subset_gedi_data_2A_2B_spdf)
names(subset_gedi_data_2A_2B_spdf)

### Randomly Sample the GEDI shots down to the desired number of final points for modeling, export as .csv
set.seed(123)
random_sampled_2A_2B_GEDI_shots <- slice_sample(subset_gedi_data_2A_2B_spdf,  n = num_samples)
setwd(gedi_processed_output_samples)
getwd()
out_csv_name <- paste0("random_sampled_2A_2B_GEDI_shots_", date_start, "_", date_end, ".csv")
fwrite(random_sampled_2A_2B_GEDI_shots, file = out_csv_name, sep = ",")

### Converting random stratified points to shapefile and exporting
random_sampled_2A_2B_GEDI_shots_spdf <- SpatialPointsDataFrame(cbind(random_sampled_2A_2B_GEDI_shots$coords.x1,random_sampled_2A_2B_GEDI_shots$coords.x2), data=random_sampled_2A_2B_GEDI_shots)
proj4string(random_sampled_2A_2B_GEDI_shots_spdf) = CRS("+init=epsg:5070")
out_shp <- st_as_sf(random_sampled_2A_2B_GEDI_shots_spdf)
setwd(gedi_processed_output_samples)
out_shp_name <- paste0("random_sampled_2A_2B_GEDI_shots_", date_start, "_", date_end, ".shp")
st_write(out_shp, out_shp_name, append=FALSE)

###______________________________________________________________________________________###
###  Random Stratified Sampling of GEDI SHOTS                                            ###
###______________________________________________________________________________________###

### Getting the total number of rows in the full dataset
total <- nrow(gedi_data_2A_2B)
total

### Getting the numbers for 1_10% cover
cover_1_10 = filter(subset_gedi_data_2A_2B_spdf, ( (cover >= 0) & (cover <= 0.1) ))
nrow_cover <- nrow(cover_1_10)
cov_percentage <- nrow_cover / total
cov_num_samples <- cov_percentage*num_samples
cov_num_samples <- round(as.numeric(cov_num_samples, digits=0))
set.seed(123)
cover_1_10 <- slice_sample(cover_1_10,  n = cov_num_samples)
cover_1_10

### Getting the numbers for 10_25% cover
cover_10_25 = filter(subset_gedi_data_2A_2B_spdf, ( (cover >= 0.1000) & (cover <= 0.249999999999) ))
nrow_cover <- nrow(cover_10_25)
cov_percentage <- nrow_cover / total
cov_num_samples <- cov_percentage*num_samples
cov_num_samples <- round(as.numeric(cov_num_samples, digits=0))
set.seed(123)
cover_10_25 <- slice_sample(cover_10_25,  n = cov_num_samples)
cover_10_25

### Getting the numbers for 25_50% cover
cover_25_50 = filter(subset_gedi_data_2A_2B_spdf, ( (cover >= 0.25000) & (cover <= 0.49999999999) ))
nrow_cover <- nrow(cover_25_50)
cov_percentage <- nrow_cover / total
cov_num_samples <- cov_percentage*num_samples
cov_num_samples <- round(as.numeric(cov_num_samples, digits=0))
set.seed(123)
cover_25_50 <- slice_sample(cover_25_50,  n = cov_num_samples)
cover_25_50

### Getting the numbers for 50_75% cover
cover_50_75 = filter(subset_gedi_data_2A_2B_spdf, ( (cover >= 0.50000) & (cover <= 0.749999999999) ))
nrow_cover <- nrow(cover_50_75)
cov_percentage <- nrow_cover / total
cov_num_samples <- cov_percentage*num_samples
cov_num_samples <- round(as.numeric(cov_num_samples, digits=0))
set.seed(123)
cover_50_75 <- slice_sample(cover_50_75,  n = cov_num_samples)
cover_50_75

### Getting the numbers for 75_100% cover
cover_75_100 = filter(subset_gedi_data_2A_2B_spdf, ( (cover >= 0.75000) & (cover <= 1) ))
nrow_cover <- nrow(cover_75_100)
cov_percentage <- nrow_cover / total
cov_num_samples <- cov_percentage*num_samples
cov_num_samples <- round(as.numeric(cov_num_samples, digits=0))
set.seed(123)
cover_75_100 <- slice_sample(cover_75_100,  n = cov_num_samples)
cover_75_100

### Bind the randomly stratified points for each cover class into a composite dataframe
random_stratified_2A_2B_GEDI_shots <- rbind(cover_1_10, cover_10_25, cover_25_50, cover_50_75, cover_75_100)
nrow(random_stratified_2A_2B_GEDI_shots)
nrow(subset_gedi_data_2A_2B_spdf)
summary(subset_gedi_data_2A_2B_spdf)

### Exporting the random stratified samples as .csv
setwd(gedi_processed_output_samples)
out_csv_name <- paste0("random_stratified_2A_2B_GEDI_shots_", date_start, "_", date_end, ".csv")
fwrite(random_stratified_2A_2B_GEDI_shots, file = out_csv_name, sep = ",")

### Converting random stratified points to shapefile and exporting
random_stratified_2A_2B_GEDI_shots_spdf <- SpatialPointsDataFrame(cbind(random_stratified_2A_2B_GEDI_shots$coords.x1,random_stratified_2A_2B_GEDI_shots$coords.x2), data=random_stratified_2A_2B_GEDI_shots)
proj4string(random_stratified_2A_2B_GEDI_shots_spdf) = CRS("+init=epsg:5070")
out_shp <- st_as_sf(random_stratified_2A_2B_GEDI_shots_spdf)
setwd(gedi_processed_output_samples)
out_shp_name <- paste0("random_stratified_2A_2B_GEDI_shots_", date_start, "_", date_end, ".shp")
st_write(out_shp, out_shp_name, append=FALSE)

###____________________________________________________________________________________________________________###
###  MINIMUM DISTANCE SAMPLING - thinning the GEDI dataset to retain 200 footprints per 3600 square kilometers ###
###____________________________________________________________________________________________________________###

### Filter the GEDI data to keep a subsample of points (every tenth footprint)
column_index <- 1:10
column_id <- as.data.frame(rep(column_index, len = nrow(subset_gedi_data_2A_2B_spdf)))
colnames(column_id) <- "column_id"
column_id

### Binding the column index to the subset gedi data, filtering to retain every nth observation, random sampling from this
thinned_gedi_data <- cbind(subset_gedi_data_2A_2B_spdf, column_id)
thinned_gedi_data <- filter(thinned_gedi_data, column_id == 1)
thinned_gedi_data <- slice_sample(thinned_gedi_data,  n = 150000)
thinned_gedi_data <- SpatialPointsDataFrame(cbind(thinned_gedi_data$coords.x1,thinned_gedi_data$coords.x2), data=thinned_gedi_data)
proj4string(thinned_gedi_data) = CRS("+init=epsg:5070")
thinned_gedi_data = subset(thinned_gedi_data, select = -c(column_id))
nrow(thinned_gedi_data)
head(thinned_gedi_data)

### Create Shapefile Grid at 1m resolution for extent of the original GEDI dataset
tile_grid <- raster(extent(thinned_gedi_data))
res(tile_grid) <- 60000
proj4string(tile_grid)<-proj4string(thinned_gedi_data)
tile_grid <- rasterToPolygons(tile_grid)
mapview(tile_grid)
length(tile_grid)

### Create a list of polygons to tile the raster
tile_list <- setNames(split(tile_grid, seq(nrow(tile_grid))), rownames(tile_grid))
length(tile_list)

### Define function to automatically select nth GEDI footprints per tile grid cell
thin.max <- function(x, cols, npoints){
  #Create empty vector for output
  inds <- vector(mode="numeric")
  
  #Create distance matrix
  this.dist <- as.matrix(dist(x[,cols], upper=TRUE))
  
  #Draw first index at random
  inds <- c(inds, as.integer(runif(1, 1, length(this.dist[,1]))))
  
  #Get second index from maximally distant point from first one
  #Necessary because apply needs at least two columns or it'll barf
  #in the next bit
  inds <- c(inds, which.max(this.dist[,inds]))
  
  while(length(inds) < npoints){
    #For each point, find its distance to the closest point that's already been selected
    min.dists <- apply(this.dist[,inds], 1, min)
    
    #Select the point that is furthest from everything we've already selected
    this.ind <- which.max(min.dists)
    
    #Get rid of ties, if they exist
    if(length(this.ind) > 1){
      print("Breaking tie...")
      this.ind <- this.ind[1]
    }
    inds <- c(inds, this.ind)
  }
  
  return(x[inds,])
}

### Initiate the processing cluster
cl <- makeCluster(20)
registerDoParallel(cl)

thinned_gedi_data <- foreach(i=1:length(tile_list), .combine =rbind, .inorder = FALSE, .errorhandling="remove") %dopar% {
  
  library(raster)
  library(data.table)
  
  ### Get the first polygon tile in the list
  polygon_list_id = tile_list[[i]]
  polygon_list_id
  
  ### Crop the spatial points dataframe to the polygon object
  cropped_gedi_data <- crop(thinned_gedi_data, polygon_list_id)
  num_rows <- nrow(cropped_gedi_data@data)
  
  ### Conditional statement to run only if the cropped SP dataframe is larger than 1 row
  if(num_rows >= 1){
    
    ### Spatially thin the cropped GEDI data within a given tile
    result <- thin.max(cropped_gedi_data@data, c("coords.x1", "coords.x2"), 225)
    return(result)
  }
  
  rm(polygon_list_id, cropped_gedi_data, result, extent, min, max, min_1, min_2, max_1, max_2, base_out_name)
  gc()
  
}

### Stop the processing cluster
stopCluster(cl)

nrow(thinned_gedi_data)

### Checking the merged thinned GEDI dataset
thinned_gedi_data <- slice_sample(thinned_gedi_data,  n = num_samples)
summary(thinned_gedi_data)
str(thinned_gedi_data)
nrow(thinned_gedi_data)

### Exporting the random stratified samples as .csv
setwd(gedi_processed_output_samples)
out_csv_name <- paste0("min_distance_thinned_2A_2B_GEDI_shots_", date_start, "_", date_end, ".csv")
fwrite(thinned_gedi_data, file = out_csv_name, sep = ",")

### Converting spatially thinned points to shapefile and exporting
thinned_gedi_data_2A_2B_GEDI_shots_spdf <- SpatialPointsDataFrame(cbind(thinned_gedi_data$coords.x1,thinned_gedi_data$coords.x2), data=thinned_gedi_data)
proj4string(thinned_gedi_data_2A_2B_GEDI_shots_spdf) = CRS("+init=epsg:5070")
out_shp <- st_as_sf(thinned_gedi_data_2A_2B_GEDI_shots_spdf)
setwd(gedi_processed_output_samples)
out_shp_name <- paste0("min_distance_thinned_2A_2B_GEDI_shots_", date_start, "_", date_end, ".shp")
st_write(out_shp, out_shp_name, append=FALSE)

###______________________________________###
### PATRICK's SPATIAL THINNING ALGORITHM ###
###______________________________________###

##Grid size (spacing between sample points)
#grid_size <- 10000 # in meters
#
#### Thin the GEDI data to just the shot numbers and coordinates
#thinned_gedi_data <- subset(gedi_data_2A_2B, select = c(shot_number, coords.x1, coords.x2))
#head(thinned_gedi_data)
#
##extent of data
#x_min_data <- min(thinned_gedi_data$coords.x1)
#x_max_data <- max(thinned_gedi_data$coords.x1)
#y_min_data <- min(thinned_gedi_data$coords.x2)
#y_max_data <- max(thinned_gedi_data$coords.x2)
#
#x_min <- x_min_data - x_min_data %% grid_size
#x_max <- x_max_data - x_max_data %% grid_size + grid_size
#y_min <- y_min_data - y_min_data %% grid_size
#y_max <- y_max_data - y_max_data %% grid_size + grid_size
#
##number of potential samples
#n_rows <- (x_max - x_min)/grid_size + 1
#n_cols <- (y_max - y_min)/grid_size + 1
#n_rows * n_cols
#
#ans <- expand.grid(seq(x_min, x_max, grid_size), seq(y_min, y_max, grid_size))
#ans
#
##answers
#ans <- expand.grid(seq(x_min, x_max, grid_size), seq(y_min, y_max, grid_size))
#ans <- data.frame(x_coord = ans$Var1, y_coord = ans$Var2)
#str(ans)
#
#head(ans); tail(ans)
#
#
##Distance function
#calc_distance <- function(x1,x2,y1,y2){
#  d <- sqrt((x2-x1)**2 + (y2-y1)**2)
#  return(d)
#}
#
#
## ---------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------
#
#### Initiate the processing cluster
#cl <- makeCluster(20)
#registerDoParallel(cl)
#
#### Loop through the tiles and spatially sample
#x <- foreach(i=1:nrow(ans), .combine="c") %dopar% {
#  
#  lower_x <- ans[i, "x_coord"] - (grid_size / 2) 
#  upper_x <- ans[i, "x_coord"] + (grid_size / 2) 
#  lower_y <- ans[i, "y_coord"] - (grid_size / 2) 
#  upper_y <- ans[i, "y_coord"] + (grid_size / 2) 
#  
#  temp <- thinned_gedi_data[
#    thinned_gedi_data$coords.x1 >= lower_x & 
#      thinned_gedi_data$coords.x1 < upper_x &
#      thinned_gedi_data$coords.x2 >= lower_y & 
#      thinned_gedi_data$coords.x2 < upper_y
#    , ]
#  
#  check <- nrow(temp) > 0
#  #check
#  if(check){
#    x_target <- ans[i, "x_coord"]
#    y_target <- ans[i, "y_coord"]
#    temp$distance <- calc_distance(x_target, temp$coords.x1, y_target, temp$coords.x2)
#    closest_shot <- temp[temp$distance == min(temp$distance), "shot_number"]
#    as.numeric(closest_shot)
#  } else {
#    closest_shot <- NA
#  }
#  
#  closest_shot
#  
#}
#
#
#### Stop the cluster
#stopCluster(cl)
#
#
#head(x)
#ans$shot_number <- x
#
## ---------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------
#
##Drop points that don't have a shot 
#ans <- ans[!is.na(ans$shot_number),]
#
##merge back in data
#head(gedi_data_2A_2B)
#
#dim(gedi_data_2A_2B)
#dim(ans)
#ans2 <- merge(x = gedi_data_2A_2B, y = ans, by = "shot_number", all.y = TRUE, all.x = FALSE)
#dim(ans2)
#
#head(ans2)
#nrow(ans2)
#
#### Exporting the random stratified samples as .csv
#setwd(gedi_processed_output_samples)
#out_csv_name <- paste0("pf_spatially_thinned_2A_2B_GEDI_shots_", date_start, "_", date_end, ".csv")
#fwrite(ans2, file = out_csv_name, sep = ",")
#
#### Converting spatially thinned points to shapefile and exporting
#ans_spdf <- SpatialPointsDataFrame(cbind(ans2$coords.x1,ans2$coords.x2), data=ans2)
#proj4string(thinned_gedi_data_2A_2B_GEDI_shots_spdf) = CRS("+init=epsg:5070")
#out_shp <- st_as_sf(ans_spdf)
#setwd(gedi_processed_output_samples)
#out_shp_name <- paste0("pf_spatially_thinned_2A_2B_GEDI_shots_", date_start, "_", date_end, ".shp")
#st_write(out_shp, out_shp_name, append=FALSE)

###___________________________________________________________________________________###
###  CONVERTING GEDI SHOTS TO A .LAS FILE FOR BETTER INTERACTIVE VEIWING OF ELEVATION ###
###___________________________________________________________________________________###

### Converting filtered gedi shots to spatial points dataframe, reprojecting to 5070, then back to dataframe
subset_gedi_data_2A_2B_spdf
for_las_gedi <- subset_gedi_data_2A_2B_spdf
for_las_gedi <- as.data.frame(for_las_gedi)
for_las_gedi

### Generating true Elevation LAS file ###
##########################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
x_las = round(for_las_gedi$coords.x1, digits=3)
y_las = round(for_las_gedi$coords.x2, digits=3)
z_las= round(for_las_gedi$elev_highestreturn, digits=3)

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_true_elevation.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating Elevation * 2 LAS file ###
##########################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$elev_highestreturn, digits=3)

### Exaggerating the Z axis of elevation for better visualization
z_las = z_las * 5

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_elevation_x_5.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating RH100 las File ###
#################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$rh100, digits=3)
z_las <- z_las * 100

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_rh100.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating Percent Cover las File ###
#########################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$cover, digits=3)
z_las <- z_las * 10000

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_pecent_cover.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating Percent FHD las File ###
#########################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$fhd_normal, digits=3)
z_las <- z_las * 1000

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_fhd.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating Percent cover profile las File ###
#################################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$cover_z_20_plus_total, digits=3)
z_las <- z_las * 10000

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_cover_z_20_plus_total.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating Percent cover profile las File ###
#################################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$cover_z_40_plus_total, digits=3)
z_las <- z_las * 10000

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_cover_z_40_plus_total.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating Percent pavd profile las File ###
#################################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$pavd_z_20_plus_total, digits=3)
z_las <- z_las * 100

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_pavd_z_20_plus_total.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

### Generating Percent pavd profile las File ###
#################################################
### Grabbing the reprojected coordinates for each gedi shot point, paired with altitude (elevation highest return)
z_las= round(for_las_gedi$pavd_z_40_plus_total, digits=3)
z_las <- z_las * 100

### Creating a las file for the extracted X, Y, and Z, writing out file
lasdata = data.frame(X = x_las, Y = y_las, Z = z_las)
lasheader = header_create(lasdata)
file = file.path(gedi_las_outputs, "gedi_shots_pavd_z_40_plus_total.las")
setwd(gedi_las_outputs)
write.las(file, lasheader, lasdata)

###_____________________________________________________________________________###
### Calculate the processing time, output to text file, and clear R environment ###
###_____________________________________________________________________________###
end_time <- Sys.time()
time_taken <- paste0(round(as.numeric(difftime(time1 = end_time, time2 = start_time, units = "hours")), 3), " Hours of Processing Time")
time_taken

setwd(gedi_processing_stats_output)
cat(time_taken, file="03_Processing time.txt")

###____________________________________________________________###
### Computing Summary Statistics for the merged 2A and 2B data ###
###____________________________________________________________###

### Adding a character identifier for different percent cover intervals to the dataset
cover_classes <- cut(gedi_data_2A_2B$cover, breaks = c(-Inf, 0.099999999, 0.249999999, 0.499999999, 0.74999999, Inf), labels = c("0-10%", "10-25%", "25-50%", "50-75%", "75-100%"), right = TRUE)
cover_classes
as.data.frame(cover_classes)
cover_classes <- as.character(cover_classes)
cover_classes  <- as.data.frame(cover_classes)

### Binding the new cover intervals with the original 2A_2B filtered dataframe
gedi_data_2A_2B_plotting <- cbind(gedi_data_2A_2B, cover_classes)
gedi_data_2A_2B_plotting
cover_distribution <- gedi_data_2A_2B_plotting %>% count(cover_classes)
cover_distribution

### Plotting percent cover by classes for the entire dataset
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=cover_classes)) + 
  xlab("Percent Cover (%)")+
  ylab("Count")+
  geom_bar(fill= "white", color = "black") 
p
setwd(gedi_summary_plots)
ggsave("01_GEDI_cover_binned_all_states.png", width = 8, height = 4, dpi = 300)

### Getting the shapefiles for each of the states
colorado <- study_area[study_area$NAME == "Colorado",]
wyoming <- study_area[study_area$NAME == "Wyoming",]
idaho <- study_area[study_area$NAME == "Idaho",]
montana <- study_area[study_area$NAME == "Montana",]
oregon <- study_area[study_area$NAME == "Oregon",]
washington <- study_area[study_area$NAME == "Washington",]

### Plotting the shapefiles for each of the states
plot(colorado)
plot(wyoming)
plot(idaho)
plot(montana)
plot(oregon)
plot(washington)

### Creating a new spatial points dataframe from the 2A_2B dataframe with the cover values attached
gedi_data_2A_2B_plotting_spdf <- SpatialPointsDataFrame(cbind(gedi_data_2A_2B_plotting$lon_lowestmode,gedi_data_2A_2B_plotting$lat_lowestmode), data=gedi_data_2A_2B_plotting)
proj4string(gedi_data_2A_2B_plotting_spdf) = CRS("+init=epsg:4326")

## Plotting Date Histogram for Filtered Data ##
###############################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=delta_time)) + 
  xlab("Date")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("00_GEDI_date.png", width = 8, height = 4, dpi = 300)

## Plotting Cover Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=cover)) + 
  xlab("Percent Cover (%)")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("01_GEDI_cover.png", width = 8, height = 4, dpi = 300)

## Plotting Foliage Height Diversity Histogram for Filtered Data ##
###################################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=fhd_normal)) + 
  xlab("Foliage Height Diversity")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("02_GEDI_fhd.png", width = 8, height = 4, dpi = 300)

## Plotting RH100 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh100)) + 
  xlab("Relative Height 100")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("03_GEDI_rh100.png", width = 8, height = 4, dpi = 300)

## Plotting RH95 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh95)) + 
  xlab("Relative Height 95")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("04_GEDI_rh95.png", width = 8, height = 4, dpi = 300)

## Plotting RH90 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh90)) + 
  xlab("Relative Height 90")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("05_GEDI_rh90.png", width = 8, height = 4, dpi = 300)

## Plotting RH85 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh85)) + 
  xlab("Relative Height 85")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("06_GEDI_rh85.png", width = 8, height = 4, dpi = 300)

## Plotting RH80 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh80)) + 
  xlab("Relative Height 80")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("07_GEDI_rh80.png", width = 8, height = 4, dpi = 300)

## Plotting RH75 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh75)) + 
  xlab("Relative Height 75")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("08_GEDI_rh75.png", width = 8, height = 4, dpi = 300)


## Plotting RH0 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh0)) + 
  xlab("Relative Height 0")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("09_GEDI_rh0.png", width = 8, height = 4, dpi = 300)

## Plotting RH5 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh5)) + 
  xlab("Relative Height 5")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("10_GEDI_rh5.png", width = 8, height = 4, dpi = 300)

## Plotting RH10 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh10)) + 
  xlab("Relative Height 10")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("11_GEDI_rh10.png", width = 8, height = 4, dpi = 300)

## Plotting RH15 Histogram for Filtered Data ##
################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=rh20)) + 
  xlab("Relative Height 20")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("12_GEDI_rh20.png", width = 8, height = 4, dpi = 300)

## Plotting Plant Area Index Histogram for Filtered Data ##
###########################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=pai)) + 
  xlab("Plant Area Index")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("13_GEDI_pai.png", width = 8, height = 4, dpi = 300)

## Plotting Plant Area Index Histogram for Filtered Data ##
###########################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=pai_z0_5m)) + 
  xlab("pai_z0_5m")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("14_GEDI_pai_z0_5m.png", width = 8, height = 4, dpi = 300)

## Plotting Plant Area Index Histogram for Filtered Data ##
###########################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=pai_z25_30m)) + 
  xlab("pai_z25_30m")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("15_GEDI_pai_z25_30m.png", width = 8, height = 4, dpi = 300)

## Plotting Plant Area Index Histogram for Filtered Data ##
###########################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=pavd_z0_5m)) + 
  xlab("pavd_z0_5m")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("16_GEDI_pavd_z0_5m.png", width = 8, height = 4, dpi = 300)

## Plotting Plant Area Index Histogram for Filtered Data ##
###########################################################
p <- ggplot(gedi_data_2A_2B_plotting, aes(x=pavd_z25_30m)) + 
  xlab("pavd_z25_30m")+
  ylab("Count")+
  geom_histogram(fill= "white", color = "black") 
p

setwd(gedi_summary_plots)
ggsave("17_GEDI_pavd_z25_30m.png", width = 8, height = 4, dpi = 300)


## Clearing R Environment ##
############################
rm(input_file, p, study_area, class_test, cover, daterange, diff_2A_2B, filtered_gedi_data_rows, lr_lat,
   lr_lon, nrow_2A, nrow_2B, target_beams, ul_lat, ul_lon, gedi_downloads_2A, gedi_downloads_2B, gedi_processed_output, 
   gedi_processed_output_shp, gedi_summary_plots, base_dir, study_area_input, end_time, i, k, list, names,
   start_time, granules, netrc, outName, usr, test, n, colorado, colorado_points, cover_1_10, cover_10_25, cover_25_50, cover_50_75,
   cover_75_100, cover_classes, cover_distribution, for_las_gedi, gedi_data_2A_2B_plotting, gedi_data_2A_2B_spdf, idaho, idaho_points,
   lasdata, lasheader, montana, montana_points, oregon, oregon_points, random_sampled_2A_2B_GEDI_shots, random_sampled_2A_2B_GEDI_shots_spdf, 
   random_stratified_2A_2B_GEDI_shots, random_stratified_2A_2B_GEDI_shots_spdf, washington, washington_points, wyoming, wyoming_points, x_las,
   y_las, z_las, total, num_samples, nrow_cover, cov_percentage, cov_num_samples, aria_exe, file, gedi_las_outputs, 
   gedi_processed_output, gedi_processing_stats_output, gedi_processed_output_samples, gedi_data_2A_2B_plotting_spdf, subset_2A_2B_GEDI_shots, 
   cl, out_shp, study_coords, study_coords_spdf, cropped_gedi_data, cropped_gedi_data.sub, dmat, extent, gedi_data_2A_2B_plotting, 
   gedi_data_2A_2B_spdf, input_csv_name, polygon_list_id, result, samples, subset_gedi_data_2A_2B_spdf, thinned_gedi_data, 
   thinned_gedi_data_2A_2B_GEDI_shots_spdf, gedi_data_2A_2B, tile_dimensions, tile_grid, tile_list, base_out_name, bbox, csv_list, date_end, date_start, 
   gedi_downloads_1B, lower_left_lat, lower_left_long, max, max_1, max_2, min, min_1, min_2, min.dist, num_cores, out_csv_name, process_1B, 
   temp_tile_dir, upper_right_lat, upper_right_long, x, thin.max, out_shp_name, column_id, column_index)

gc()

time_taken