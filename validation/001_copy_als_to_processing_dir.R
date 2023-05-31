rm(list=ls())

# Name: 001_copy_als_to_processing_dir.R
# Purpose: Copy the ALS files to a directory where the processing will occur
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-03-01


cat("\n\n001_copy_als_to_processing_dir.R\n\n"); flush.console()


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(sf)
library(lidR)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Source parameter file
source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# List of Spatial References (EPSG codes)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

dict_als_crs = list(
	"Canyon_Creek_2018-06-15" = "",
	"OR_NRCSUSGS_2_2019" = 6557,
	"Olympics_2017" = 32149,
	"WA_Olympics_South_Opsw_2019" = 2927,
	"CO_SanLuisJuanMiguel_5_2020" = 6341,
	"CO_Southwest_NRCS_B1_2018" = 6350,
	"IPNF2019I" = "26911",
	"QuartzUpperJoe2016" = "6340",
	"tieton_2018" = "",
	"WA_Klickitat_3DEP_2019" = 2927,
	"payette2017QL1" = "",
	"USGS_ID_NorthForkPayette_2020" = "",
	"OR_RogueRiverSiskiyouNF_B1_2019" = 6339,
	"Siskiyou_2017" = "6318",
	"USGS_ID_Franklin_Bear_2017" = "",
	"WY_Southwest_1_2020" = 6341,
	"CO_Central_Western_2016" = 6428,
	"CO_DRCOG_3_2020" = 6342
)

#Overwrite existing data?
overwrite = TRUE


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

create_tile_map <- function(als_poly, als_crs, fp_out, dir_als){

	if(file.exists(fp_out) & !overwrite){
		cat(aoi, "already exists. Use overwrite = TRUE to replace.\n")
		cat("\tUsing previously generated polygon.\n"); flush.console()
		return(st_read(fp_out))
	} else {
		cat("starting", aoi, "\n"); flush.console()
	
		#make sure there are LAZ files
		#if(!dir.exists(dirLaz)) next
		if(length(dir(dir_als_files)) == 0){
			cat("  !! no LAZs in ", project, " !!\n")
			next
		}
  	
		#scan lidar files and create a LAScatalog
		ctg <- readLAScatalog(dir_als_files)
		#las_check(ctg)
	
		#plot the tiles
		#windows()
		#plot(ctg, main = project)
		
		# check for crs
		crsProject <- sp::CRS(paste0("+init=epsg:", als_crs))
		
		#spatial polygon
		dat <- ctg@data[,c("filename", "geometry")]
		st_crs(dat) <- crsProject

		dat$File <- basename(dat$filename)
		dat$AOI <- aoi
		dat$File <- basename(dat$filename)
		dat$Tile <- sapply(strsplit(dat$File, "[.]"), "[", 1)
		dat$Filepath <- dat$filename

		dat <- dat[,c("AOI", "Tile", "File", "Filepath")]

		
		#save spdf as shapefile
		# Write SHP
		sf::st_write(
			obj = dat, 
			dsn = fp_out, 
			layer = aoi 
			#delete_dsn=TRUE
		)
     
		
		cat("\n")
		cat("finished", aoi, "\n"); flush.console()
		
		rm(crsProject)
		rm(ctg)

		return(dat)
	} #end if exists

}


find_dir_als <- function(aoi){
	# Directory to LAZ files
	if(is.element(aoi, c(
			"OR_RogueRiverSiskiyouNF_B1_2019", 
			"OR_NRCSUSGS_2_2019",
			"WA_Klickitat_3DEP_2019",
			"WA_Olympics_South_Opsw_2019"
		))){
		dir_als_files <- file.path("L:", "Lidar", aoi, "Points", "LAZ")
	}
	if(is.element(aoi, c(
			"WY_Southwest_1_2020", 
			"CO_DRCOG_3_2020",
			"CO_SanLuisJuanMiguel_5_2020"		
		))){
		dir_als_files <- file.path("M:", "Lidar", aoi, "Points", "LAZ")
	}
	if(aoi == "tieton_2018") {
		dir_als_files <- r"(J:\gedi_lidar\tieton_2018\datasetsA\tieton_basin_2018\laz)"
	}
	if(aoi == "olympics_2017") {
		dir_als_files <- r"(J:\gedi_lidar\olympics_2017\datasetsA\olympics_2017\laz)"
	}
	if(aoi == "IPNF2019I") {
		dir_als_files <- r"(L:\Lidar\ID_IPNF_2019\Points\LAZ)"
	}
	if(aoi == "QuartzUpperJoe2016") {
		dir_als_files <- r"(I:\Data\LidarData\Idaho\QuartzUpperJoe2016\Points\LAZ)"
	}
	return(dir_als_files)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

aois <- c(
	"CO_SanLuisJuanMiguel_5_2020",
	"CO_DRCOG_3_2020",
	"WY_Southwest_1_2020",
	"QuartzUpperJoe2016", 
	"IPNF2019I", 
	"WA_Olympics_South_Opsw_2019",
	"WA_Klickitat_3DEP_2019", 
	"OR_NRCSUSGS_2_2019",
	"OR_RogueRiverSiskiyouNF_B1_2019", 
	"tieton_2018", 
	"olympics_2017"
)

for(aoi in aois){
	cat("processing", aoi, "\n")
	flush.console()
	lidar_year <- as.integer(df_init[df_init$aoi == aoi, "lidar_year"])
	ecoregion <- df_init[df_init$aoi == aoi, "ecoregion"]

	# -----------------------------------------------------------------------------
	# Read in polygon of aoi
	# -----------------------------------------------------------------------------
	#These are the aois for the map validation and for the shot-level
	fp_shp_aoi <- file.path(dir_als_units, ecoregion, aoi, paste0(aoi, ".shp"))

	poly_aoi <- st_read(fp_shp_aoi)


	# -----------------------------------------------------------------------------
	# create tile map
	# -----------------------------------------------------------------------------
	# create ctg of the als unit
	overwrite = FALSE

	als_poly = poly_aoi
	als_crs = dict_als_crs[aoi][[1]]
	dir_tile_map <- file.path(dir_data, "als_units", "tile_map")
	dir_tile_map_aoi <- file.path(dir_tile_map, aoi)
	if(!dir.exists(dir_tile_map)) dir.create(dir_tile_map)
	if(!dir.exists(dir_tile_map_aoi)) dir.create(dir_tile_map_aoi)

	fp_out = file.path(dir_tile_map_aoi, paste0("tile_map_", aoi, ".shp"))

	dir_als_files <- find_dir_als(aoi)


	tile_map <- create_tile_map(als_poly, als_crs, fp_out, dir_als)


	# intersection
	poly_aoi_trans <- st_transform(poly_aoi, crs=crs(tile_map))
	inter <- st_filter(tile_map, poly_aoi_trans)


	# copy laz files
	dir_out <- file.path(dir_als_data, aoi)
	if(!dir.exists(dir_als_data)) dir.create(dir_als_data)
	if(!dir.exists(dir_out)) dir.create(dir_out)


	for(i in 1:length(inter$Filepath)){
	
		fp_in <- inter$Filepath[i]
		f_out <- inter$File[i]
		fp_out <- file.path(dir_out, f_out)
		
		if(!file.exists(fp_out)){
			file.copy(from = fp_in, fp_out)
		}
	}
	rm(tile_map)
}




# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Recently downloaded (just the tiles that I need)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

aoi <- "Olympics_2017"

	#dir_als_files <- file.path(dir_als_data, aoi)
dir_als_files <- r"(O:\gedi_validation\data\als_data\Olympics_2017)"
	#scan lidar files and create a LAScatalog
	ctg <- readLAScatalog(dir_als_files)
	dat <- ctg@data[,c("filename", "geometry")]

	als_crs = dict_als_crs[aoi][[1]]

	# check for crs
	#st_set_crs(ctg) <- als_crs		

	#spatial polygon

		dat <- ctg@data[,c("filename", "geometry")]



	dat$File <- basename(dat$filename)
	dat$AOI <- aoi
	dat$File <- basename(dat$filename)
	dat$Tile <- sapply(strsplit(dat$File, "[.]"), "[", 1)
	dat$Filepath <- dat$filename

	dat <- dat[,c("AOI", "Tile", "File", "Filepath")]

	dir_tile_map <- file.path(dir_data, "als_units", "tile_map")
	dir_tile_map_aoi <- file.path(dir_tile_map, aoi)
	fp_out <- file.path(dir_tile_map_aoi, paste0("tile_map_", aoi, ".shp"))
		
	if(!dir.exists(dir_tile_map_aoi)) dir.create(dir_tile_map_aoi)

		#save spdf as shapefile
		# Write SHP
		sf::st_write(
			obj = dat, 
			dsn = fp_out, 
			layer = aoi 
			#delete_dsn=TRUE
		)
     
		
		cat("\n")
		cat("finished", aoi, "\n"); flush.console()
		
		rm(crsProject)
		rm(ctg)




	# -----------------------------------------------------------------------------
	# create tile map
	# -----------------------------------------------------------------------------
	# create ctg of the als unit
	overwrite = FALSE

	als_poly = poly_aoi
	als_crs = dict_als_crs[aoi][[1]]
	dir_tile_map <- file.path(dir_data, "als_units", "tile_map")
	dir_tile_map_aoi <- file.path(dir_tile_map, aoi)
	if(!dir.exists(dir_tile_map)) dir.create(dir_tile_map)
	if(!dir.exists(dir_tile_map_aoi)) dir.create(dir_tile_map_aoi)

	fp_out = file.path(dir_tile_map_aoi, paste0("tile_map_", aoi, ".shp"))




