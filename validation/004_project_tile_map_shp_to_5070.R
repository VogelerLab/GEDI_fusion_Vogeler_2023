rm(list=ls())

# Name: 004_project_tile_map_shp_to_5070.R
# Purpose: project validation tile maps shapefiles to 5070
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-03-03


cat("\n\n004_project_tile_map_shp_to_5070.R\n\n"); flush.console()


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
# Processing
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Year of lidar collection
yol <- read.csv(fp_yol, as.is = TRUE)

# boundary shapefiles
aois <- dir(dir_als_5070) 

for(aoi in aois){

	fp_out <- file.path(dir_als_tile_map_5070, paste0(aoi, "_tile_map.shp"))

	if(!file.exists(fp_out)){
		#directory to the las in 5070
		dir_als_5070_aoi <- file.path(dir_als_5070, aoi)
	
		#read catalog
		ctg <- catalog(dir_als_5070_aoi)

		#spatial polygon

		dat <- ctg@data[,c("filename", "geometry")]

		dat$File <- basename(dat$filename)
		dat$AOI <- aoi
		dat$File <- basename(dat$filename)
		dat$Tile <- sapply(strsplit(dat$File, "[.]"), "[", 1)
		dat$Filepath <- dat$filename

		dat <- dat[,c("AOI", "Tile", "File", "Filepath")]	

		# save new boundaries
		if(!dir.exists(dir_als_tile_map_5070)) dir.create(dir_als_tile_map_5070)
		st_write(dat, fp_out)
	}
}




