rm(list=ls())

# Name: 002_project_shp_to_5070.R
# Purpose: project validation boundary shapefiles to 5070
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-03-03


cat("\n\n002_project_shp_to_5070.R\n\n"); flush.console()


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(sf)

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
fp_boundaries <- dir(dir_als_units, recursive = TRUE, pattern = "[.]shp$", full.names = TRUE)

for(f in fp_boundaries){

	#base name
	bn <- basename(f)

	fp_out <- file.path(dir_als_boundary_5070, bn)
	
	if(!file.exists(fp_out)){
		shp <- st_read(f)
		shp_trans <- st_transform(shp, 5070)

		#populate attribute table
		lidar_name <- sub(".shp", "", bn)
		year <- yol[yol$lidar_unit == lidar_name, "lidar_year"]
		shp_trans$name <- lidar_name
		shp_trans$year <- year
		shp_trans$area_km2 <- as.numeric(st_area(shp_trans)) / 1000000

		shp_trans <- shp_trans[,c("name", "year", "area_km2")]		

		# save new boundaries
		if(!dir.exists(dir_als_boundary_5070)) dir.create(dir_als_boundary_5070)
		st_write(shp_trans, fp_out)
	}
}



