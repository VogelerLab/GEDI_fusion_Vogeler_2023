rm(list=ls())

# Name: 221_create_single_db_of_pixel_values.R
# Purpose: create rasters that represent potential strata used when validating prediction maps
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-21


cat("\n\nRunning 221_create_single_db_of_pixel_values.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(DBI)
library(RSQLite)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Source parameter file
source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------




# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# aois
aois <- dir(dir_val_aois)
drop <- grep("^_", aois, value=TRUE)
aois <- setdiff(aois, drop)
aois <- setdiff(aois, "IPNF2019I_test")

for(aoi in aois){
	cat(aoi, "\n")
	flush.console()

	#DB for all pixels
	fp_val_all_pixels_dat 

	#DB for aoi
	dir_val_aoi <- file.path(dir_val_aois, aoi) 
	dir_val_grids <- file.path(dir_val_aoi, "03_grid_level")
	dir_val_sqlite <- file.path(dir_val_grids, "04_sqlite_db")
      fp_val_sqlite <- file.path(dir_val_sqlite, paste0(aoi, ".db"))


	if(!dir.exists(dir_val_all_pixels)) dir.create(dir_val_all_pixels)
	if(!dir.exists(dir_val_all_pixels_dat)) dir.create(dir_val_all_pixels_dat)


	con_aoi <- dbConnect(SQLite(), fp_val_sqlite)
	vals <- dbReadTable(conn = con_aoi, name = "pixel_values")
	dbDisconnect(con_aoi)

	if(aoi == aois[1]){
		con_all <- dbConnect(SQLite(), fp_val_all_pixels_dat)
		dbWriteTable(conn = con_all, value = vals, name = "pixel_values", overwrite = TRUE)
		dbDisconnect(con_all)
	} else {
		con_all <- dbConnect(SQLite(), fp_val_all_pixels_dat)
		dbWriteTable(conn = con_all, value = vals, name = "pixel_values", append = TRUE)
		dbDisconnect(con_all)
	}
	
	rm(vals)
}

