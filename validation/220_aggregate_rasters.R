rm(list=ls())

# Name: 220_aggregate_rasters.R
# Purpose: create rasters that represent potential strata used when validating prediction maps
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-21


cat("\n\nRunning 220_aggregate_rasters.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(terra)
library(sf)
library(DBI)
library(RSQLite)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Source parameter file
source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")


# Terra Options
terraOptions(tempdir = dir_temp)
if(!dir.exists(dir_temp)) dir.create(dir_temp)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------




# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# ALS grids and prediction grids
# -----------------------------------------------------------------------------

cat("\n", aoi, "\n")
cat("Lidar collected in", lidar_year, "\n\n")


#rasters
dir(dir_model_pred_grids, pattern = "pred_year_")


for(pred_year in prediction_years){
	dir_pred_grids <- file.path(dir_model_pred_grids, paste0("pred_year_", pred_year), "01_aoi_clips")
	fn_rasters <- dir(dir_pred_grids, pattern = "[.]tif$")

 
	for(fn in fn_rasters){
		#file path
		fp_rast <- file.path(dir_pred_grids, fn)

		#ensure all extents are equal

		# Read in pixel values
		if(pred_year == prediction_years[1] & fn == fn_rasters[1]){
			r <- rast(fp_rast)
			vals <- values(r)

			#get coordinates, too
			vals <- cbind(vals, xyFromCell(r, 1:ncell(r)))
	
			#reorder matrix
			vals <- vals[,c(2,3,1)]


		} else {
			r <- rast(fp_rast)
	
			if(!is.element(names(r), colnames(vals))){
				val <- values(r)
		
				#Add pixel values to data base
				vals = cbind(vals, val)
			}
		}
	}
}

# -----------------------------------------------------------------------------
# Add in simulated grids
# -----------------------------------------------------------------------------

sim_grids <- dir(dir_val_als_gedi_sim_grid_results, pattern = "[.]tif$")

for(sim_grid in sim_grids){

	#read in simulated grid
	fp_sim_grid <- file.path(dir_val_als_gedi_sim_grid_results, sim_grid)
	r_sim <- rast(fp_sim_grid)
	
	#make simulated grid equal in extent to other metrics
	#r_pred <- rast(fp_rast)

	# get the extent of the study area
	fp_shp_aoi <- file.path(dir_als_boundary_5070, paste0(aoi, ".shp")) 

	shp_aoi <- vect(fp_shp_aoi)
	shp_aoi_5070 <- st_transform(st_as_sf(shp_aoi), crs="+init=EPSG:5070")
	
	xmin <- floor(ext(shp_aoi_5070)$xmin)
	xmax <- ceiling(ext(shp_aoi_5070)$xmax)
	ymin <- floor(ext(shp_aoi_5070)$ymin)
	ymax <- ceiling(ext(shp_aoi_5070)$ymax)
	
	new_ext <- ext(c(xmin, xmax, ymin, ymax))

	r_extend <- extend(r_sim, new_ext)
	r_crop <- crop(r_extend, new_ext)
	r_mask <- mask(r_crop, vect(shp_aoi_5070))

	layer_name <- paste0("sim_", names(r_mask))
	names(r_mask) <- layer_name

	if(!is.element(names(r_mask), colnames(vals))){
		val <- values(r_mask)
		
		#Add pixel values to data base
		vals = cbind(vals, val)
	}
}

#Remove rows with NAs
dim(vals)
vals <- vals[complete.cases(vals), ]
dim(vals)

#convert to data frame
vals <- data.frame(vals)

#add in additional attributes 
vals$lidar_unit <- aoi
vals$lidar_year <- lidar_year


# Save as a sqlite database
if(!dir.exists(dir_val_sqlite)) dir.create(dir_val_sqlite)
con <- dbConnect(SQLite(), fp_val_sqlite)
dbWriteTable(conn = con, value = vals, name = "pixel_values", overwrite = TRUE)
dbDisconnect(con)

