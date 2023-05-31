rm(list=ls())

# Name: 203_difference_pred_and_sim_maps.R
# Purpose: subtract gedi prediction maps from gediSimulator maps
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-15

cat("\n\nRunning 203_difference_pred_and_sim_maps.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(sf)
library(terra)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")


#simulated gridded metrics
dir_gedi_sim_windows_aoi <- file.path(dir_gedi_sim_windows, aoi)
dir_gedi_sim_windows_grids <- file.path(dir_gedi_sim_windows_aoi, "gedi_als_gridded_metrics")


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

difference_maps <- function(x, y){
	#Purpose - subtract two maps
	# x - y
	# x filepath or terra rast object
	# y filepath or terra rast object

	#read in rasters
	if(class(x)!="SpatRaster"){
		x <- rast(x)
	}
	if(class(y)!="SpatRaster"){
		y <- rast(y)
	}

	# make the extent of the two rasters equal
	if(ext(x) != ext(y)){
		list_rast <- make_extents_equal(x, y)
		x <- list_rast[[1]]
		y <- list_rast[[2]]
		rm(list_rast)
	}
	# do the subtraction
	d <- x - y

	return(d)

}


make_extents_equal <- function(x, y){
	#Purpose - make the extent of 2 rast objects equal
	# x - y
	# x filepath or terra rast object
	# y filepath or terra rast object

	#read in rasters
	if(class(x)!="SpatRaster"){
		x <- rast(x)
	}
	if(class(y)!="SpatRaster"){
		y <- rast(y)
	}

	# ensure spatial extents are the same
	if(ext(x) != ext(y)){
		x_xmin <- ext(x)@ptr$vector[1]
		x_xmax <- ext(x)@ptr$vector[2]
		x_ymin <- ext(x)@ptr$vector[3]
		x_ymax <- ext(x)@ptr$vector[4]

		y_xmin <- ext(y)@ptr$vector[1]
		y_xmax <- ext(y)@ptr$vector[2]
		y_ymin <- ext(y)@ptr$vector[3]
		y_ymax <- ext(y)@ptr$vector[4]

		xmin <- min(x_xmin, y_xmin)
		xmax <- max(x_xmax, y_xmax)
		ymin <- min(x_ymin, y_ymin)
		ymax <- max(x_ymax, y_ymax)

		# new extent
		new_ext <- ext(xmin, xmax, ymin, ymax)
		
		x <- extend(x, new_ext)
		y <- extend(y, new_ext)

	}

	return(list(x,y))

}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

#gridded simulation results
dir(dir_val_als_gedi_sim_grid_results)

#ALS year
lidar_year


# gedi prediction maps
dir_val_pred_year <- file.path(dir_model_pred_grids, paste0("pred_year_", lidar_year)) # directory for a given gedi prediction year
dir_val_rasters <- file.path(dir_val_pred_year, "01_aoi_clips") # directory of clipped rasters


# directory of the differenced rasters
dir_val_gedi_sim_minus_pred_grid


rast_pairs <- list(
	
	c("rhReal_50", paste0("rh50_l8_topo_bio_s1_dist_", lidar_year)),
	c("rhReal_50", paste0("rh50_l8_topo_bio_", lidar_year)),
	c("rhReal_75", paste0("rh75_l8_topo_bio_s1_dist_", lidar_year)),
	c("rhReal_75", paste0("rh75_l8_topo_bio_", lidar_year)),
	c("rhReal_98", paste0("rh98_l8_topo_bio_s1_dist_", lidar_year)),
	c("rhReal_98", paste0("rh98_l8_topo_bio_", lidar_year)),
	c("FHD", paste0("fhd_normal_l8_topo_bio_s1_dist_", lidar_year)),
	c("FHD", paste0("fhd_normal_l8_topo_bio_", lidar_year)),
	c("cover", paste0("cover_l8_topo_bio_s1_dist_", lidar_year)),
	c("cover", paste0("cover_l8_topo_bio_", lidar_year)),

	c("sim_pavdz_5_10m", paste0("pavd_z5_10m_l8_topo_bio_s1_dist_", lidar_year)),
	c("sim_pavdz_5_10m", paste0("pavd_z5_10m_l8_topo_bio_", lidar_year)),
	c("sim_pavd_z_20_plus", paste0("pavd_z_20_plus_l8_topo_bio_s1_dist_", lidar_year)),
	c("sim_pavd_z_20_plus", paste0("pavd_z_20_plus_l8_topo_bio_", lidar_year)),
	c("sim_pavd_z_40_plus", paste0("pavd_z_40_plus_l8_topo_bio_s1_dist_", lidar_year)),
	c("sim_pavd_z_40_plus", paste0("pavd_z_40_plus_l8_topo_bio_", lidar_year))
	#c("", "")
	#c("", "")
)


for (rast_pair in rast_pairs){
	rast_1 <- rast_pair[1]
	rast_2 <- rast_pair[2]
	
	#output file
	fp_out <- file.path(dir_val_gedi_sim_minus_pred_grid, paste0(rast_1,"-", rast_2, ".tif"))
		
	if(!file.exists(fp_out)){
		# file paths to the rasters that will be differenced
		fp_x = file.path(dir_val_als_gedi_sim_grid_results, paste0(rast_1, ".tif"))
		fp_y = file.path(dir_val_rasters, paste0(rast_2, ".tif"))
			
		# Differencing
		d <- difference_maps(x = fp_x, y = fp_y)

		#Get rid of bad gediSimulator values
		d[abs(d > 201)] <- NA			
		
		#Saving
		if(!dir.exists(dir_val_gedi_sim_minus_pred_grid)) dir.create(dir_val_gedi_sim_minus_pred_grid)
				
		writeRaster(d, fp_out, overwrite=TRUE)
	}
}


