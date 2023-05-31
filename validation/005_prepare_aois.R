rm(list=ls())

# Name: 005_prepare_aois.R
# Purpose: select and clip maps for ALS - modeled GEDI comparison
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-01-30


cat("\n\nRunning 005_prepare_aois.R\n\n"); flush.console()

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


get_model_id <- function(model){
	#returns a string that signifies either a global or landsat model
	model_id <- NA
	if(model == "global") model_id <- "l8_topo_bio_s1_dist"
	if(model == "landsat") model_id <- "l8_topo_bio"

	if(is.na(model_id)) return ("model must be 'global' or 'landsat'")
	return(model_id)
}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

for(pred_year in prediction_years){

	# Outputs
	dir_val_pred_year <- file.path(dir_model_pred_grids, paste0("pred_year_", pred_year)) # directory for a given gedi prediction year
	dir_val_rasters <- file.path(dir_val_pred_year, "01_aoi_clips") # directory of clipped rasters
	

	#Files to clip
	#dir, old_name, new_name
	
	mat <- matrix(c(
			# global model
			file.path(dir_pred_gedi, pred_year, paste0("cover_", model_id_g)), paste0("cover_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("cover_", model_id_g, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("fhd_normal_", model_id_g)), paste0("fhd_normal_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("fhd_normal_", model_id_g, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("pavd_z_20_plus_", model_id_g)), paste0("pavd_z_20_plus_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("pavd_z_20_plus_", model_id_g, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("pavd_z_40_plus_", model_id_g)), paste0("pavd_z_40_plus_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("pavd_z_40_plus_", model_id_g, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("pavd_z5_10m_", model_id_g)), paste0("pavd_z5_10m_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("pavd_z5_10m_", model_id_g, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("rh50_", model_id_g)), paste0("rh50_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("rh50_", model_id_g, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("rh75_", model_id_g)), paste0("rh75_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("rh75_", model_id_g, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("rh98_", model_id_g)), paste0("rh98_", model_id_g, "_merged_watermask_", pred_year, ".tif"), paste0("rh98_", model_id_g, "_", pred_year, ".tif"),

	
		#landsat model
			file.path(dir_pred_gedi, pred_year, paste0("cover_", model_id_l)), paste0("cover_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("cover_", model_id_l, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("fhd_normal_", model_id_l)), paste0("fhd_normal_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("fhd_normal_", model_id_l, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("pavd_z_20_plus_", model_id_l)), paste0("pavd_z_20_plus_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("pavd_z_20_plus_", model_id_l, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("pavd_z_40_plus_", model_id_l)), paste0("pavd_z_40_plus_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("pavd_z_40_plus_", model_id_l, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("pavd_z5_10m_", model_id_l)), paste0("pavd_z5_10m_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("pavd_z5_10m_", model_id_l, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("rh50_", model_id_l)), paste0("rh50_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("rh50_", model_id_l, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("rh75_", model_id_l)), paste0("rh75_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("rh75_", model_id_l, "_", pred_year, ".tif"),
			file.path(dir_pred_gedi, pred_year, paste0("rh98_", model_id_l)), paste0("rh98_", model_id_l, "_merged_watermask_", pred_year, ".tif"), paste0("rh98_", model_id_l, "_", pred_year, ".tif"),

			#ALS metrics
			#file.path(dir_als_aoi, aoi, "height_all_rtns_lidar"), paste0(aoi, "_pct_all_rtns_above_2m.tif"), "als_cover_all_rtn.tif",
			file.path(dir_als_aoi, aoi, "height_1st_rtns_lidar"), paste0(aoi, "_pct_1st_rtns_above_2m.tif"), "als_cover_1st_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_1st_rtns_lidar"), paste0(aoi, "_all_rtns_elev_P99.tif"), "als_P99_all_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_all_rtns_lidar"), paste0(aoi, "_all_rtns_elev_P95.tif"), "als_P95_all_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_all_rtns_lidar"), paste0(aoi, "_all_rtns_elev_P75.tif"), "als_P75_all_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_all_rtns_lidar"), paste0(aoi, "_all_rtns_elev_P50.tif"), "als_P50_all_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_all_rtns_lidar"), paste0(aoi, "_all_rtns_elev_P25.tif"), "als_P25_all_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_1st_rtns_lidar"), paste0(aoi, "_1st_rtns_elev_P99.tif"), "als_P99_1st_rtn.tif",
			file.path(dir_als_aoi, aoi, "height_1st_rtns_lidar"), paste0(aoi, "_1st_rtns_elev_P95.tif"), "als_P95_1st_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_1st_rtns_lidar"), paste0(aoi, "_1st_rtns_elev_P75.tif"), "als_P75_1st_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_1st_rtns_lidar"), paste0(aoi, "_1st_rtns_elev_P50.tif"), "als_P50_1st_rtn.tif",
			#file.path(dir_als_aoi, aoi, "height_1st_rtns_lidar"), paste0(aoi, "_1st_rtns_elev_P25.tif"), "als_P25_1st_rtn.tif",
			file.path(dir_als_aoi, aoi, "topo_lidar"), paste0(aoi, "_elevation.tif"), "als_elevation.tif",
			file.path(dir_als_aoi, aoi, "topo_lidar"),  paste0(aoi, "_slope_pct.tif"), "als_slope.tif"
		),
		ncol=3, byrow=TRUE
	)
	df <- as.data.frame(mat)
	names(df) <- c("dir_in", "old_name", "new_name")
	
	
	# get the extent of the study area
	shp_aoi <- vect(file.path(dir_als_boundary_5070, paste0(aoi, ".shp")))
	shp_aoi_5070 <- st_transform(st_as_sf(shp_aoi), crs="+init=EPSG:5070")
	
	xmin <- floor(ext(shp_aoi_5070)$xmin)
	xmax <- ceiling(ext(shp_aoi_5070)$xmax)
	ymin <- floor(ext(shp_aoi_5070)$ymin)
	ymax <- ceiling(ext(shp_aoi_5070)$ymax)
	
	new_ext <- ext(c(xmin, xmax, ymin, ymax))
	
	#Clip the rasters
	for(i in 1:nrow(df)){

		fp_in <- file.path(df[i,"dir_in"], df[i,"old_name"])
		fp_out <- file.path(dir_val_rasters, df[i,"new_name"])
	
		if(!file.exists(fp_out)){
			r_in <- rast(fp_in)
			r_extend <- extend(r_in, new_ext)
			r_crop <- crop(r_extend, new_ext)
			r_mask <- mask(r_crop, vect(shp_aoi_5070))

			layer_name <- sub(".tif", "", df[i,"new_name"])
			names(r_mask) <- layer_name
		
			if(!dir.exists(dir_val)) dir.create(dir_val)
			#if(!dir.exists(dir_val_footprint)) dir.create(dir_val_footprint)
			if(!dir.exists(dir_val_aois)) dir.create(dir_val_aois)
			if(!dir.exists(dir_val_aoi)) dir.create(dir_val_aoi)
			if(!dir.exists(dir_model_pred_grids)) dir.create(dir_model_pred_grids)
			if(!dir.exists(dir_val_pred_year)) dir.create(dir_val_pred_year)
			if(!dir.exists(dir_val_rasters)) dir.create(dir_val_rasters)
			writeRaster(r_mask, fp_out)

			rm(r_in)
			rm(r_extend)
			rm(r_crop)
			rm(r_mask)
		}
	}
	
	
	###NLCD
	#for(nlcd_year in nlcd_years){
	#
	#	fp_out <- file.path(dir_val_rasters, paste0("nlcd_", nlcd_year, ".tif"))
	#	if(file.exists(fp_out)) next
	#	
	#	fp_in <- get(paste0("fp_nlcd_", nlcd_year))
	#
	#	#read raster
	#	rast_ncld <- rast(fp_in)
	#
	#	# slightly buffered extent in NAD83
	#	e <- ext(new_ext$xmin-60, new_ext$xmax+60, new_ext$ymin-60, new_ext$ymax+60)
	#	nlcd_crop <- crop(rast_ncld, e)
	#
	#	#Force to NAD83 5070
	#	crs(nlcd_crop) <- "EPSG:5070"
	#
	#	#
	#	r_crop <- crop(nlcd_crop, new_ext)
	#	r_mask <- mask(r_crop, vect(shp_aoi_5070))
	#	
	#	#write
	#	writeRaster(r_mask, fp_out)
	#}
	
	##LCMAP
	
	fp_out <- file.path(dir_val_rasters, paste0("lcmap_", pred_year, ".tif"))
	if(file.exists(fp_out)) next
	
	fp_in <- file.path(dir_lcmap, paste0("LCMAP_CU_", pred_year, "_V13_LCPRI.tif"))
	#read raster
	rast_ncld <- rast(fp_in)
	
	# slightly buffered extent in NAD83
	e <- ext(new_ext$xmin-60, new_ext$xmax+60, new_ext$ymin-60, new_ext$ymax+60)
	nlcd_crop <- crop(rast_ncld, e)
	
	#Force to NAD83 5070
	crs(nlcd_crop) <- "EPSG:5070"
	
	#
	r_crop <- crop(nlcd_crop, new_ext)
	r_mask <- mask(r_crop, vect(shp_aoi_5070))
		
	#write
	writeRaster(r_mask, fp_out)
	
}

