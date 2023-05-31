
rm(list=ls())

# Name: 250_create_stratified_maps.R
# Purpose: create rasters that represent potential strata used when validating prediction maps
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-21


cat("\n\nRunning 250_create_stratified_maps.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(terra)
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

# Read sql database
con <- dbConnect(SQLite(), fp_val_sqlite)
vals <- dbReadTable(conn = con, name = "pixel_values")
dbDisconnect(con)


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

#ALS year
lidar_year


#try stratifying
names(vals)

#raster name to be stratified
layer <- "als_P99_all_rtn"

#output raster file path
fp_out <- file.path(dir_val_map_strata, paste0(layer, "_stratified.tif"))

#raster to be stratified
dir_val_pred_year <- file.path(dir_model_pred_grids, paste0("pred_year_", lidar_year)) # directory for a given gedi prediction year
dir_val_rasters <- file.path(dir_val_pred_year, "01_aoi_clips") # directory of clipped rasters
fp_ras <- file.path(dir_val_rasters, paste0(layer, ".tif"))

# quantiles to be used in the stratified (equal weighted)
ht_quant <- quantile(vals[, layer], probs = c(0,0.25,0.5,0.75,1))

#reclassification matrix
counter <- 1
rcv <- c()
for(i in 2:length(ht_quant)){
	rcv <- c(rcv, ht_quant[i-1], ht_quant[i], counter)
	counter <- counter + 1
}

rcl <- matrix(rcv, byrow = TRUE, ncol = 3)
rcl
rcl[nrow(rcl),2] <- rcl[nrow(rcl),2] + 1

if(!dir.exists(dir_val_map_strata)) dir.create(dir_val_map_strata)
r <- rast(fp_ras)
classify(x = r, rcl = rcl, right = FALSE, filename = fp_out, datatype = "INT2U", overwrite = T)



set.seed(123)
for(i in 2:length(ht_quart)){
	potential_rows <- vals[
		vals$ALL_RETURNS_elev_P99_2plus_30METERS >= ht_quart[1] & vals$ALL_RETURNS_elev_P99_2plus_30METERS < ht_quart[2], 
	]
}

sample_points <- potential_rows[sample(1:nrow(potential_rows), 10),]
sample_points