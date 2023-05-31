rm(list=ls())

# Name: 012_calculate_model_predictions_at_gedi_shots.R
# Purpose: Calculate and area weighted estimate of gedi predictions at the 
#  gedi shot locations
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-02

cat("\n\n012_calculate_model_predictions_at_gedi_shots.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(doParallel)

sf::sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Source parameter file
source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")

# Number of cores for parallel processing
n_cores <- length(prediction_years)

# Terra Options
terraOptions(tempdir = dir_temp)
if(!dir.exists(dir_temp)) dir.create(dir_temp)

#gedi prediction metrics
gedi_pred_metrics <- c(
	"cover", 
	"rh50", 
	"rh75", 
	"rh98", 
	"fhd", 
	"pavd_z_20", 
	"pavd_z_40", 
	"pavd_z5_10m", 
	"als_elevation", 
	"als_slope"
)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Read Data
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# polygon of gedi shots that intersect study are
intersecting_shots <- read_sf(fp_shp_intersecting_shots, as_tibble = FALSE)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

#project gedi shots into crs of prediction maps
intersecting_shots_5070 <- st_transform(intersecting_shots, crs = st_crs(5070))

cat(nrow(intersecting_shots_5070), "intersection shots", "\n")
flush.console()


AA <- Sys.time()

cl <- makeCluster(n_cores)
registerDoParallel(cl)

df_ans <-foreach(j=1:length(prediction_years), .combine=rbind, .packages=c("sf", "terra")) %dopar% {

	pred_year <- prediction_years[j]

	df_year <- data.frame()
	counter <- 1

	cat("prediction year ", pred_year, "\n")
	flush.console()
	
	dir_val_pred_year <- file.path(dir_model_pred_grids, paste0("pred_year_", pred_year)) # directory for a given gedi prediction year
	dir_val_rasters <- file.path(dir_val_pred_year, "01_aoi_clips") # directory of clipped rasters

	#gedi prediction rasters
	val_rasters <- dir(dir_val_rasters)
	prediction_rasters <- vector()
	for(gedi_metric in gedi_pred_metrics){
			prediction_rasters <- c(prediction_rasters, val_rasters[startsWith(val_rasters, gedi_metric)])
	}

	#Extract a clip of the prediction raster using the dtm-buffer radius
	for(i in 1:nrow(intersecting_shots_5070)){

		# shot number
		shot_no <- intersecting_shots_5070[i,]$shot_no

		#buffer shot center by buffer_radius_dtm
		shot_dtm_buffer <- st_buffer(x = intersecting_shots_5070[i,], dist = buffer_radius_dtm)

		for(pred_raster in prediction_rasters){
			#crop prediction map using shot_dtm_buffer
			fp_pred_raster <- file.path(dir_val_rasters, pred_raster)
			rast_pred_raster <- rast(fp_pred_raster)
			chip_big <- crop(x = rast_pred_raster, y = shot_dtm_buffer)
	
			#resample to 1 meter
			s <- rast(nrows=nrow(chip_big)*30, ncols=ncol(chip_big)*30, extent = ext(chip_big))
			chip <- resample(chip_big, s, method="average")

			for(buff in buffers){
				# buffered gedi shot
				poly_gedi_buff <- st_buffer(x = intersecting_shots_5070[i,], dist = buff)
	
				# extract pixels inside buffered shot
				mask_buff <- mask(chip, vect(poly_gedi_buff))
				vals <- values(mask_buff, mat = FALSE)
				mean_metric <- mean(vals, na.rm=TRUE)

				# populate answer data frame
				df_year[counter, "shot_no"] <- shot_no
				df_year[counter, "prediction_year"] <- pred_year
				df_year[counter, "prediction_metric"] <- sub(".tif", "", pred_raster)
				df_year[counter, "buff"] <- buff
				df_year[counter, "mean_value"] <- mean_metric

				counter <- counter + 1

			}# end for buff
		} #end for pred_raster

	}# end for i
	df_year
	
} #end for pred year
stopCluster(cl)

BB <- Sys.time()
print(BB - AA)


head(df_ans)
tail(df_ans)
dim(df_ans)



# Now save df_ans
for(pred_metric in unique(df_ans$prediction_metric)){
	for(buff in unique(df_ans$buff)){
		ans <- df_ans[df_ans$prediction_metric == pred_metric & df_ans$buff == buff,]

		fp_out <- file.path(dir_val_pred_at_gedi, paste0("map_preds_at_gedi_shots_", pred_metric, "_", buff, "m.csv"))
		if(!dir.exists(dir_val_pred_at_gedi)) dir.create(dir_val_pred_at_gedi)

		# Write file
		write.csv(ans, file = fp_out, row.names = FALSE)
	}
}

