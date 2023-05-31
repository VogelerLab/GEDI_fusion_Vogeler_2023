rm(list=ls())

# Name: 015_prepare_als_clips_for_simulator.R
# Purpose: Prepare the als clips for use in the gedi simulator. 
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-07


#Notes:
# - need to calculate height above ground
# - need to classify all returns < 60 cm as ground returns 
# - need to save as LAS files

cat("\n\nRunning 015_prepare_als_clips_for_simulator.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(lidR)


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

cat("\n\nPreparing lidar clips for gedi simulator:\n")
for(buff in buffers){

	cat("Buffer = ", buff, "\n")
	flush.console()
	#lidar clips
	dir_lidar_clips <- file.path(dir_val_shots_clips, paste0("buffer_", buff))
	clips_in <- dir(dir_lidar_clips)

	#output clips
	dir_out <- file.path(dir_val_shots_gedi_sim_clips, paste0("buffer_", buff))
	
	for(clip_in in clips_in){

		#basename of file
		bn <- sub(".laz", "", clip_in)

		#output file name
		fp_clip_out <- file.path(dir_out, paste0(bn, ".las"))
		if(file.exists(fp_clip_out)) next

		#file path in
		fp_clip_in <- file.path(dir_lidar_clips, clip_in)

		las <- readLAS(fp_clip_in)

		#calculate height above ground
		las <- normalize_height(las, algorithm = tin(), use_class = c(2L, 9L))
		
		#re-classify ground returns
		las@data$Classification <- ifelse(las@data$Z <= 0.6, as.integer(2L), las@data$Classification)

		#remove the normalization
		las <- unnormalize_height(las)

		#Write lasfile
		if(!dir.exists(dir_val_shots_gedi_sim_clips)) dir.create(dir_val_shots_gedi_sim_clips)
		if(!dir.exists(dir_out)) dir.create(dir_out)
		writeLAS(las, fp_clip_out)
	}
}


