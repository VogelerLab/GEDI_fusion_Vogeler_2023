rm(list=ls())

# Name: 020_create_bash_script_for_simulator.R
# Purpose: Creates a bash script that can be used to generate simulated waveforms
#		(e.g., the calls to gediRat). Also move lidar clips to linux workspace
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-07

cat("\n\nRunning 020_create_bash_script_for_simulator.R\n\n"); flush.console()


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(lidR)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Source parameter file
source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")

dir_gedi_sim_windows_aoi <- file.path(dir_gedi_sim_windows, aoi)
dir_gedi_sim_windows_las <- file.path(dir_gedi_sim_windows_aoi, "las")
dir_gedi_sim_windows_scripts <- file.path(dir_gedi_sim_windows_aoi, "scripts")

dir_gedi_sim_linux_aoi <- paste0(dir_gedi_sim_linux, aoi)
dir_gedi_sim_linux_las <- paste0(dir_gedi_sim_linux_aoi, "/", "las")
dir_gedi_sim_linux_l1b <- paste0(dir_gedi_sim_linux_aoi, "/", "l1b")
dir_gedi_sim_linux_l2 <- paste0(dir_gedi_sim_linux_aoi, "/", "l2")



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

# Read in lidar unit boundary shapefile
fp_shp_aoi <- file.path(dir_als_boundary_5070, paste0(aoi, ".shp"))
poly_boundary <- read_sf(fp_shp_aoi, as_tibble = FALSE)

# Gedi shots that intersect the lidar unit
shp_intersecting_shots_4326 <- read_sf(fp_shp_intersecting_shots, as_tibble = FALSE)
#shp_intersecting_shots <- st_transform(shp_intersecting_shots_4326, crs=crs(poly_boundary))
shp_intersecting_shots <- st_transform(shp_intersecting_shots_4326, crs=5070)



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# get coordinates in the same CRS as the lidar
shp_intersecting_shots[,c("X", "Y")] <-  st_coordinates(shp_intersecting_shots)

df_intersecting_shots <- as.data.frame(shp_intersecting_shots[,c("shot_no", "X", "Y")])
head(df_intersecting_shots)


# Creates gediRat scripts
for(buff in buffers){

	# Lidar clips at GEDI footprints
	dir_las_buff <- file.path(dir_val_shots_gedi_sim_clips, paste0("buffer_", buff))
	als_clips <- dir(dir_las_buff)

	if(!dir.exists(dir_val_shots_gedi_sim_shot_scripts)) dir.create(dir_val_shots_gedi_sim_shot_scripts)

	#File path for the gediRat bash script
	fp_script <- file.path(dir_val_shots_gedi_sim_shot_scripts, paste0("01_gedi_sim_gediRat_shots_buff_", buff, ".sh"))

	#Need to use write function with wb to ensure linux end of line returns
	output.file <- file(fp_script, "wb")
	write(paste("echo Running gediRat for", aoi, "at", buff), file = output.file)
	write(paste("mkdir", dir_gedi_sim_linux_l1b), file = output.file, append = TRUE)
	write(paste("mkdir", paste0(dir_gedi_sim_linux_l1b, "/", "buffer_", buff)), file = output.file, append = TRUE)

	for(als_clip in als_clips){
		#shot number
		shot_no <- sub(".las", "", als_clip)

		#coords
		X <- df_intersecting_shots[df_intersecting_shots$shot_no == shot_no, "X"]
		Y <- df_intersecting_shots[df_intersecting_shots$shot_no == shot_no, "Y"]

		#file paths in linux format
		fp_las_in_linux <- paste0(dir_gedi_sim_linux_las, "/", "buffer_", buff, "/", als_clip)
		fp_hd5_out_linux <- paste0(dir_gedi_sim_linux_l1b, "/", "buffer_", buff, "/", shot_no, ".h5")

		#command
		cmd <- paste0(
			#"singularity exec --bind /mnt ~/singularity-ce-3.10.0/gediSingularity gediRat -input ", #Gates
			"singularity exec --bind /mnt ~/gediSingularity gediRat -input ", #Denali
			fp_las_in_linux, " ",
			"-coord ", X, " ", Y, " ",
			"-output ", fp_hd5_out_linux, " ",
			"-waveID ", shot_no, " ",
		#!!!!	"-fSigma ", buff * 2, " ",
			"-ground -hdf"
		)

		#write commands to disk
		write(cmd, file = output.file, append=TRUE)
	}
	close(output.file)
}



# Creates gediMetric scripts
for(buff in buffers){

	# Lidar clips at GEDI footprints
	dir_las_buff <- file.path(dir_val_shots_gedi_sim_clips, paste0("buffer_", buff))
	als_clips <- dir(dir_las_buff)

	if(!dir.exists(dir_val_shots_gedi_sim_shot_scripts)) dir.create(dir_val_shots_gedi_sim_shot_scripts)

	#File path for the gediMetric bash script
	fp_script <- file.path(dir_val_shots_gedi_sim_shot_scripts, paste0("02_gedi_sim_gediMetric_shots_buff_", buff, ".sh"))

	output.file <- file(fp_script, "wb")
	write(paste("echo Running gediMetrics for", aoi, "at", buff), file = output.file)
	write(paste("mkdir", dir_gedi_sim_linux_l2), file = output.file, append = TRUE)
	write(paste("mkdir", paste0(dir_gedi_sim_linux_l2, "/", "buffer_", buff)), file = output.file, append = TRUE)

	for(als_clip in als_clips){
		#shot number
		shot_no <- sub(".las", "", als_clip)

		#file paths in linux format
		fp_hd5_in_linux <- paste0(dir_gedi_sim_linux_l1b, "/", "buffer_", buff, "/", shot_no, ".h5")
		dir_out_linux <- paste0(dir_gedi_sim_linux_l2, "/", "buffer_", buff, "/", shot_no)

		#command
		cmd <- paste0(
			#"singularity exec --bind /mnt ~/singularity-ce-3.10.0/gediSingularity ", #Gates
			"singularity exec --bind /mnt ~/gediSingularity ", #Denali
			"gediMetric ",
			"-input ", fp_hd5_in_linux, " ",
			"-outRoot ", dir_out_linux, " ",
			"-readHDFgedi ",
			"-ground ", 
			"-rhRes 1 ",
			"-laiRes 5 ", 
			"-laiH 50 "
		)

		#write commands to disk
		write(cmd, file = output.file, append=TRUE)
	}
	close(output.file)
}



#Copy lidar files to linux processing workspace
for(buff in buffers){

	dir_las_buff <- file.path(dir_val_shots_gedi_sim_clips, paste0("buffer_", buff))
	files_to_copy <- dir(dir_las_buff)
	for(f in files_to_copy){
		if(!dir.exists(dir_gedi_sim_windows)) dir.create(dir_gedi_sim_windows)
		if(!dir.exists(dir_gedi_sim_windows_aoi)) dir.create(dir_gedi_sim_windows_aoi)
		if(!dir.exists(dir_gedi_sim_windows_las)) dir.create(dir_gedi_sim_windows_las)
		if(!dir.exists(file.path(dir_gedi_sim_windows_las, paste0("buffer_", buff)))) dir.create(file.path(dir_gedi_sim_windows_las, paste0("buffer_", buff)))
		file.copy(file.path(dir_las_buff, f), file.path(dir_gedi_sim_windows_las, paste0("buffer_", buff), f))
	}
}



#Copy bash scripts to linux processing workspace
files_to_copy <- dir(dir_val_shots_gedi_sim_shot_scripts)
for(f in files_to_copy){
	if(!dir.exists(dir_gedi_sim_windows_scripts)) dir.create(dir_gedi_sim_windows_scripts)
	file.copy(
		file.path(dir_val_shots_gedi_sim_shot_scripts, f), 
		file.path(dir_gedi_sim_windows_scripts, f),
		overwrite = TRUE
	)
}

cat(paste0("

cd"), dir_gedi_sim_linux_aoi, 
paste0("
cd scripts

bash 01_gedi_sim_gediRat_shots_buff_12.5.sh

bash 02_gedi_sim_gediMetric_shots_buff_12.5.sh

"))
