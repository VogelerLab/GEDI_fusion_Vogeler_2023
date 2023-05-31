rm(list=ls())

# Name: 030_copy_scripts_for_simulator_grids.R
# Purpose: Saves creates a of the scripts needed to run the gediSimulator and output grids
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-09

cat("\n\nRunning 030_copy_scripts_for_simulator_grids.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Source parameter file
source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")

dir_gedi_sim_windows_aoi <- file.path(dir_gedi_sim_windows, aoi)
dir_gedi_sim_windows_scripts <- file.path(dir_gedi_sim_windows_aoi, "scripts")


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



# -----------------------------------------------------------------------------
# Create a run script
# -----------------------------------------------------------------------------

#Create directories
if(!dir.exists(dir_gedi_sim_windows_aoi)) dir.create(dir_gedi_sim_windows_aoi)
if(!dir.exists(dir_gedi_sim_windows_scripts)) dir.create(dir_gedi_sim_windows_scripts)

fp_bash <- file.path(dir_gedi_sim_windows_scripts, "199_run_gedi_grids.sh")

#sink(fp_bash)

#Need to use write function with wb to ensure linux end of line returns
output.file <- file(fp_bash, "wb")

write("#!/bin/bash", file = output.file)
write(paste0("cd ", dir_gedi_sim_linux, aoi, "/scripts/"), file = output.file, append = TRUE)
write(paste("echo Running gediSimulator gridded metrics for", aoi), file = output.file, append = TRUE)

write("", file = output.file, append = TRUE)

write("# USAGE:", file = output.file, append = TRUE)
write("# python python 01_prepare_als.py -dir_las xxx -dir_out yyyy [opitonal switches]", file = output.file, append = TRUE)
write("# python 02_gediRat.py -dir_las xxx -dir_out yyyy [opitonal switches]", file = output.file, append = TRUE)
write("# python 03_gridMetric.py -dir_las xxx -dir_out yyyy [opitonal switches]", file = output.file, append = TRUE)
write("# python 04_rasterize_products.py -dir_las xxx -dir_out yyyy [opitonal switches]", file = output.file, append = TRUE)

write("", file = output.file, append = TRUE)

write(paste0('STUDY_AREA="', aoi, '"'), file = output.file, append = TRUE)
write(paste0('DIR_LAS="', dir_als_files_linux, '"'), file = output.file, append = TRUE)
write(paste0('DIR_OUT="', dir_gedi_sim_linux, aoi, '/gedi_als_gridded_metrics/', '"'), file = output.file, append = TRUE)
write("", file = output.file, append = TRUE)


write(paste0('python 101_prepare_als_less_ground.py -dir_las ${DIR_LAS} -dir_out ${DIR_OUT}'), file = output.file, append = TRUE)
write(paste0('python 102_gediRat.py -dir_out ${DIR_OUT} -n_cores_max ', 8), file = output.file, append = TRUE)
write(paste0('python 103_gridMetric.py -dir_out ${DIR_OUT} -n_cores_max ', 8), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras FHD'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras "rhReal 50"'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras "rhReal 75"'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras "rhReal 98"'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras cover'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras "ALS cover" '), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras pointDense'), file = output.file, append = TRUE)
#write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras beamDense'), file = output.file, append = TRUE)
#write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras gaussHalfCov'), file = output.file, append = TRUE)
#write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras maxHalfCov'), file = output.file, append = TRUE)
#write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras infHalfCov'), file = output.file, append = TRUE)
#write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras bayHalfCov'), file = output.file, append = TRUE)

#!!!!!!add pavdz
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI5t10'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI20t25'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI25t30'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI30t35'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI35t40'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI40t45'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI45t50'), file = output.file, append = TRUE)
write(paste0('python 104_rasterize_products.py -dir_out ${DIR_OUT} -out_ras tLAI50t55'), file = output.file, append = TRUE)

close(output.file)

# -----------------------------------------------------------------------------
# Copy scripts
# -----------------------------------------------------------------------------

#copy the 1xx script
dir(dir_code_gedi, pattern = "^1")

if(!dir.exists(dir_gedi_sim_windows_aoi)) dir.create(dir_gedi_sim_windows_aoi)
if(!dir.exists(dir_gedi_sim_windows_scripts)) dir.create(dir_gedi_sim_windows_scripts)

for(f in dir(dir_code_gedi, pattern = "^1")){
	src <- file.path(dir_code_gedi, f)
	dest <- file.path(dir_gedi_sim_windows_scripts, f)
	file.copy(src, dest, overwrite = TRUE)
}

#copy these 2 scripts
f <- "gedi_sim_parameter_file.py"
src <- file.path(dir_code_gedi, f)
dest <- file.path(dir_gedi_sim_windows_scripts, f)
file.copy(src, dest, overwrite = TRUE)

f <- "lasheader_class.py"
src <- file.path(dir_code_gedi, f)
dest <- file.path(dir_gedi_sim_windows_scripts, f)
file.copy(src, dest, overwrite = TRUE)


# -----------------------------------------------------------------------------
# Print code to console
# -----------------------------------------------------------------------------
cat("\n")
cat("#", aoi, "\n")
cat("conda activate gedi_sim", "\n")
cat("bash ", dir_gedi_sim_linux, aoi, "/scripts/", basename(fp_bash), "\n", sep="")
cat("\n")



