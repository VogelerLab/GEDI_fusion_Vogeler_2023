rm(list=ls())

# Name: 011_calculate_fusion_metrics_at_gedi_shots.R
# Purpose: Calculate and area weighted estimate of gedi predictions at the 
#  gedi shot locations
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-02

cat("\n\nRunning 011_calculate_fusion_metrics_at_gedi_shots.R\n\n"); flush.console()

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

source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

run_cloudmetrics <- function(project, dir_lidar_clips, dir_output, dir_fusion){
    # PURPOSE: Run cloud metrics on all lidar files in a specified directory
    # INPUTS:
    #  project (str) - name of field inventory project
    #  dir_points (str) - file path to height normalized lidar files
    #  dir_output (str) - file path to fusion cloudmetrics
    #  dir_fusion (str) - file path to fusion executables
    # OUTPUTS:
    # CSV files of plot-level cloud metrics

    # CloudMetrics [switches] InputDataSpecifier OutputFileName
    switches <- "/MINHT:2 /ABOVE:2 /new /outlier:-30,150 /STRATA:0.5,1,2,4,8,16,32,48,64"
    
    las_files <- dir(dir_lidar_clips, pattern = "[.]las$", recursive = TRUE, full.names = TRUE)
    laz_files <- dir(dir_lidar_clips, pattern = "[.]laz$", recursive = TRUE, full.names = TRUE)

    fp_lidar_files_list <- file.path(dir_output, paste0("lidar_file_paths_", project, ".txt"))
    if(!dir.exists(dir_output)) dir.create(dir_output)
    sink(fp_lidar_files_list)
    for(i in las_files){
        cat(i)
        cat("\n")
    }
    for(i in laz_files){
        cat(i)
        cat("\n")
    }
    sink()


    InputDataSpecifier <- fp_lidar_files_list
    OutputFileName <- file.path(dir_output, paste0(project, "_AllRtns.csv"))

    cmd <- paste0(
        file.path(dir_fusion, "CloudMetrics.exe "),
        switches,
        " ",
        InputDataSpecifier,
        " ",
        OutputFileName
    )

    system(cmd)

    rm(switches)
    rm(InputDataSpecifier)
    rm(OutputFileName)
    rm(cmd)

    switches <- "/MINHT:2 /ABOVE:2 /new /firstreturn /outlier:-30,150 /STRATA:0.5,1,2,4,8,16,32,48,64"
    InputDataSpecifier <- fp_lidar_files_list
    OutputFileName <- file.path(dir_output,  paste0(project, "_1stRtns.csv"))

    cmd = paste0(
        file.path(dir_fusion, "CloudMetrics.exe "),
        switches,
        " ",
        InputDataSpecifier,
        " ",
        OutputFileName
    )

    system(cmd)
}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Read Data
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Create FUSION cloud metrics
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


for(buff in buffers){
	cat("creating fusion metrics for buffer", buff, "\n")
	flush.console()

	dir_lidar_clips <- file.path(dir_val_shots_clips_normalized, paste0("buffer_", buff))
	dir_cloudmetrics <- file.path(dir_val_shots_cloudmetrics, paste0("buffer_", buff))
	
	if(!dir.exists(dir_val_shots_cloudmetrics)) dir.create(dir_val_shots_cloudmetrics)
	if(!dir.exists(dir_cloudmetrics)) dir.create(dir_cloudmetrics)

	run_cloudmetrics(
	    project = "plot_level_clips", 
	    dir_lidar_clips = dir_lidar_clips, 
	    dir_output = dir_cloudmetrics, 
	    dir_fusion
	)
}

