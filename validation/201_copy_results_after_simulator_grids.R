rm(list=ls())

# Name: 201_copy_results_after_simulator_grids.R
# Purpose: Copies the gridded metrics created after running gediSimulator
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-13

cat("\n\nRunning 201_copy_results_after_simulator_grids.R\n\n"); flush.console()

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
#dir_gedi_sim_windows_scripts <- file.path(dir_gedi_sim_windows_aoi, "scripts")

#simulated gridded metrics
dir_gedi_sim_windows_grids <- file.path(dir_gedi_sim_windows_aoi, "gedi_als_gridded_metrics")

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

dir(dir_gedi_sim_windows_grids)

# -----------------------------------------------------------------------------
# Copy bash scripts
# -----------------------------------------------------------------------------
dir_grid_bash_scripts <- file.path(dir_gedi_sim_windows_grids, "bash_scripts")


#create directories
if(!dir.exists(dir_val_grids)) dir.create(dir_val_grids)
if(!dir.exists(dir_val_als_gedi_sim_grid_scripts)) dir.create(dir_val_als_gedi_sim_grid_scripts)

for(f in dir(dir_grid_bash_scripts, recursive = TRUE)){
	src <- file.path(dir_grid_bash_scripts, f)
	dest <- file.path(dir_val_als_gedi_sim_grid_scripts, f)
	dir_out <- dirname(dest)
	if(!dir.exists(dir_out)) dir.create(dir_out)

	file.copy(src, dest)
}
rm(src)
rm(dest)
rm(dir_out)


# -----------------------------------------------------------------------------
# Copy gridded products
# -----------------------------------------------------------------------------
dir_grids <- file.path(dir_gedi_sim_windows_grids, "gridded_products")

if(!dir.exists(dir_val_als_gedi_sim_grid_results)) dir.create(dir_val_als_gedi_sim_grid_results)

for(f in dir(dir_grids)){
	src <- file.path(dir_grids, f)
	dest <- file.path(dir_val_als_gedi_sim_grid_results, f)

	file.copy(src, dest)
}
rm(src)
rm(dest)


