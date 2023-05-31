rm(list=ls())

# Name: 202_create_pavzd_grids.R
# Purpose: Copies the gridded metrics created after running gediSimulator
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-03-14

cat("\n\nRunning 202_create_pavzd_grids.R\n\n"); flush.console()

#tLAI0t10 - LAI at each height band (0-10m in this case) using the ALS ground estimate to isolate canopy returns


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(terra)

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
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# pavd_z5_10m
# -----------------------------------------------------------------------------

#This is just renaming the file.
fp_src <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI5t10", ".tif"))
fp_dest <- file.path(dir_val_als_gedi_sim_grid_results, paste0("sim_pavdz_5_10m", ".tif"))

file.copy(fp_src, fp_dest)


fp_pavdz_5_10m <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI5t10", ".tif"))
ras_pavdz_5_10m <- rast(fp_pavdz_5_10m)
names(ras_pavdz_5_10m) <- "pavdz_5_10m"

writeRaster(x = ras_pavdz_5_10m, filename = fp_dest, overwrite = TRUE)



# -----------------------------------------------------------------------------
# pavd_z_20_plus
# -----------------------------------------------------------------------------

fp_tLAI20t25 <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI20t25", ".tif"))
fp_tLAI25t30 <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI25t30", ".tif"))
fp_tLAI30t35 <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI30t35", ".tif"))
fp_tLAI35t40 <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI35t40", ".tif"))
fp_tLAI40t45 <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI40t45", ".tif"))
fp_tLAI45t50 <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI45t50", ".tif"))
fp_tLAI50t55 <- file.path(dir_val_als_gedi_sim_grid_results, paste0("tLAI50t55", ".tif"))



ras_tLAI20t25 <- rast(fp_tLAI20t25)
ras_tLAI25t30 <- rast(fp_tLAI25t30)
ras_tLAI30t35 <- rast(fp_tLAI30t35)
ras_tLAI35t40 <- rast(fp_tLAI35t40)
ras_tLAI40t45 <- rast(fp_tLAI40t45)
ras_tLAI45t50 <- rast(fp_tLAI45t50)
ras_tLAI50t55 <- rast(fp_tLAI50t55)


fp_pavd_z_20_plus <- file.path(dir_val_als_gedi_sim_grid_results, paste0("sim_pavd_z_20_plus", ".tif"))

ras_pavd_z_20_plus <- sum(
	ras_tLAI20t25,
	ras_tLAI25t30,
	ras_tLAI30t35,
	ras_tLAI35t40,
	ras_tLAI40t45,
	ras_tLAI45t50,
	ras_tLAI50t55
)

names(ras_pavd_z_20_plus) <- "pavd_z_20_plus"

writeRaster(x = ras_pavd_z_20_plus, filename = fp_pavd_z_20_plus, overwrite = TRUE)

# -----------------------------------------------------------------------------
# pavd_z_40_plus
# -----------------------------------------------------------------------------
fp_pavd_z_40_plus <- file.path(dir_val_als_gedi_sim_grid_results, paste0("sim_pavd_z_40_plus", ".tif"))

ras_pavd_z_40_plus <- sum(
	ras_tLAI40t45,
	ras_tLAI45t50,
	ras_tLAI50t55
)

names(ras_pavd_z_40_plus) <- "pavd_z_40_plus"

writeRaster(x = ras_pavd_z_40_plus, filename = fp_pavd_z_40_plus, overwrite = TRUE)


