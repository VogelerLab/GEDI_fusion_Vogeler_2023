# Name: 000_gedi_validation_parameters
# Purpose: parameters used in the gedi validation (models and maps)
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-06


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# processed lidar unit

aoi <- "CO_DRCOG_3_2020"

#Initializing matrix
mat_init <- matrix(c(
	#"aoi", "lidar_year", "ecoregion"

	"Canyon_Creek_2018-06-15", 2016, "Blue_Mountains",
	"OR_NRCSUSGS_2_2019", 2019, "Blue_Mountains",
	
	"WA_Olympics_South_Opsw_2019", 2019, "Coast_Range",
	"olympics_2017", 2017, "Coast_Range", 
	
	"CO_SanLuisJuanMiguel_5_2020", 2020, "Colorado_Plateaus",
	"CO_Southwest_NRCS_B1_2018", 2018, "Colorado_Plateaus",
	
	"IPNF2019I", 2019, "Columbia_Mountains_Northern_Rockies",
	"IPNF2019I_test", 2019, "Columbia_Mountains_Northern_Rockies",
	"QuartzUpperJoe2016", 2016, "Columbia_Mountains_Northern_Rockies",
	
	"WA_Klickitat_3DEP_2019", 2019, "Eastern_Cascades_Slopes_and_Foothills",
	"tieton_2018" ,2018, "Eastern_Cascades_Slopes_and_Foothills",
	
	"payette2017QL1", 2017, "Idaho_Batholith",
	"USGS_ID_NorthForkPayette_2020", 2020, "Idaho_Batholith",
	
	"OR_RogueRiverSiskiyouNF_B1_2019", 2019, "Klamath_Mountains",
	"Siskiyou_2017", 2017, "Klamath_Mountains",
	
	"WY_Southwest_1_2020", 2020, "Middle_Rockies",
	"USGS_ID_Franklin_Bear_2017", 2017, "Middle_Rockies",
	
	"CO_Central_Western_2016", 2016, "Southern_Rockies",
	"CO_DRCOG_3_2020", 2020, "Southern_Rockies"

), ncol = 3, byrow = TRUE)

colnames(mat_init) <- c("aoi", "lidar_year", "ecoregion")
df_init <- as.data.frame(mat_init)
rm(mat_init)


#
lidar_year <- as.integer(df_init[df_init$aoi == aoi, "lidar_year"])
ecoregion <- df_init[df_init$aoi == aoi, "ecoregion"]

# gedi model names (global and landsat)
model_id_g <-  "l8_topo_bio_s1_dist"
model_id_l <- "l8_topo_bio"


# Years that have gedi prediction rasters
prediction_years <- c(2016:2020)

# Years of NLCD layers to extract
nlcd_years <- c(2016, 2019)

# Buffers for clipping ALS at gedi locations
buffer_radius_dtm <- 56.5 # approximately 1 ha
#buffers <- c(30, 20, 12.5) # for lidar clips
buffers <- c(12.5) # for lidar clips


# Number of cores for various processes
n_core_pdal <- 4


#Number of pixels in the map validation sample per AOI
sample_size <- 1100 # use 1000 for dev

#random number seed = 193820 when selecting validation pixels
query_seed <- 193820


# -----------------------------------------------------------------------------
# Directories and file paths
# -----------------------------------------------------------------------------

# directory to RStor
dir_rstor <- file.path("N:","RStor","jvogeler","Lab")

# Main project directory
dir_gedi <- file.path(dir_rstor, "projects", "GEDI")

# Directory to these scripts
dir_code_gedi <- file.path(dir_rstor, "users", "pafekety", "code", "GEDI", "validation")

# Directory to the processing 
dir_processing <- file.path("E:", "gedi_validation") #denali
#dir_processing <- file.path("O:", "gedi_validation") #gates

# Directory to data
dir_data <- file.path(dir_processing, "data")
	dir_yol <- file.path(dir_data, "als_units", "year_of_lidar")
		fp_yol <- file.path(dir_yol, "year_of_lidar_collection.csv")
	dir_als_units <- file.path(dir_data, "als_units", "validation_aois")
	dir_als_boundary_5070 <- file.path(dir_data, "als_units", "validation_aois_5070")
	dir_als_tile_map_5070 <- file.path(dir_data, "als_units", "validation_tile_maps_5070")
	dir_als_5070 <- file.path(dir_data, "als_data_5070")
		dir_als_5070_aoi <- file.path(dir_als_5070, aoi)




#Consistent name and dir structure on the linux machine
#dir_als_files_linux <- paste0("/mnt/", "l/Lidar/", aoi, "/Points", "/LAZ/") #Gates
dir_als_files_linux <- paste0("/mnt/", "e/gedi_validation/data/als_data_5070/", aoi, "/") #Denali

# directory to gridded als lidar metrics
dir_als <- file.path(dir_rstor,"data","lidar","grid_metrics")
dir_als_aoi <- file.path(dir_als, aoi)

# Gridded GEDI predictions
dir_pred_gedi <- file.path(dir_rstor, "projects", "GEDI", "05_gedi_predicted_maps", "run03")

# NLCD 
dir_nlcd <- file.path(dir_rstor, "data", "NLCD_Land_Cover", "NLCD_landcover_2019_release_all_files_20210604")
fp_nlcd_2016 <- file.path(dir_nlcd, "nlcd_2016_land_cover_l48_20210604", "nlcd_2016_land_cover_l48_20210604.img")
fp_nlcd_2019 <- file.path(dir_nlcd, "nlcd_2019_land_cover_l48_20210604", "nlcd_2019_land_cover_l48_20210604.img")

# LCMAP
dir_lcmap <- file.path(dir_rstor, "data", "LCMAP_v1.3", "LCPRI")


# Directories of gedi shots as a csv file
dir_shots_2019 <- file.path(dir_gedi, "01_gedi_2019")
dir_shots_2020 <- file.path(dir_gedi, "02_gedi_2020")

# RH values are in the 2A folders
fp_shots_2a_2019 <- file.path(dir_shots_2019,  "03_gedi_filtered_data_csv", "GEDI_2A_raw.csv")
fp_shots_2a_2020 <- file.path(dir_shots_2020,  "03_gedi_filtered_data_csv", "GEDI_2A_raw.csv")
# Cover, pai, and pavd values are in the 2B folders
fp_shots_2b_2019 <- file.path(dir_shots_2019,  "03_gedi_filtered_data_csv", "GEDI_2B_raw.csv")
fp_shots_2b_2020 <- file.path(dir_shots_2020,  "03_gedi_filtered_data_csv", "GEDI_2B_raw.csv")


# Directory to FUSION executable
dir_fusion <- file.path("C:", "FUSION")


# Terra Options
dir_temp <- file.path("D:", "r_temp") #Denali
#dir_temp <- file.path("E:", "r_temp") #Gates


# Local directories for running linux, in linux format!!
#dir_gedi_sim_linux <- "/mnt/j/gedi_sim/" #Gates
dir_gedi_sim_linux <- "/mnt/e/gedi_sim/" #Denali

#same as above, but in windows format
#dir_gedi_sim_windows <- r"(J:\gedi_sim)" #Gates
dir_gedi_sim_windows <- r"(E:\gedi_sim)" #Denali



# -----------------------------------------------------------------------------
# Directories - Outputs
# -----------------------------------------------------------------------------

dir_val <- file.path(dir_processing, "10_predicted_vs_simulated")
  dir_val_aois <- file.path(dir_val, "01_aoi") # directory for all the aois
    dir_val_aoi <- file.path(dir_val_aois, aoi) # outputs for this study area
      dir_model_pred_grids <- file.path(dir_val_aoi, "01_model_prediction_grids")
        dir_val_pred_at_gedi <- file.path(dir_model_pred_grids, "map_preds_at_gedi") # outputs for predictions at gedi shots
    dir_val_all_aoi <- file.path(dir_val_aois, "_all_aois") # summaries of all aois
	  dir_val_all_shots <- file.path(dir_val_all_aoi, "01_shot_level")
	    dir_val_all_shots_dat <- file.path(dir_val_all_shots, "01_data")
        dir_val_all_shots_fig <- file.path(dir_val_all_shots, "02_figures")
		dir_val_all_shots_tab <- file.path(dir_val_all_shots, "03_tables")
        dir_val_all_shots_final_tab <- file.path(dir_val_all_shots, "04_final_tables")
		dir_val_all_shots_final_fig <- file.path(dir_val_all_shots, "05_final_figures")
	  dir_val_all_pixels <- file.path(dir_val_all_aoi, "02_map_level")
        dir_val_all_pixels_dat <- file.path(dir_val_all_pixels, "01_data")
          fp_val_all_pixels_dat <- file.path(dir_val_all_pixels_dat, "all_pixel_data.db")
          fp_val_pixels_pre_gedi_dat <- file.path(dir_val_all_pixels_dat, "sample_pre_gedi_pixels.csv")
          fp_val_pixels_post_gedi_dat <- file.path(dir_val_all_pixels_dat, "sample_post_gedi_pixels.csv")
        dir_val_all_pixels_tab <- file.path(dir_val_all_pixels, "02_tables")
        dir_val_all_pixels_fig <- file.path(dir_val_all_pixels, "03_figures")
        dir_val_all_pixels_final_tab <- file.path(dir_val_all_pixels, "04_final_tables")
		dir_val_all_pixels_final_fig <- file.path(dir_val_all_pixels, "05_final_figures")
  
  dir_val_shots <- file.path(dir_val_aoi, "02_shot_level") # directory for ALS 
    dir_val_shots_intersecting_shots <- file.path(dir_val_shots, "01_intersecting_shots") # directory for ALS clips
      fp_shp_intersecting_shots <- file.path(dir_val_shots_intersecting_shots, "gedi_shots_point.shp")
      fp_tab_intersecting_shots_by_year <- file.path(dir_val_shots_intersecting_shots, "gedi_shots_point_by_lidar_year.csv")
      fp_tab_intersecting_shots_2b <- file.path(dir_val_shots_intersecting_shots, "intersecting_gedi_shots_2b.csv")
      fp_tab_intersecting_shots_2a <- file.path(dir_val_shots_intersecting_shots, "intersecting_gedi_shots_2a.csv")
    dir_val_shots_clips <- file.path(dir_val_shots, "02_clips_not_normalized") # directory for ALS clips
    dir_val_shots_dtm <- file.path(dir_val_shots, "03_clips_dtm") # directory for ALS DTMs
    dir_val_shots_clips_normalized <- file.path(dir_val_shots, "04_clips_normalized") # directory for height normalized ALS clips
    dir_val_shots_cloudmetrics <- file.path(dir_val_shots, "05_fusion_cloudmetrics") # directory for cloudmetrics
    dir_val_shots_gedi_sim_clips <- file.path(dir_val_shots, "06_clips_for_gedi_sim") # directory for lidar clips that are fed to gediSimulator
    dir_val_shots_gedi_sim_shot_scripts <- file.path(dir_val_shots, "07_gedi_sim_shot_bash_scripts") # directory for scripts to run gediSimulator on clips
    dir_val_shots_gedi_sim_shot_results <- file.path(dir_val_shots, "08_gedi_sim_shot_results") # directory for 
      dir_val_shots_gedi_sim_shot_l1b <- file.path(dir_val_shots_gedi_sim_shot_results, "01_gedi_shots_llb")
      dir_val_shots_gedi_sim_shot_l2 <- file.path(dir_val_shots_gedi_sim_shot_results, "02_gedi_shots_l2")
      dir_val_shots_gedi_sim_shot_summary <- file.path(dir_val_shots_gedi_sim_shot_results, "03_gedi_shots_summary")
    dir_val_shots_figures <- file.path(dir_val_shots, "09_gedi_shot_figures") # directory for the shot-level aoi figures
  dir_val_grids <- file.path(dir_val_aoi, "03_grid_level") # directory for ALS 
    dir_val_als_gedi_sim_grid_scripts <- file.path(dir_val_grids, "01_gedi_sim_grids_bash_scripts") # directory for scripts to run gediSimulator on entire als collection
    dir_val_als_gedi_sim_grid_results <- file.path(dir_val_grids, "02_gedi_sim_grids_results") # directory for grids created by gediSimulator
    dir_val_gedi_sim_minus_pred_grid <- file.path(dir_val_grids, "03_gedi_sim_minus_pred_grids") # directory for difference grids
    dir_val_sqlite <- file.path(dir_val_grids, "04_sqlite_db")
      fp_val_sqlite <- file.path(dir_val_sqlite, paste0(aoi, ".db"))
    dir_val_map_strata <- file.path(dir_val_grids, "05_map_strata")







