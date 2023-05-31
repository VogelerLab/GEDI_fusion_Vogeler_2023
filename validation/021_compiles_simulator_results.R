rm(list=ls())

# Name: 021_compiles_simulator_results.R
# Purpose: Moves the results of the shot-level gedi simulations from the linux
#		workspace to RStor
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-08

cat("\n\nRunning 021_compiles_simulator_results\n\n"); flush.console()


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
dir_gedi_sim_windows_las <- file.path(dir_gedi_sim_windows_aoi, "las")
dir_gedi_sim_windows_scripts <- file.path(dir_gedi_sim_windows_aoi, "scripts")
dir_gedi_sim_windows_l1b <- file.path(dir_gedi_sim_windows_aoi, "l1b")
dir_gedi_sim_windows_l2 <- file.path(dir_gedi_sim_windows_aoi, "l2")



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

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Copy to RStor
# L1b
cat("Copy L1B - simulator_results\n"); flush.console()
l1b_dirs <- dir(dir_gedi_sim_windows_l1b)
for(d in l1b_dirs){
	l1b_files <- dir(file.path(dir_gedi_sim_windows_l1b, d))

	for(f in l1b_files){
		src <- file.path(file.path(dir_gedi_sim_windows_l1b, d, f))
		dest <- file.path(dir_val_shots_gedi_sim_shot_l1b, d, f)

		if(!dir.exists(dir_val_shots_gedi_sim_shot_results)) dir.create(dir_val_shots_gedi_sim_shot_results)
		if(!dir.exists(dir_val_shots_gedi_sim_shot_l1b)) dir.create(dir_val_shots_gedi_sim_shot_l1b)
		if(!dir.exists(file.path(dir_val_shots_gedi_sim_shot_l1b, d))) dir.create(file.path(dir_val_shots_gedi_sim_shot_l1b, d))
		file.copy(src, dest)
	}

}

#L2
cat("Copy L2 - simulator_results\n"); flush.console()
l2_dirs <- dir(dir_gedi_sim_windows_l2)
for(d in l2_dirs){
	l2_files <- dir(file.path(dir_gedi_sim_windows_l2, d))

	for(f in l2_files){
		src <- file.path(file.path(dir_gedi_sim_windows_l2, d, f))
		dest <- file.path(dir_val_shots_gedi_sim_shot_l2, d, f)

		if(!dir.exists(dir_val_shots_gedi_sim_shot_results)) dir.create(dir_val_shots_gedi_sim_shot_results)
		if(!dir.exists(dir_val_shots_gedi_sim_shot_l2)) dir.create(dir_val_shots_gedi_sim_shot_l2)
		if(!dir.exists(file.path(dir_val_shots_gedi_sim_shot_l2, d))) dir.create(file.path(dir_val_shots_gedi_sim_shot_l2, d))
		file.copy(src, dest)
	}

}


#Summarize L2 files
cat("Summarize L2 - simulator_results\n"); flush.console()
for(d in dir(dir_val_shots_gedi_sim_shot_l2)){
	df_ans <- data.frame()

	l2_files <- dir(file.path(dir_val_shots_gedi_sim_shot_l2, d), full.names = TRUE)

	for(f in l2_files){
		temp <- read.table(file = f, header = F, sep = " ", skip = 1, colClass = "character")
		df_ans <- rbind(df_ans, temp)
		rm(temp)
	}
	#assign column names
	temp_names_vector <-  readLines(con = f, n = 1)
	temp_names <- strsplit(temp_names_vector, ",")[[1]]
	#remove leading #
	temp_names[1] <- sub("#", "", temp_names[1])
	#remove leading spaces, and number
	for(i in 1:length(temp_names)){
		temp_names[i] <- sub(" ", "", temp_names[i])
	}
	for(i in 1:length(temp_names)){
		temp <- strsplit(temp_names[i], " ")[[1]]
		temp_names[i] <- paste(temp[2:length(temp)], collapse = "_")
	}
	#Update column names
	colnames(df_ans) <- temp_names

	#convert data type
	chars <- c(
		"wave_ID",
		"filename", 
		"linkM",
		"linkCov"
	)
	for(i in 1:ncol(df_ans)){
		if(!is.element(colnames(df_ans)[i], chars)) df_ans[,i] <- as.numeric(df_ans[,i])
	}

	#Write dataframe
	fp_out <- file.path(dir_val_shots_gedi_sim_shot_summary, paste0("summary_gedi_sim_shots_", aoi, "_", d, ".csv"))
	if(!dir.exists(dir_val_shots_gedi_sim_shot_summary)) dir.create(dir_val_shots_gedi_sim_shot_summary)
	write.csv(df_ans, fp_out, row.names = FALSE)
}



