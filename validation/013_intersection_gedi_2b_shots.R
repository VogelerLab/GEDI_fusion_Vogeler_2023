rm(list=ls())

# Name: 013_intersection_gedi_2b_shots.R
# Purpose: create figures, stats, and the like. Comparing simulated footprints to real footprints
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-09

cat("\n\nRunning 013_intersection_gedi_2b_shots.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(foreign)


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


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Read Data
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# true gedi shots - 2A
# -----------------------------------------------------------------------------

gedi_shots_2a <- read.dbf(sub(".shp", ".dbf", fp_shp_intersecting_shots))
gedi_shots_2a$shot_no <- as.character(gedi_shots_2a$shot_no)


# -----------------------------------------------------------------------------
# year of gedi shot - 2B
# -----------------------------------------------------------------------------

# Read in gedi shots csv files
fp_shots_2b <- ifelse(lidar_year == 2019, fp_shots_2b_2019, fp_shots_2b_2020)
gedi_shots_2b <- read.csv(fp_shots_2b, colClasses = "character", stringsAsFactors = FALSE)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Convert data type
col_to_num <- setdiff(1:ncol(gedi_shots_2b),c(1,2,9))

for(i in col_to_num){
	gedi_shots_2b[,i] <- as.numeric(gedi_shots_2b[,i])
}


intersecting_gedi_shots_2b <- gedi_shots_2b[is.element(gedi_shots_2b$shot_number, gedi_shots_2a$shot_no), ]
dim(intersecting_gedi_shots_2b)


write.csv(intersecting_gedi_shots_2b, fp_tab_intersecting_shots_2b, row.names = FALSE)
