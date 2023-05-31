rm(list=ls())

# Name: 225_select_sample_of_pixels.R
# Purpose: selects a sample of pixels for analysis
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-03-14


cat("\n\nRunning 225_select_sample_of_pixels.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(DBI)
library(RSQLite)

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

#DB for all pixels
fp_val_all_pixels_dat 

#establish connection to db
con_all <- dbConnect(SQLite(), fp_val_all_pixels_dat)

# Find the number of rows in db
#
n_rows_pre <- dbGetQuery(con_all,
	paste0("SELECT COUNT(lidar_unit) FROM pixel_values WHERE lidar_year IN (2016, 2017, 2018)")
)
n_rows_pre <- n_rows_pre[1,1]
cat("number of pixels before gedi:", n_rows_pre, "\n")

# during the gedi era
n_rows_post <- dbGetQuery(con_all,
	paste0("SELECT COUNT(lidar_unit) FROM pixel_values WHERE lidar_year IN (2019, 2020)")
)
n_rows_post <- n_rows_post[1,1]
cat("number of pixels during gedi:", n_rows_post, "\n")


# SRS Query
sql_query_srs_pre <- paste0(
	"SELECT * FROM pixel_values ",
	"WHERE lidar_year IN (2016, 2017, 2018);"
)
	
sql_query_srs_post <- paste0(
	"SELECT * FROM pixel_values ",
	"WHERE lidar_year IN (2019, 2020);"
)

# -----------------------------------------------------------------------------
#pre-gedi	
# -----------------------------------------------------------------------------
query_results <- dbGetQuery(con_all, sql_query_srs_pre)
query_results <- query_results[order(query_results$x, query_results$y, query_results$als_elevation),]

dat_pre <- data.frame()
for(lidar_unit in unique(query_results$lidar_unit)){
	set.seed(query_seed)
	dat_temp <- query_results[query_results$lidar_unit == lidar_unit,]
	dat_temp <- dat_temp[sample(1:nrow(dat_temp), sample_size),]
	dat_pre <- rbind(dat_pre, dat_temp)
	rm(dat_temp)
}


cat("table of lidar year for all pixels:\n")
print(table(query_results$lidar_year))
cat("\n")

cat("table of lidar year for all pixels:\n")
print(table(dat_pre$lidar_year))
cat("\n\n")
flush.console()

rm(query_results)

# -----------------------------------------------------------------------------
#Post- Gedi
# -----------------------------------------------------------------------------
query_results <- dbGetQuery(con_all, sql_query_srs_post)
query_results <- query_results[order(query_results$x, query_results$y, query_results$als_elevation),]

dat_post <- data.frame()
for(lidar_unit in unique(query_results$lidar_unit)){
	set.seed(query_seed)
	dat_temp <- query_results[query_results$lidar_unit == lidar_unit,]
	dat_temp <- dat_temp[sample(1:nrow(dat_temp), sample_size),]
	dat_post <- rbind(dat_post, dat_temp)
	rm(dat_temp)
}

cat("table of lidar year for all pixels:\n")
print(table(query_results$lidar_year))
cat("\n")

cat("table of lidar year for all pixels:\n")
print(table(dat_post$lidar_year))
cat("\n")
flush.console()


dbDisconnect(con_all)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Save data tables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

write.csv(dat_pre, fp_val_pixels_pre_gedi_dat, row.names = FALSE)
write.csv(dat_post, fp_val_pixels_post_gedi_dat, row.names = FALSE)

