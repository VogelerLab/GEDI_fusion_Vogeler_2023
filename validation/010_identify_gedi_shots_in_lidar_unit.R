rm(list=ls())

# Name: 010_identify_gedi_shots_in_lidar_unit.R
# Purpose: find gedi shots that intersect a given lidar unit
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-02

cat("\n\nRunning 010_identify_gedi_shots_in_lidar_unit.R\n\n"); flush.console()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(sf)
library(lidR)
library(terra)
library(doParallel)

sf::sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Source parameter file
source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")

#Number of cores for parallel processing (this script is memory limited)
n_cores <- 4

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
# Read Data
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Read in lidar unit boundary shapefile	
fp_shp_aoi <- file.path(dir_als_boundary_5070, paste0(aoi, ".shp"))
poly_boundary <- read_sf(fp_shp_aoi, as_tibble = FALSE)
poly_boundary <- st_transform(poly_boundary, crs = 4326)

#add a column used when identifying shots in the polygon
poly_boundary$als_name <- aoi
poly_boundary$als_year <- lidar_year

# Read in gedi shots csv files for year of lidar
fp_shots_2a <- ifelse(lidar_year == 2019, fp_shots_2a_2019, fp_shots_2a_2020)
gedi_shots_2a <- read.csv(fp_shots_2a, colClasses = "character", stringsAsFactors = FALSE)

# Convert data type
for(i in 3:ncol(gedi_shots_2a)){
	gedi_shots_2a[,i] <- as.numeric(gedi_shots_2a[,i])
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Create sf object of gedi shots
# -----------------------------------------------------------------------------

poly_gedi_shots <- st_as_sf(
	gedi_shots_2a, 
	coords = c("lon_lowestmode","lat_lowestmode"), 
	remove = FALSE, 
	crs = 4326
)


rm(gedi_shots_2a)
gc()

# -----------------------------------------------------------------------------
# Intersect gedi shots with ALS boundary
# -----------------------------------------------------------------------------

sj <- st_join(x = poly_gedi_shots, y = poly_boundary, join = st_within)
intersecting_shots <- sj[!is.na(sj$als_name),]
nrow(intersecting_shots)

#convert shot number to char
intersecting_shots$shot_number <- as.character(intersecting_shots$shot_number)

#change field names
names(intersecting_shots)
names(intersecting_shots)[1] <- "shot_no"
names(intersecting_shots)[3] <- "degrd_flg"
names(intersecting_shots)[4] <- "qual_flg"
names(intersecting_shots)[5] <- "sens"
names(intersecting_shots)[6] <- "solar_elev"
names(intersecting_shots)[7] <- "lon"
names(intersecting_shots)[8] <- "lat"
names(intersecting_shots)

# Delete this field
intersecting_shots$FilePath <- NULL
intersecting_shots$FID__lidar <- NULL
intersecting_shots$name <- NULL
intersecting_shots$year <- NULL
intersecting_shots$STATEFP <- NULL
intersecting_shots$STUSPS <- NULL
intersecting_shots$NA_L3NAME <- NULL
intersecting_shots$NA_L2NAME <- NULL
intersecting_shots$NA_L1NAME <- NULL
intersecting_shots$area <- NULL

#polygon of shots that intersect the aoi
if(!dir.exists(dir_val)) dir.create(dir_val)
if(!dir.exists(dir_val_aois)) dir.create(dir_val_aois)
if(!dir.exists(dir_val_aoi)) dir.create(dir_val_aoi)
if(!dir.exists(dir_val_shots)) dir.create(dir_val_shots)
if(!dir.exists(dir_val_shots_intersecting_shots)) dir.create(dir_val_shots_intersecting_shots)

layer_name <- basename(fp_shp_intersecting_shots)
layer_name <- sub(".shp", "", layer_name)
if(!file.exists(fp_shp_intersecting_shots)){
	st_write(obj = intersecting_shots, dsn = fp_shp_intersecting_shots, layer = layer_name)
}

#write as csv file (2a)
fp_tab_intersecting_shots_2a

head(intersecting_shots)
df_intersecting_2a <- intersecting_shots
df_intersecting_2a$geometry <- NULL
head(df_intersecting_2a)
str(df_intersecting_2a)

write.csv(df_intersecting_2a, fp_tab_intersecting_shots_2a, row.names = FALSE)

#Table of shot number and year
if(length(intersecting_shots$shot_no) > 0){
	tab <- data.frame(shot_no = intersecting_shots$shot_no)
	tab$gedi_year <- lidar_year
}

write.csv(tab, fp_tab_intersecting_shots_by_year, row.names = FALSE)

# -----------------------------------------------------------------------------
# Buffer and Intersect gedi shots with ALS tile map
# -----------------------------------------------------------------------------

# create a las catalog
ctg <- readLAScatalog(dir_als_5070_aoi)
opt_progress(ctg) <- FALSE
cat("\n")


#project gedi shots into laz crs
intersecting_shots_trans <- st_transform(intersecting_shots, crs = crs(ctg))
intersecting_shots_trans[,c("X", "Y")] <-  st_coordinates(intersecting_shots_trans)


#Area of clip
area_dtm_clip <- pi * buffer_radius_dtm ** 2
# assume that if the area of the lidar clips is less than 90% of area_dtm_clip, then the 
#  clip is on a boundary
min_area_clip <- area_dtm_clip * 0.9

cat("Clipping ALS data to create DTMs at", buffer_radius_dtm, "\n")
flush.console()

cl <- makeCluster(n_cores)
registerDoParallel(cl)
x <- foreach(i = 1:nrow(intersecting_shots_trans), .packages=c("lidR"))%dopar%{


	dir_clips <- file.path(dir_val_shots_clips, paste0("buffer_", buffer_radius_dtm))
	fp_laz_out <- file.path(dir_clips, paste0(intersecting_shots_trans$shot_no[i], ".laz"))

	#only create if file doesn't exist
	if(!file.exists(fp_laz_out)) {

		# Clip LAZ
		las_clip <- clip_circle(
			las = ctg, 
			xcenter = intersecting_shots_trans$X[i],
			ycenter = intersecting_shots_trans$Y[i],
			radius = buffer_radius_dtm
		)

		# Check area of the clip
		if(area(las_clip) >= min_area_clip){ 
			#filter returns
			las_clip <- filter_poi(las_clip, is.element(Classification, c(0L, 1L, 2L, 3L, 4L, 5L)))

			#write LAZ file
			if(!dir.exists(dir_val_shots_clips)) dir.create(dir_val_shots_clips)
			if(!dir.exists(dir_clips)) dir.create(dir_clips)
		
			if(length(las_clip@data$NumberOfReturns) > 0){
				writeLAS(las_clip, file = fp_laz_out)
			}
		}
	}

}
stopCluster(cl)

# -----------------------------------------------------------------------------
# Clips at various radii
# -----------------------------------------------------------------------------
cat("Clipping ALS data at various radii", "\n")
flush.console()


dir_large_clips <- file.path(dir_val_shots_clips, paste0("buffer_", buffer_radius_dtm))
#cl <- makeCluster(1)
#registerDoParallel(cl)

#x <- foreach(i = 1:length(dir(dir_large_clips)), .packages=c("lidR", "terra")) %dopar% {
for(i in 1:length(dir(dir_large_clips))){
	clp <- dir(dir_large_clips)[i]

	#shot number
	shot_no <- sub(".laz", "", clp)

	for(buff in buffers){

		dir_clips <- file.path(dir_val_shots_clips, paste0("buffer_", buff))
		fp_laz_out <- file.path(dir_clips, paste0(shot_no, ".laz"))

		if(!file.exists(fp_laz_out)){

			#Read LAZ file
			laz_in <- file.path(dir_large_clips, clp)
			las <- readLAS(laz_in)

			# Clip LAZ
			shot_no <- sub(".laz", "", clp)
			X <- intersecting_shots_trans[intersecting_shots_trans$shot_no == shot_no, ]$X
			Y <- intersecting_shots_trans[intersecting_shots_trans$shot_no == shot_no, ]$Y

			las_clip <- clip_circle(
				las = las, 
				xcenter = X,
				ycenter = Y,
				radius = buff
			)
	
			if(length(las_clip@data$NumberOfReturns) > 0){
				if(!dir.exists(dir_clips)) dir.create(dir_clips)
				writeLAS(las_clip, file = fp_laz_out)
			}
		}
	}
}

#stopCluster(cl)



# -----------------------------------------------------------------------------
# Create DTMs
# -----------------------------------------------------------------------------
cat("Creating DTMs", "\n")
flush.console()


dir_clips <- file.path(dir_val_shots_clips, paste0("buffer_", buffer_radius_dtm))
cl <- makeCluster(n_cores)
registerDoParallel(cl)

x <- foreach(i = 1:length(dir(dir_clips)), .packages=c("lidR", "terra")) %dopar% {

	clp <- dir(dir_clips)[i]

	#shot number
	shot_no <- sub(".laz", "", clp)

	# Create DTM file
	dir_dtm <- file.path(dir_val_shots_dtm, paste0("buffer_", buffer_radius_dtm))
	fp_dtm <- file.path(dir_dtm, paste0("dtm_", shot_no, ".tif"))
		
	if(!file.exists(fp_dtm)){

		#Read LAZ file
		laz_in <- file.path(dir_clips, clp)
		las <- readLAS(laz_in)

		dtm <- rasterize_terrain(las, algorithm = tin())

		if(!dir.exists(dir_val_shots_dtm)) dir.create(dir_val_shots_dtm)
		if(!dir.exists(dir_dtm)) dir.create(dir_dtm)
		terra::writeRaster(x = dtm, filename = fp_dtm)

		rm(dtm)
	}
	rm(fp_dtm)
	rm(dir_dtm)
}

stopCluster(cl)


# -----------------------------------------------------------------------------
# Height normalize at various radii
# -----------------------------------------------------------------------------
cat("Height Normalizing at various radii", "\n")
flush.console()

#height normalizd
for(buff in buffers){

	dir_clips_in <- file.path(dir_val_shots_clips, paste0("buffer_", buffer_radius_dtm))
	dir_norm <- file.path(dir_val_shots_clips_normalized, paste0("buffer_", buff))

	cl <- makeCluster(n_cores)
	registerDoParallel(cl)	
	#for(f in dir(dir_clips_in)){
	foreach(i=1:length(dir(dir_clips_in)), .packages="lidR")%dopar%{

		f <- dir(dir_clips_in)[i]

		fp_laz_in <- file.path(dir_clips_in, f)
		fp_norm <- file.path(dir_norm, f)

		#shot number
		shot_no <- sub(".laz", "", f)

		if(!file.exists(fp_norm)){

			#Read LAZ file
			las <- readLAS(fp_laz_in)

			# height normalize large clip
			las_norm <- normalize_height(las, algorithm = tin(), use_class = c(2L, 9L))

			# Clip LAZ
			X <- intersecting_shots_trans[intersecting_shots_trans$shot_no == shot_no, ]$X
			Y <- intersecting_shots_trans[intersecting_shots_trans$shot_no == shot_no, ]$Y

			las_clip <- clip_circle(
				las = las_norm, 
				xcenter = X,
				ycenter = Y,
				radius = buff
			)

			if(!dir.exists(dir_val_shots_clips_normalized)) dir.create(dir_val_shots_clips_normalized)
			if(!dir.exists(dir_norm)) dir.create(dir_norm)
			if(length(las_norm@data$NumberOfReturns) > 0){
				writeLAS(las_norm, file = fp_norm)
			}
		} #end if !file.exists
	}
	stopCluster(cl)
}

