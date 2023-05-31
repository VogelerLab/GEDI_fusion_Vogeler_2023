rm(list=ls())

# Name: 022_histograms_and_figures_sim_gedi_by_aois.R
# Purpose: create figures, stats, and the like. Comparing simulated footprints to real footprints
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-09

cat("\n\nRunning 022_histograms_and_figures_sim_gedi_by_aois.R\n\n"); flush.console()

#LCMAP pixel Values
# 1 - Developed
# 2 - Cropland
# 3 - Grass/Shrub
# 4 - Tree Cover
# 5 - Water
# 6 - Wetland
# 7 - Ice/Snow
# 8 - Barren

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Load Libraries
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(ggplot2)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")

	
# Terra Options
if(!dir.exists(dir_temp)) dir.create(dir_temp)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Define Functions
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


plt_2d <- function(fp_x, fp_y, q_limit = 1.0){
	x_lab <- basename(fp_x)
	x_lab <- gsub(".tif", "", x_lab)
	y_lab <- basename(fp_y)
	y_lab <- gsub(".tif", "", y_lab)


	#Read in rasters
	rast_x <- rast(fp_x)
	rast_y <- rast(fp_y)
	
	#Get pixel values
	val_x <- values(rast_x, mat = FALSE)
	val_y <- values(rast_y, mat = FALSE)

	#Identify NA pixels
	mask_x <- is.na(val_x)
	mask_y <- is.na(val_y)
	mask_xy <- mask_x | mask_y 
	
	#mask out any NA values
	val_x <- val_x[!mask_xy]
	val_y <- val_y[!mask_xy]
	
	#remove the extremes
	low_limit_x <- quantile(val_x, probs = (1-q_limit))
	up_limit_x <- quantile(val_x, probs = q_limit) 
	low_limit_y <- quantile(val_y, probs = (1-q_limit))
	up_limit_y <- quantile(val_y, probs = q_limit) 

	mask_x <- (val_x < low_limit_x) | (val_x > up_limit_x)	
	mask_y <- (val_y < low_limit_y) | (val_y > up_limit_y)
	mask_xy <- mask_x | mask_y 
	
	#mask out any extreme values
	val_x <- val_x[!mask_xy]
	val_y <- val_y[!mask_xy]
	
	
	#plot(val_x, val_y, pch = 20, xlab = "diff 98Pct", ylab="slope")
	cor(val_x, val_y)

	# Default call (as object)
	p <- ggplot(data.frame(val_x, val_y), aes(val_x,val_y))
	p <- p + stat_bin2d()
	p <- p + xlab(x_lab) + ylab(y_lab)
	p <- p + scale_fill_gradient(trans = "log1p", breaks=c(10,100,1000,5000, 10000, 50000))
	#p
	return(p)
}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Read Data
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

buff <- 12.5


# -----------------------------------------------------------------------------
# summary of simulated metrics
# -----------------------------------------------------------------------------

#first get column types
one_row <- read.csv(
	file.path(
		dir_val_shots_gedi_sim_shot_summary, 
		paste0("summary_gedi_sim_shots_", aoi, "_buffer_", buff, ".csv")
	),
	nrows = 1
)
temp_names <- names(one_row)
chars <- c(
	"wave_ID",
	"filename", 
	"linkM",
	"linkCov"
)
colClass <- ifelse(is.element(temp_names, chars), "character", "numeric")

metrics_sim <- read.csv(
	file.path(
		dir_val_shots_gedi_sim_shot_summary, 
		paste0("summary_gedi_sim_shots_", aoi, "_buffer_", buff, ".csv")
	),
	stringsAsFactors = FALSE, 
	colClasses = colClass
)
rm(one_row)
rm(temp_names)
rm(chars)
rm(colClass)

#remove some unneed fields
# RH Gaussian
drop <- grepl("rhGauss_", names(metrics_sim))
metrics_sim <- metrics_sim[,!drop]

# RH Max
drop <- grepl("rhMax_", names(metrics_sim))
metrics_sim <- metrics_sim[,!drop]

#RH Infl ??
drop <- grepl("rhInfl_", names(metrics_sim))
metrics_sim <- metrics_sim[,!drop]

# LAIs,
#Yes, the PAVD is labelled as "gLAI0t10" or "tLAI0t10" etc.
drop <- grepl("hgLAI", names(metrics_sim))
metrics_sim <- metrics_sim[,!drop]

drop <- grepl("hiLAI", names(metrics_sim))
metrics_sim <- metrics_sim[,!drop]

drop <- grepl("hmLAI", names(metrics_sim))
metrics_sim <- metrics_sim[,!drop]


#rename cover
metrics_sim$sim_cover <- metrics_sim$cover
metrics_sim$cover <- NULL

#create PAVDs
#Yes, the PAVD is labelled as "gLAI0t10" or "tLAI0t10" etc. 
metrics_sim$pavd_z5_10m_g <- metrics_sim$gLAI5t10
metrics_sim$pavd_z5_10m_t <- metrics_sim$tLAI5t10

lai_metrics <- grep("gLAI", names(metrics_sim), value=T)
lai_thresh <- data.frame()
for(i in 1:length(lai_metrics)){
	rng <- strsplit(lai_metrics[i], "gLAI")[[1]][2]
	lai_thresh[i,"start"] <- as.numeric(strsplit(rng, "t")[[1]][1])
	lai_thresh[i,"stop"] <- as.numeric(strsplit(rng, "t")[[1]][2])
}
rm(i)
rm(rng)
lai_thresh$gLAI <- paste0("gLAI", lai_thresh$start, "t", lai_thresh$stop)
lai_thresh$tLAI <- paste0("tLAI", lai_thresh$start, "t", lai_thresh$stop)

gLAI_gt20 <- lai_thresh[lai_thresh$start >= 20,"gLAI"]
tLAI_gt20 <- lai_thresh[lai_thresh$start >= 20,"tLAI"]
gLAI_gt40 <- lai_thresh[lai_thresh$start >= 40,"gLAI"]
tLAI_gt40 <- lai_thresh[lai_thresh$start >= 40,"tLAI"]

metrics_sim$pavd_z_20_plus_g <- apply(metrics_sim[,is.element(names(metrics_sim), gLAI_gt20)], 1, sum)
metrics_sim$pavd_z_20_plus_t <- apply(metrics_sim[,is.element(names(metrics_sim), tLAI_gt20)], 1, sum)
metrics_sim$pavd_z_40_plus_g <- apply(metrics_sim[,is.element(names(metrics_sim), gLAI_gt40)], 1, sum)
metrics_sim$pavd_z_40_plus_t <- apply(metrics_sim[,is.element(names(metrics_sim), tLAI_gt40)], 1, sum)



# -----------------------------------------------------------------------------
# year of gedi shot
# -----------------------------------------------------------------------------

tab_gedi_year <- read.csv(fp_tab_intersecting_shots_by_year, as.is = TRUE, colClasses = c("character", "integer"))

# -----------------------------------------------------------------------------
# true gedi shots - 2A
# -----------------------------------------------------------------------------

df_intersecting_shots_2a <- read.csv(fp_tab_intersecting_shots_2a, colClass = "character")

# Convert data type
col_to_num <- setdiff(
	names(df_intersecting_shots_2a), 
	c("shot_no", "beam", "als_name")
)

for(i in col_to_num){
	df_intersecting_shots_2a[,i] <- as.numeric(df_intersecting_shots_2a[,i])
}

rm(col_to_num)

# -----------------------------------------------------------------------------
# year of gedi shot - 2B
# -----------------------------------------------------------------------------

df_intersecting_shots_2b <- read.csv(fp_tab_intersecting_shots_2b, colClass = "character")

# Convert data type
col_to_num <- setdiff(
	names(df_intersecting_shots_2b), 
	c("shot_number", "beam", "delta_time")
)


for(i in col_to_num){
	df_intersecting_shots_2b[,i] <- as.numeric(df_intersecting_shots_2b[,i])
}

rm(col_to_num)

pavdz_metrics <- grep("pavd_z", names(df_intersecting_shots_2b), value=T)
pavdz_thresh <- data.frame()
for(i in 1:length(pavdz_metrics)){
	rng <- strsplit(pavdz_metrics[i], "pavd_z")[[1]][2]
	pavdz_thresh[i,"start"] <- as.numeric(strsplit(rng, "_")[[1]][1])
	temp <- strsplit(rng, "_")[[1]][2]
	temp <- sub("m", "", temp)
	pavdz_thresh[i,"stop"] <- as.numeric(temp)
}
rm(i)
rm(rng)
rm(temp)

pavdz_thresh$pavdz <- paste0("pavd_z", pavdz_thresh$start, "_", pavdz_thresh$stop, "m")


pavdz_gt20 <- pavdz_thresh[pavdz_thresh$start >= 20,"pavdz"]
pavdz_gt40 <- pavdz_thresh[pavdz_thresh$start >= 40,"pavdz"]

df_intersecting_shots_2b$pavd_z_20_plus <- apply(df_intersecting_shots_2b[,is.element(names(df_intersecting_shots_2b), pavdz_gt20)], 1, sum)
df_intersecting_shots_2b$pavd_z_40_plus <- apply(df_intersecting_shots_2b[,is.element(names(df_intersecting_shots_2b), pavdz_gt40)], 1, sum)

# -----------------------------------------------------------------------------
# polygon of intersecting shots
# -----------------------------------------------------------------------------

poly_intersecting_shots <- read_sf(fp_shp_intersecting_shots, as_tibble = FALSE)


# -----------------------------------------------------------------------------
# Determine LCMAP class
# -----------------------------------------------------------------------------

#LCMAP rasters
ras_lcmap <- rast(file.path(dir_lcmap, paste0("LCMAP_CU_", lidar_year, "_V13_LCPRI.tif")))
names(ras_lcmap) <- "LCMAP_CU_V13_LCPRI" #rename LCMAP layer without year


#transform crs
poly_intersecting_shots <- st_transform(x = poly_intersecting_shots, crs = st_crs(ras_lcmap))


df_lcmap <- data.frame(shot_no = poly_intersecting_shots$shot_no)
df_lcmap <- cbind(df_lcmap, extract(y = vect(poly_intersecting_shots), x = ras_lcmap)[2])

head(df_lcmap)

# -----------------------------------------------------------------------------
# fusion als metrics
# -----------------------------------------------------------------------------

fp_shots_cloudmetrics <- file.path(dir_val_shots_cloudmetrics, paste0("buffer_", buff), "plot_level_clips_AllRtns.csv")
df_shots_cloudmetrics <- read.csv(fp_shots_cloudmetrics, as.is = TRUE, colClass = "character")


for(i in 3:ncol(df_shots_cloudmetrics)){
	df_shots_cloudmetrics[,i] <- as.numeric(df_shots_cloudmetrics[,i])
}


head(df_shots_cloudmetrics)


# -----------------------------------------------------------------------------
# als derived topometrics
# -----------------------------------------------------------------------------

dir_topo_ras <- file.path(dir_model_pred_grids, paste0("pred_year_", lidar_year), "01_aoi_clips")

#topo rasters
ras_elev <- rast(file.path(dir_topo_ras, "als_elevation.tif"))
ras_slope <- rast(file.path(dir_topo_ras, "als_slope.tif"))


#transform crs
poly_intersecting_shots <- st_transform(x = poly_intersecting_shots, crs = st_crs(ras_elev))


df_topo <- data.frame(shot_no = poly_intersecting_shots$shot_no)
df_topo <- cbind(df_topo, extract(y = vect(poly_intersecting_shots), x = ras_elev)[2])
df_topo <- cbind(df_topo, extract(y = vect(poly_intersecting_shots), x = ras_slope)[2])

#rename
names(df_topo)
names(df_topo)[2] <- "als_elev"
names(df_topo)[3] <- "als_slope"

head(df_topo)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

#Merge dataframes, drop duplicate fields
drop <- c(
	"beam",
	"rh100",
	"coords.x1",
	"coords.x2"
)
keep <- setdiff(names(df_intersecting_shots_2b), drop)

dim(df_intersecting_shots_2a)
dim(df_intersecting_shots_2b)
dat <- merge(
	x = df_intersecting_shots_2a, 
	y = df_intersecting_shots_2b[,keep], 
	by.x = "shot_no", 
	by.y = "shot_number", 
	all.y = FALSE, 
	all.x = FALSE
)
dim(dat)
rm(drop)
rm(keep)

#add year of gedi shot
dim(dat)
dim(tab_gedi_year)
dat <- merge(x = dat, y = tab_gedi_year, by = "shot_no", all = FALSE)
dim(dat)

#bring in simulated gedi metrics
drop <- c(
	"lon",
	"lat"
)
keep <- setdiff(names(metrics_sim), drop)

dim(dat)
dim(metrics_sim)
dat <- merge(
	x = dat, 
	y = metrics_sim[,keep], 
	by.x = "shot_no", 
	by.y = "wave_ID", 
	all.x = FALSE, 
	all.y = FALSE
)
dim(dat)
rm(drop)
rm(keep)


#bring in lcmap class
dim(dat)
dim(df_lcmap)
dat <- merge(
	x = dat, 
	y = df_lcmap, 
	by = "shot_no",
	all.x = FALSE,
	all.y = FALSE
)
dim(dat)


#bring in fusion cloud metrics
dim(dat)
dim(df_shots_cloudmetrics)
dat <- merge(
	x = dat, 
	y = df_shots_cloudmetrics, 
	by.x = "shot_no",
	by.y = "FileTitle",
	all.x = FALSE,
	all.y = FALSE
)
dim(dat)


#bring in topo
dim(dat)
dim(df_topo)
dat <- merge(
	x = dat, 
	y = df_topo, 
	by = "shot_no",
	all.x = FALSE,
	all.y = FALSE
)
dim(dat)

#add color field for plotting
dat$lcmap_col <- NA
dat$lcmap_col <- ifelse(dat$LCMAP_CU_V13_LCPRI == 3, "darkkhaki", dat$lcmap_col)
dat$lcmap_col <- ifelse(dat$LCMAP_CU_V13_LCPRI == 4, "chartreuse4", dat$lcmap_col)
dat$lcmap_col <- ifelse(is.na(dat$lcmap_col) , "red", dat$lcmap_col)
table(dat$lcmap_col)

# -----------------------------------------------------------------------------
# Histograms of differences
# -----------------------------------------------------------------------------

# cover
hist((dat$cover - dat$sim_cover), breaks=seq(-1,1,0.05))
plot(y=dat$cover, x=dat$sim_cover, pch = 20, col = dat$LCMAP_CU_V13_LCPRI, xlab="simulated", ylab="gedi")

legend(
	"topleft", legend=unique(dat$LCMAP_CU_V13_LCPRI), pch=20, 
	col = unique(dat$LCMAP_CU_V13_LCPRI), title= "lcmap class", inset = .02, 
	bg = "white"
)

summary(lm(dat$cover ~ dat$sim_cover))


plot((dat$cover - dat$sim_cover) ~ dat$als_slope, pch = 20, col = dat$LCMAP_CU_V13_LCPRI, xlab="slope")


# RH98
lim <- ceiling(max(abs(dat$rh98 - dat$rhReal_98)))
hist((dat$rh98 - dat$rhReal_98), breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
plot(y=dat$rh98, x=dat$rhReal_98, pch = 20, col = dat$LCMAP_CU_V13_LCPRI, xlab="simulated", ylab="gedi")
summary(lm(dat$rh98 ~ dat$rhReal_98))

plot((dat$rh98 - dat$rhReal_98) ~ dat$als_slope, pch = 20, col = dat$LCMAP_CU_V13_LCPRI, xlab="slope")


# RH75
lim <- ceiling(max(abs(dat$rh75 - dat$rhReal_75)))
hist(dat$rh75 - dat$rhReal_75, breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
plot(y=dat$rh75, x=dat$rhReal_75, pch = 20, col = dat$LCMAP_CU_V13_LCPRI, xlab="simulated", ylab="gedi")
abline(0,1)
summary(lm(dat$rh75 ~ dat$rhReal_75))

#slope
lim <- ceiling(max(abs(dat$als_slope - dat$gSlope)))
hist(dat$als_slope - dat$gSlope, breaks=seq(-lim,lim,1), xlim=c(-100,100))
rm(lim)
plot(y=dat$als_slope, x=dat$gSlope, pch = 20, col = dat$LCMAP_CU_V13_LCPRI, xlab="simulated", ylab="gedi")
summary(lm(dat$als_slope ~ dat$gSlope))

summary(lm(dat[dat$gSlope > 1,"als_slope"] ~ dat[dat$gSlope > 1,"gSlope"]))
colx = (as.integer(scale(dat$als_slope)*2+5))
plot(y=dat$rh98, x=dat$rhReal_98, pch = 20, col = (colx - min(colx)), xlab="simulated")



#read in als metrics
#find nlcd lcmap values	

#plot(dat$rh98 ~ dat$rhReal_98 : dat$als_slope, pch = 20, col = dat$LCMAP_CU_V13_LCPRI, xlab="simulated")
summary(lm(dat$rh98 ~ dat$rhReal_98 + dat$als_slope))

# -----------------------------------------------------------------------------
# Boxplots by LCMAP
# -----------------------------------------------------------------------------

#(gedi - gediSimulator)
	
boxplot((dat$cover - dat$sim_cover) ~ dat$LCMAP_CU_V13_LCPRI)

boxplot((dat$rh98 - dat$rhReal_98) ~ dat$LCMAP_CU_V13_LCPRI)

boxplot((dat$rh75 - dat$rhReal_75) ~ dat$LCMAP_CU_V13_LCPRI)

boxplot((dat$rh50 - dat$rhReal_50) ~ dat$LCMAP_CU_V13_LCPRI)

boxplot((dat$pavd_z5_10m - dat$pavd_z5_10m_t) ~ dat$LCMAP_CU_V13_LCPRI)
boxplot((dat$pavd_z5_10m - dat$pavd_z5_10m_g) ~ dat$LCMAP_CU_V13_LCPRI)

boxplot((dat$pavd_z_20_plus - dat$pavd_z_20_plus_t) ~ dat$LCMAP_CU_V13_LCPRI)
boxplot((dat$pavd_z_20_plus - dat$pavd_z_20_plus_g) ~ dat$LCMAP_CU_V13_LCPRI)

boxplot((dat$pavd_z_40_plus - dat$pavd_z_40_plus_t) ~ dat$LCMAP_CU_V13_LCPRI)
boxplot((dat$pavd_z_40_plus - dat$pavd_z_40_plus_g) ~ dat$LCMAP_CU_V13_LCPRI)

plot(x = dat$pavd_z_40_plus_t, y = dat$pavd_z_40_plus_g, pch=20)
abline(0,1, col = "blue")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Save Figures
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
	
if(!dir.exists(dir_val_shots_figures)) dir.create(dir_val_shots_figures)

# -----------------------------------------------------------------------------
## Histograms
# -----------------------------------------------------------------------------
# Cover
lim <- ceiling(max(abs(dat$cover - dat$sim_cover)))
fp_fig <- file.path(dir_val_shots_figures, "hist_cover.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist((dat$cover - dat$sim_cover), breaks=seq(-lim,lim,0.05), xlim=c(-1,1))
rm(lim)
dev.off()

# RH98
lim <- ceiling(max(abs(dat$rh98 - dat$rhReal_98)))
fp_fig <- file.path(dir_val_shots_figures, "hist_rh98.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist((dat$rh98 - dat$rhReal_98), breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
dev.off()

# RH75
lim <- ceiling(max(abs(dat$rh75 - dat$rhReal_75)))
hist(dat$rh75 - dat$rhReal_75, breaks=seq(-lim,lim,1), xlim=c(-50,50))
fp_fig <- file.path(dir_val_shots_figures, "hist_rh75.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$rh75 - dat$rhReal_75, breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
dev.off()

# RH50
lim <- ceiling(max(abs(dat$rh50 - dat$rhReal_50)))
hist(dat$rh50 - dat$rhReal_50, breaks=seq(-lim,lim,1), xlim=c(-50,50))
fp_fig <- file.path(dir_val_shots_figures, "hist_rh50.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$rh50 - dat$rhReal_50, breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
dev.off()

# FHD
lim <- ceiling(max(abs(dat$fhd_normal - dat$FHD)))
hist(dat$fhd_normal - dat$FHD, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_shots_figures, "hist_fhd.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$fhd_normal - dat$FHD, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()

#PAVD - z5_10m
lim <- ceiling(max(abs(dat$pavd_z5_10m - dat$pavd_z5_10m_g)))
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_shots_figures, "hist_pavd_z5_10m_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()
lim <- ceiling(max(abs(dat$pavd_z5_10m - dat$pavd_z5_10m_t)))
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_shots_figures, "hist_pavd_z5_10m_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()

#PAVD - z_20_plus
lim <- ceiling(max(abs(dat$pavd_z_20_plus - dat$pavd_z_20_plus_g)))
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_shots_figures, "hist_pavd_z_20_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()
lim <- ceiling(max(abs(dat$pavd_z_20_plus - dat$pavd_z_20_plus_t)))
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_shots_figures, "hist_pavd_z_20_plus_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()

#PAVD - z_40_plus
lim <- ceiling(max(abs(dat$pavd_z_40_plus - dat$pavd_z_40_plus_g)))
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_shots_figures, "hist_pavd_z_40_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()
lim <- ceiling(max(abs(dat$pavd_z_40_plus - dat$pavd_z_40_plus_t)))
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_shots_figures, "hist_pavd_z_20_plus_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()



# -----------------------------------------------------------------------------
## scatterplots
# -----------------------------------------------------------------------------
# Cover
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_cover.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$cover, x=dat$sim_cover, pch = 20, col = dat$lcmap_col, main = "cover", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()

# RH98
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_rh98.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$rh98, x=dat$rhReal_98, pch = 20, col = dat$lcmap_col, main = "RH98", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()

# RH75
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_rh75.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$rh75, x=dat$rhReal_75, pch = 20, col =dat$lcmap_col, main = "RH75", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()

# RH50
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_rh50.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$rh50, x=dat$rhReal_50, pch = 20, col = dat$lcmap_col, main = "RH50", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()

# FHD
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_fhd.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$fhd_normal, x=dat$FHD, pch = 20, col = dat$lcmap_col, main = "FHD", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()



# PAVD - z5_10m
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_pavd_z5_10m_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z5_10m, x=dat$pavd_z5_10m_g, pch = 20, col = dat$lcmap_col, main = "PAVD - z5_10m (g)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_pavd_z5_10m_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z5_10m, x=dat$pavd_z5_10m_t, pch = 20, col = dat$lcmap_col, main = "PAVD - z5_10m (t)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()


#PAVD - z_20_plus
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_pavd_z_20_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z_20_plus, x=dat$pavd_z_20_plus_g, pch = 20, col = dat$lcmap_col, main = "PAVD > 20 m (g)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_pavd_z5_10m_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z_20_plus, x=dat$pavd_z_20_plus_t, pch = 20, col = dat$lcmap_col, main = "PAVD > 20 m (t)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()

#PAVD - z_40_plus
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_pavd_z_40_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z_40_plus, x=dat$pavd_z_40_plus_g, pch = 20, col = dat$lcmap_col, main = "PAVD > 40 m (g)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()
fp_fig <- file.path(dir_val_shots_figures, "scatter_plot_pavd_z5_10m_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z_40_plus, x=dat$pavd_z_40_plus_t, pch = 20, col = dat$lcmap_col, main = "PAVD > 40 m (t)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()

# -----------------------------------------------------------------------------
# Boxplots by LCMAP
# -----------------------------------------------------------------------------


fp_fig <- file.path(dir_val_shots_figures, "boxplot_cover.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$cover - dat$sim_cover) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_shots_figures, "boxplot_rh98.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$rh98 - dat$rhReal_98) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_shots_figures, "boxplot_rh75.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$rh75 - dat$rhReal_75) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_shots_figures, "boxplot_rh50.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$rh50 - dat$rhReal_50) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()

fp_fig <- file.path(dir_val_shots_figures, "boxplot_fhd.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$fhd_normal - dat$FHD) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_shots_figures, "boxplot_pavd_z5_10m_g.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z5_10m_g - dat$pavd_z5_10m) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()
fp_fig <- file.path(dir_val_shots_figures, "boxplot_pavd_z5_10m_t.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z5_10m_t - dat$pavd_z5_10m) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()

fp_fig <- file.path(dir_val_shots_figures, "boxplot_pavd_z_20_plus_g.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_20_plus_g - dat$pavd_z_20_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()
fp_fig <- file.path(dir_val_shots_figures, "boxplot_pavd_z_20_plus_t.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_20_plus_t - dat$pavd_z_20_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()

fp_fig <- file.path(dir_val_shots_figures, "boxplot_pavd_z_40_plus_g.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_40_plus_g - dat$pavd_z_40_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()
fp_fig <- file.path(dir_val_shots_figures, "boxplot_pavd_z_40_plus_t.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_40_plus_t - dat$pavd_z_40_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()




# -----------------------------------------------------------------------------
# Save dataframe
# -----------------------------------------------------------------------------

if(!dir.exists(dir_val_all_aoi)) dir.create(dir_val_all_aoi)
if(!dir.exists(dir_val_all_shots)) dir.create(dir_val_all_shots)
if(!dir.exists(dir_val_all_shots_dat)) dir.create(dir_val_all_shots_dat)

fp_dat <- file.path(dir_val_all_shots_dat, paste0("shot_level_data_", aoi, ".csv"))
write.csv(dat, fp_dat, row.names = FALSE)



