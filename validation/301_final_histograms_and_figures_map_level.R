rm(list=ls())

# Name: 301_final_histograms_and_figures_map_level.R
# Purpose: create figures, stats, and the like. Comparing simulated pixels to model results
# Author: PA Fekety
# Date: 2023-03-14

cat("\n\nRunning 230_histograms_and_figures_map_level.R\n\n"); flush.console()

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

library(ggplot2)
library(RColorBrewer)
library(gridExtra )
library(grid)
library(cowplot)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Assign Variables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

source(r"(N:\RStor\jvogeler\Lab\users\pafekety\code\GEDI\validation\000_gedi_validation_parameters.R)")

#cross walk to link model name to simulated name
cw <- matrix(c(
	"rh50", "sim_rhReal_50",
	"rh75", "sim_rhReal_75",
	"rh98", "sim_rhReal_98",
#	"cover", "sim_cover",
	"cover", "als_cover_1st_rtn",
	"fhd_normal", "sim_FHD",
	"pavd_z5_10m", "sim_pavdz_5_10m",
	"pavd_z_20_plus", "sim_pavd_z_20_plus",
	"pavd_z_40_plus", "sim_pavd_z_40_plus"

	), byrow=TRUE, ncol = 2
) 
cw <- data.frame(cw)
names(cw) <- c("pred_name", "sim_name")

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

calc_rmse <- function(obs, pred){

	error <- pred - obs
	mse <- mean(error^2)
	rmse <- sqrt(mse)
	return(rmse)
}


calc_bias <- function(obs, pred){

	error <- pred - obs
	bias <- mean(error)
	return(bias)

}

calc_r2 <- function(obs, pred){

	cor_r2 <- cor(pred, obs)
	r2 <- cor_r2^2
	return(r2)

}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Read Data
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

dat_pre <- read.csv(fp_val_pixels_pre_gedi_dat, as.is = TRUE)
dat_post <- read.csv(fp_val_pixels_post_gedi_dat, as.is = FALSE)

#convert als cover to proportion
dat_pre$als_cover_1st_rtn <- dat_pre$als_cover_1st_rtn / 100
dat_post$als_cover_1st_rtn <- dat_post$als_cover_1st_rtn / 100

#remove pixels that were assign FHD = 0. These are pixels over water and were 
# introduced by the water masking step
summary(dat_pre$fhd_normal_l8_topo_bio_2020)
dat_pre <- dat_pre[dat_pre$fhd_normal_l8_topo_bio_2020 != 0, ]
dat_post <- dat_post[dat_post$fhd_normal_l8_topo_bio_2020 != 0, ]


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


metrics = c(
	"rh50",
	"rh75",
	"rh98",
	"cover",
	"fhd_normal",
	"pavd_z5_10m",
	"pavd_z_20_plus",
	"pavd_z_40_plus"
)


# -----------------------------------------------------------------------------
# Global Model, Pre Gedi
# -----------------------------------------------------------------------------

tab_global_pre <- data.frame()
counter <- 1
model = model_id_g
dat = dat_pre
lidar_year <- dat$lidar_year

for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat_pre)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	#boxplot(pred-obs, ylab = "pred - obs", main = met)
	tab_global_pre[counter, "attribute"] <- met
	tab_global_pre[counter, "r2"] <- round(calc_r2(obs=obs, pred=pred), 3)
	tab_global_pre[counter, "rmse"] <- round(calc_rmse(obs=obs, pred=pred), 3)
	tab_global_pre[counter, "bias"] <- round(calc_bias(obs=obs, pred=pred), 3)

	counter <- counter + 1
}
rm(counter)
rm(model)
rm(dat)
rm(lidar_year)

tab_global_pre


# -----------------------------------------------------------------------------
# Global Model, During Gedi
# -----------------------------------------------------------------------------

tab_global_post <- data.frame()
counter <- 1
model = model_id_g
dat = dat_post
lidar_year <- dat$lidar_year

for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat_pre)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	#boxplot(pred-obs, ylab = "pred - obs", main = met)
	tab_global_post[counter, "attribute"] <- met
	tab_global_post[counter, "r2"] <- round(calc_r2(obs=obs, pred=pred), 3)
	tab_global_post[counter, "rmse"] <- round(calc_rmse(obs=obs, pred=pred), 3)
	tab_global_post[counter, "bias"] <- round(calc_bias(obs=obs, pred=pred), 3)

	counter <- counter + 1
}
rm(counter)
rm(model)
rm(dat)
rm(lidar_year)


tab_global_post



# -----------------------------------------------------------------------------
# Landsat Model, Pre Gedi
# -----------------------------------------------------------------------------

tab_landsat_pre <- data.frame()
counter <- 1
model = model_id_l
dat = dat_pre
lidar_year <- dat$lidar_year

for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat_pre)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	#boxplot(pred-obs, ylab = "pred - obs", main = met)
	tab_landsat_pre[counter, "attribute"] <- met
	tab_landsat_pre[counter, "r2"] <- round(calc_r2(obs=obs, pred=pred), 3)
	tab_landsat_pre[counter, "rmse"] <- round(calc_rmse(obs=obs, pred=pred), 3)
	tab_landsat_pre[counter, "bias"] <- round(calc_bias(obs=obs, pred=pred), 3)

	counter <- counter + 1
}
rm(counter)
rm(model)
rm(dat)
rm(lidar_year)


tab_landsat_pre


# -----------------------------------------------------------------------------
# Landsat Model, During Gedi
# -----------------------------------------------------------------------------

tab_landsat_post <- data.frame()
counter <- 1
model = model_id_l
dat = dat_post
lidar_year <- dat$lidar_year

for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat_pre)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	#boxplot(pred-obs, ylab = "pred - obs", main = met)
	tab_landsat_post[counter, "attribute"] <- met
	tab_landsat_post[counter, "r2"] <- round(calc_r2(obs=obs, pred=pred), 3)
	tab_landsat_post[counter, "rmse"] <- round(calc_rmse(obs=obs, pred=pred), 3)
	tab_landsat_post[counter, "bias"] <- round(calc_bias(obs=obs, pred=pred), 3)

	counter <- counter + 1
}
rm(counter)
rm(model)
rm(dat)
rm(lidar_year)


tab_landsat_post



# -----------------------------------------------------------------------------
# save tables
# -----------------------------------------------------------------------------

if(!dir.exists(dir_val_all_pixels_final_tab)) dir.create(dir_val_all_pixels_final_tab)

fp_val_all_pixels_tab_global_pre <- file.path(dir_val_all_pixels_final_tab, "tab_map_global_pre.csv")
fp_val_all_pixels_tab_global_post <- file.path(dir_val_all_pixels_final_tab, "tab_map_global_post.csv")
fp_val_all_pixels_tab_landsat_pre <- file.path(dir_val_all_pixels_final_tab, "tab_map_landsat_pre.csv")
fp_val_all_pixels_tab_landsat_post <- file.path(dir_val_all_pixels_final_tab, "tab_map_landsat_post.csv")

write.csv(tab_global_pre, fp_val_all_pixels_tab_global_pre, row.names = FALSE)
write.csv(tab_global_post, fp_val_all_pixels_tab_global_post, row.names = FALSE)
write.csv(tab_landsat_pre, fp_val_all_pixels_tab_landsat_pre, row.names = FALSE)
write.csv(tab_landsat_post, fp_val_all_pixels_tab_landsat_post, row.names = FALSE)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Multipanels
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# -------------------------------------------------------------------
# REVISIONS
# -------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Landsat Model, Post Gedi - multipanel
# -----------------------------------------------------------------------------

model = model_id_g
dat = dat_post
lidar_year <- dat$lidar_year

met = "rh50"

	pred <- vector()
	obs <- vector()
	ht <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
		ht <- c(ht, dat[i,"als_P95_1st_rtn"] )
	}

p_rh50 <- ggplot(data.frame(pred, obs, ht), aes(x=pred, y=obs, colour=ht)) + 
	  geom_point(size=1)+ sc + 
	  xlim(0,50) + ylim(0,50) +
	  labs(x="GEDI fusion (m)", y = "") +
	  labs(color = "Canopy \nHeight (m)") + 
	  theme(text=element_text(family = "serif")) +
	  ggtitle("RH50") + 
	  theme(plot.title = element_text(hjust = 0.5, vjust = -5, size = 30, face = "bold")) + 
	  theme(axis.text = element_text(size = 24, colour = "black")) + 
	  theme(axis.title=element_text(size=26)) +
	  theme(legend.position="none")


met = "rh75"

	pred <- vector()
	obs <- vector()
	ht <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
		ht <- c(ht, dat[i,"als_P95_1st_rtn"] )
	}

p_rh75 <- ggplot(data.frame(pred, obs, ht), aes(x=pred, y=obs, colour=ht)) + 
	  geom_point(size=1)+ sc + 
	  xlim(0,50) + ylim(0,50) +
	  labs(x="GEDI fusion (m)", y = "") +
	  labs(color = "Canopy \nHeight (m)") + 
	  theme(text=element_text(family = "serif")) +
	  ggtitle("RH75") + 
	  theme(plot.title = element_text(hjust = 0.5, vjust = -5, size = 30, face = "bold")) + 
	  theme(axis.text = element_text(size = 24, colour = "black")) + 
	  theme(axis.title=element_text(size=26)) +

	  theme(legend.position="none")

	

met = "rh98"

	pred <- vector()
	obs <- vector()
	ht <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
		ht <- c(ht, dat[i,"als_P95_1st_rtn"] )
	}

p_rh98 <- ggplot(data.frame(pred, obs, ht), aes(x=pred, y=obs, colour=ht)) + 
	  geom_point(size=1)+ sc + 
	  xlim(0,50) + ylim(0,50) +
	  labs(x="GEDI fusion (m)", y = "ALS") +
	  labs(color = "Canopy \nHeight (m)") + 
	  theme(text=element_text(family = "serif")) +
	  ggtitle("RH98") + 
	  theme(plot.title = element_text(hjust = 0.5, vjust = -5, size = 30, face = "bold")) + 
	  theme(axis.text = element_text(size = 24, colour = "black")) + 
	  theme(axis.title=element_text(size=26)) +
	  theme(legend.position="none")

met = "cover"

	pred <- vector()
	obs <- vector()
	ht <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
		ht <- c(ht, dat[i,"als_P95_1st_rtn"] )
	}

p_cover <- ggplot(data.frame(pred, obs, ht), aes(x=pred, y=obs, colour=ht)) + 
	  geom_point(size=1)+ sc + 
	  ##xlim(0,1) + ylim(0,1) +
	  labs(x="GEDI fusion (proportional)", y = "")  +
	  labs(color = "Canopy \nHeight (m)") + 
	  theme(text=element_text(family = "serif")) +
	  ggtitle("COVER") + 
	  theme(plot.title = element_text(hjust = 0.5, vjust = -5, size = 30, face = "bold")) + 
	  theme(axis.text = element_text(size = 24, colour = "black")) + 
	  theme(axis.title=element_text(size=26)) +
	  theme(legend.position="none") +
	  scale_x_continuous(breaks = seq(0, 1, by = 0.2), limits=c(0,1))  +
	  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits=c(0,1))


met = "fhd_normal"

	pred <- vector()
	obs <- vector()
	ht <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
		ht <- c(ht, dat[i,"als_P95_1st_rtn"] )
	}


p_fhd <- ggplot(data.frame(pred, obs, ht), aes(x=pred, y=obs, colour=ht)) + 
	  geom_point(size=1)+ sc + 
	  xlim(1,4) + ylim(3,6) +
	  labs(x="GEDI fusion (unitless)", y = "ALS")  +
	  labs(color = "Canopy \nHeight (m)") + 
	  theme(text=element_text(family = "serif")) +
	  ggtitle("FHD") + 
	  theme(plot.title = element_text(hjust = 0.5, vjust = -5, size = 30, face = "bold")) + 
	  theme(axis.text = element_text(size = 24, colour = "black")) + 
	  theme(axis.title=element_text(size=26)) +
	  theme(legend.position="none")

p_l <- ggplot(data.frame(pred, obs, ht), aes(x=pred, y=obs, colour=ht)) + 
	  geom_point(size=1) + sc +
	  labs(color = "Canopy \nHeight (m)") + 
	  theme(text=element_text(family = "serif")) + 
	  theme(legend.key.size = unit(2, 'cm')) + 
	  theme(legend.title = element_text(size=26)) +
	  theme(legend.text = element_text(size=24))


l <- get_legend(p_l)


fp_fig <- file.path(dir_val_all_pixels_final_fig, paste0("fig_final_scatter_multi_", model, "_during_gedi_revision1.png"))
png(fp_fig, width = 960, height = 960)
grid.arrange(p_rh98, p_rh75, p_rh50, p_fhd, p_cover, l,  nrow = 2)
dev.off()

