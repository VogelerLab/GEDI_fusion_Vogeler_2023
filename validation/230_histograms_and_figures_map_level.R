rm(list=ls())

# Name: 230_histograms_and_figures_map_level.R
# Purpose: create figures, stats, and the like. Comparing simulated pixels to model results
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
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


dat_pre$als_cover_1st_rtn <- dat_pre$als_cover_1st_rtn / 100
dat_post$als_cover_1st_rtn <- dat_post$als_cover_1st_rtn / 100

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

if(!dir.exists(dir_val_all_pixels_tab)) dir.create(dir_val_all_pixels_tab)

fp_val_all_pixels_tab_global_pre <- file.path(dir_val_all_pixels_tab, "tab_global_pre.csv")
fp_val_all_pixels_tab_global_post <- file.path(dir_val_all_pixels_tab, "tab_global_post.csv")
fp_val_all_pixels_tab_landsat_pre <- file.path(dir_val_all_pixels_tab, "tab_landsat_pre.csv")
fp_val_all_pixels_tab_landsat_post <- file.path(dir_val_all_pixels_tab, "tab_landsat_post.csv")

write.csv(tab_global_pre, fp_val_all_pixels_tab_global_pre, row.names = FALSE)
write.csv(tab_global_post, fp_val_all_pixels_tab_global_post, row.names = FALSE)
write.csv(tab_landsat_pre, fp_val_all_pixels_tab_landsat_pre, row.names = FALSE)
write.csv(tab_landsat_post, fp_val_all_pixels_tab_landsat_post, row.names = FALSE)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

if(!dir.exists(dir_val_all_pixels_fig)) dir.create(dir_val_all_pixels_fig)

fp_val_all_pixels_fig_global_pre <- file.path(dir_val_all_pixels_fig, "fig_scatter_global_pre.png")
fp_val_all_pixels_fig_global_post <- file.path(dir_val_all_pixels_fig, "fig_scatter_global_post.png")
fp_val_all_pixels_fig_landsat_pre <- file.path(dir_val_all_pixels_fig, "fig_scatter_landsat_pre.png")
fp_val_all_pixels_fig_landsat_post <- file.path(dir_val_all_pixels_fig, "fig_scatter_landsat_post.png")
fp_val_all_pixels_fig_global_hist_diff <- file.path(dir_val_all_pixels_fig, "fig_boxplot_global.png")
fp_val_all_pixels_fig_landsat_hist_diff <- file.path(dir_val_all_pixels_fig, "fig_boxplot_landsat.png")



# -----------------------------------------------------------------------------
# Global Model, Pre Gedi
# -----------------------------------------------------------------------------

model = model_id_g
dat = dat_pre
lidar_year <- dat$lidar_year


png(fp_val_all_pixels_fig_global_pre, width = 960, height = 960, pointsize = 24)
layout(matrix(1:9, 3, 3, byrow = TRUE))
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	plot(x = pred, y = obs, pch = 20, main = met)
	abline(0, 1, col = "blue", lwd = 2, lty = 2)

}
dev.off()


# -----------------------------------------------------------------------------
# Global Model, Post Gedi
# -----------------------------------------------------------------------------

model = model_id_g
dat = dat_post
lidar_year <- dat$lidar_year


png(fp_val_all_pixels_fig_global_post, width = 960, height = 960, pointsize = 24)
layout(matrix(1:9, 3, 3, byrow = TRUE))
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	plot(x = pred, y = obs, pch = 20, main = met)
	abline(0, 1, col = "blue", lwd = 2, lty = 2)

}
dev.off()


# -----------------------------------------------------------------------------
# Landsat Model, Pre Gedi
# -----------------------------------------------------------------------------

model = model_id_l
dat = dat_pre
lidar_year <- dat$lidar_year


png(fp_val_all_pixels_fig_landsat_pre, width = 960, height = 960, pointsize = 24)
layout(matrix(1:9, 3, 3, byrow = TRUE))
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	plot(x = pred, y = obs, pch = 20, main = met)
	abline(0, 1, col = "blue", lwd = 2, lty = 2)

}
dev.off()


# -----------------------------------------------------------------------------
# Landsat Model, During Gedi
# -----------------------------------------------------------------------------

model = model_id_l
dat = dat_post
lidar_year <- dat$lidar_year


png(fp_val_all_pixels_fig_landsat_post, width = 960, height = 960, pointsize = 24)
layout(matrix(1:9, 3, 3, byrow = TRUE))
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	plot(x = pred, y = obs, pch = 20, main = met)
	abline(0, 1, col = "blue", lwd = 2, lty = 2)

}
dev.off()


# -----------------------------------------------------------------------------
# Histograms - Global
# -----------------------------------------------------------------------------

model = model_id_g
dat = dat_pre
lidar_year <- dat$lidar_year

dat = dat_pre
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}
	diff <- pred - obs

	if(met == metrics[1]){
		dat_hist_pre <- data.frame(diff)
	} else {
		dat_hist_pre <- cbind(dat_hist_pre, diff)
	}
}

names(dat_hist_pre) <- metrics
dat_hist_pre$gedi <- "pre"


dat = dat_post
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}
	diff <- pred - obs

	if(met == metrics[1]){
		dat_hist_post <- data.frame(diff)
	} else {
		dat_hist_post <- cbind(dat_hist_post, diff)
	}
}

names(dat_hist_post) <- metrics
dat_hist_post$gedi <- "post"

dat_hist <- rbind(dat_hist_pre, dat_hist_post)

head(dat_hist)




boxplot(rh50 ~ gedi, data = dat_hist)


png(fp_val_all_pixels_fig_global_hist_diff, width = 960, height = 960, pointsize = 24)
layout(matrix(1:9, 3, 3, byrow = TRUE))
for(met in metrics){
	boxplot(dat_hist[,met] ~ dat_hist$gedi, main = met, ylab = "")

}
dev.off()


# -----------------------------------------------------------------------------
# Histograms - Landsat
# -----------------------------------------------------------------------------

model = model_id_l
dat = dat_pre
lidar_year <- dat$lidar_year

dat = dat_pre
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}
	diff <- pred - obs

	if(met == metrics[1]){
		dat_hist_pre <- data.frame(diff)
	} else {
		dat_hist_pre <- cbind(dat_hist_pre, diff)
	}
}

names(dat_hist_pre) <- metrics
dat_hist_pre$gedi <- "pre"


dat = dat_post
for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met, "_", model, "_", dat$lidar_year[i])
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}
	diff <- pred - obs

	if(met == metrics[1]){
		dat_hist_post <- data.frame(diff)
	} else {
		dat_hist_post <- cbind(dat_hist_post, diff)
	}
}

names(dat_hist_post) <- metrics
dat_hist_post$gedi <- "post"

dat_hist <- rbind(dat_hist_pre, dat_hist_post)



png(fp_val_all_pixels_fig_landsat_hist_diff, width = 960, height = 960, pointsize = 24)
layout(matrix(1:9, 3, 3, byrow = TRUE))
for(met in metrics){
	boxplot(dat_hist[,met] ~ dat_hist$gedi, main = met, ylab = "")

}
dev.off()


