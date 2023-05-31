rm(list=ls())

# Name: 300_final_histograms_and_figures_sim_gedi_all_aois.R
# Purpose: create figures, stats, and the like. Comparing simulated footprints to real footprints
# Author: PA Fekety, Colorado State University, patrick.fekety@colostate.edu
# Date: 2023-02-09

cat("\n\nRunning 023_histograms_and_figures_sim_gedi_all_aois.R\n\n"); flush.console()

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
library(RColorBrewer)

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

dat <- data.frame()
for (f in dir(dir_val_all_shots_dat, pattern = "^shot_")) {
	f_temp <- read.csv(file.path(dir_val_all_shots_dat, f), as.is = TRUE)
	dat <- rbind(dat, f_temp)
	rm(f_temp)
}
rm(f)
dim(dat)
buff <- 12.5

#Get cover on the same scale
dat$als_cover_1st_rtn <- dat$Percentage.all.returns.above.2.00 / 100

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## Save Figures
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
#
#if(!dir.exists(dir_val_all_shots_final_fig)) dir.create(dir_val_all_shots_final_fig)
#
#
## Define colour pallete
#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
#sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(dat$Elev.P95)))
#
#
#
## -----------------------------------------------------------------------------
### scatterplots
## -----------------------------------------------------------------------------
## Cover
#fp_fig <- file.path(dir_val_all_shots_final_fig, "scatter_plot_cover.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#
#ggplot(dat, aes(x=sim_cover, y=cover, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc + 
#  labs(x="simulated cover", y = "GEDI cover", fill = "Top Height (m)")  +
#  labs(colour = "Top Height (m)") + theme(text=element_text(family = "serif"))
#
#dev.off()
#
## RH98
#fp_fig <- file.path(dir_val_all_shots_final_fig, "scatter_plot_rh98.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#
#ggplot(dat, aes(x=rhReal_98, y=rh98, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc + 
#  labs(x="simulated RH98", y = "GEDI RH98", fill = "Top Height (m)")  +
#  labs(color = "Top Height (m)") + 
#  theme(text=element_text(family = "serif"))
#
#dev.off()
#
## RH75
#fp_fig <- file.path(dir_val_all_shots_final_fig, "scatter_plot_rh75.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#
#ggplot(dat, aes(x=rhReal_75, y=rh75, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc + 
#  xlim(0,60) + ylim(0,60) +
#  labs(x="simulated RH75", y = "GEDI RH75", fill = "Top Height (m)")  +
#  labs(color = "Top Height (m)") + 
#  theme(text=element_text(family = "serif"))
#
#
#dev.off()
#
## RH50
#fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_rh50.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#
#ggplot(dat, aes(x=rhReal_50, y=rh50, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc
#
#dev.off()
#
## FHD
#fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_fhd.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#
#ggplot(dat, aes(x=FHD, y=fhd_normal, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc
#
#dev.off()
#
#
#
## PAVD - z5_10m
#fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z5_10m_t.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#
#ggplot(dat, aes(x=pavd_z5_10m_t, y=pavd_z5_10m, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc
#
#dev.off()
#
#
##PAVD - z_20_plus
#fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z5_10m_t.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#ggplot(dat, aes(x=pavd_z_20_plus_t, y=pavd_z_20_plus, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc
#
#dev.off()
#
##PAVD - z_40_plus
#
#fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z5_10m_t.png")
#png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
#ggplot(dat, aes(x=pavd_z_40_plus_t, y=pavd_z_40_plus, colour=Elev.P95)) + 
#  geom_point(size=1)+ sc
#
#dev.off()
#
## -----------------------------------------------------------------------------
## Boxplots by LCMAP
## -----------------------------------------------------------------------------
#
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_cover.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$cover - dat$sim_cover) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_rh98.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$rh98 - dat$rhReal_98) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_rh75.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$rh75 - dat$rhReal_75) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_rh50.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$rh50 - dat$rhReal_50) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_fhd.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$fhd_normal - dat$FHD) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z5_10m_g.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$pavd_z5_10m_g - dat$pavd_z5_10m) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z5_10m_t.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$pavd_z5_10m_t - dat$pavd_z5_10m) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_20_plus_g.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$pavd_z_20_plus_g - dat$pavd_z_20_plus) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_20_plus_t.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$pavd_z_20_plus_t - dat$pavd_z_20_plus) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_40_plus_g.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$pavd_z_40_plus_g - dat$pavd_z_40_plus) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_40_plus_t.png")
#png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
#boxplot((dat$pavd_z_40_plus_t - dat$pavd_z_40_plus) ~ dat$LCMAP_CU_V13_LCPRI)
#dev.off()
#
#
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Save Tables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

if(!dir.exists(dir_val_all_shots_final_tab)) dir.create(dir_val_all_shots_final_tab)

head(dat)


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

#cross walk to link model name to simulated name
cw <- matrix(c(
	"rh50", "rhReal_50",
	"rh75", "rhReal_75",
	"rh98", "rhReal_98",
#	"cover", "sim_cover",
	"cover", "als_cover_1st_rtn",
	"fhd_normal", "FHD",
	"pavd_z5_10m", "pavd_z5_10m_t",
	"pavd_z_20_plus", "pavd_z_20_plus_t",
	"pavd_z_40_plus", "pavd_z_40_plus_t"

	), byrow=TRUE, nrow = 8
) 
cw <- data.frame(cw)
names(cw) <- c("pred_name", "sim_name")


# -----------------------------------------------------------------------------
# Shot-level data
# -----------------------------------------------------------------------------

tab_shot_stats <- data.frame()
counter <- 1

for(met in metrics){
	pred <- vector()
	obs <- vector()
	for(i in 1:nrow(dat)){
		this_pred_name <- paste0(met)
		this_obs_name <- cw[cw$pred_name == met, "sim_name"]

		pred <- c(pred, dat[i,this_pred_name] )
		obs <- c(obs, dat[i,this_obs_name] )
	}

	#boxplot(pred-obs, ylab = "pred - obs", main = met)
	tab_shot_stats[counter, "attribute"] <- met
	tab_shot_stats[counter, "r2"] <- round(calc_r2(obs=obs, pred=pred), 3)
	tab_shot_stats[counter, "rmse"] <- round(calc_rmse(obs=obs, pred=pred), 3)
	tab_shot_stats[counter, "bias"] <- round(calc_bias(obs=obs, pred=pred), 3)

	rm(pred)
	rm(obs)

	counter <- counter + 1
}
rm(counter)

tab_shot_stats



fp_val_all_shots_stats_tab <- file.path(dir_val_all_shots_final_tab, "tab_all_shots_stats.csv")

write.csv(tab_shot_stats, fp_val_all_shots_stats_tab, row.names = FALSE)



