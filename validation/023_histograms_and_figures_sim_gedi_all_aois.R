rm(list=ls())

# Name: 023_histograms_and_figures_sim_gedi_all_aois.R
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

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Processing
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



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
	
if(!dir.exists(dir_val_all_shots_fig)) dir.create(dir_val_all_shots_fig)

# -----------------------------------------------------------------------------
## Histograms
# -----------------------------------------------------------------------------
# Cover
lim <- ceiling(max(abs(dat$cover - dat$sim_cover)))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_cover.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist((dat$cover - dat$sim_cover), breaks=seq(-lim,lim,0.05), xlim=c(-1,1))
rm(lim)
dev.off()

# RH98
lim <- ceiling(max(abs(dat$rh98 - dat$rhReal_98)))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_rh98.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist((dat$rh98 - dat$rhReal_98), breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
dev.off()

# RH75
lim <- ceiling(max(abs(dat$rh75 - dat$rhReal_75)))
hist(dat$rh75 - dat$rhReal_75, breaks=seq(-lim,lim,1), xlim=c(-50,50))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_rh75.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$rh75 - dat$rhReal_75, breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
dev.off()

# RH50
lim <- ceiling(max(abs(dat$rh50 - dat$rhReal_50)))
hist(dat$rh50 - dat$rhReal_50, breaks=seq(-lim,lim,1), xlim=c(-50,50))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_rh50.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$rh50 - dat$rhReal_50, breaks=seq(-lim,lim,1), xlim=c(-50,50))
rm(lim)
dev.off()

# FHD
lim <- ceiling(max(abs(dat$fhd_normal - dat$FHD)))
hist(dat$fhd_normal - dat$FHD, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_fhd.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$fhd_normal - dat$FHD, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()

#PAVD - z5_10m
lim <- ceiling(max(abs(dat$pavd_z5_10m - dat$pavd_z5_10m_g)))
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_pavd_z5_10m_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()
lim <- ceiling(max(abs(dat$pavd_z5_10m - dat$pavd_z5_10m_t)))
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_pavd_z5_10m_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z5_10m - dat$pavd_z5_10m_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()

#PAVD - z_20_plus
lim <- ceiling(max(abs(dat$pavd_z_20_plus - dat$pavd_z_20_plus_g)))
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_pavd_z_20_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()
lim <- ceiling(max(abs(dat$pavd_z_20_plus - dat$pavd_z_20_plus_t)))
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_pavd_z_20_plus_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_20_plus - dat$pavd_z_20_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()

#PAVD - z_40_plus
lim <- ceiling(max(abs(dat$pavd_z_40_plus - dat$pavd_z_40_plus_g)))
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_pavd_z_40_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_g, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()
lim <- ceiling(max(abs(dat$pavd_z_40_plus - dat$pavd_z_40_plus_t)))
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
fp_fig <- file.path(dir_val_all_shots_fig, "hist_pavd_z_20_plus_t.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
hist(dat$pavd_z_40_plus - dat$pavd_z_40_plus_t, breaks=seq(-lim,lim,0.1), xlim=c(-5,5))
rm(lim)
dev.off()



# -----------------------------------------------------------------------------
## scatterplots
# -----------------------------------------------------------------------------
# Cover
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_cover.png")
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
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_rh98.png")
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
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_rh75.png")
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
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_rh50.png")
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
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_fhd.png")
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
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z5_10m_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z5_10m, x=dat$pavd_z5_10m_g, pch = 20, col = dat$lcmap_col, main = "PAVD - z5_10m (g)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z5_10m_t.png")
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
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z_20_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z_20_plus, x=dat$pavd_z_20_plus_g, pch = 20, col = dat$lcmap_col, main = "PAVD > 20 m (g)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z5_10m_t.png")
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
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z_40_plus_g.png")
png(filename = fp_fig,  width = 2*480, height = 2*480, pointsize = 12*2)
plot(y=dat$pavd_z_40_plus, x=dat$pavd_z_40_plus_g, pch = 20, col = dat$lcmap_col, main = "PAVD > 40 m (g)", xlab="simulated", ylab="gedi")
legend(
	x = "topleft", title= "lcmap class", 
	legend=c("3", "4", "other"), pch=20, 
	col = c("darkkhaki", "chartreuse", "red"), 
	bg = "white", inset = .02, 
)
dev.off()
fp_fig <- file.path(dir_val_all_shots_fig, "scatter_plot_pavd_z5_10m_t.png")
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


fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_cover.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$cover - dat$sim_cover) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_rh98.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$rh98 - dat$rhReal_98) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_rh75.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$rh75 - dat$rhReal_75) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_rh50.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$rh50 - dat$rhReal_50) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()

fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_fhd.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$fhd_normal - dat$FHD) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z5_10m_g.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z5_10m_g - dat$pavd_z5_10m) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()
fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z5_10m_t.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z5_10m_t - dat$pavd_z5_10m) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()

fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_20_plus_g.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_20_plus_g - dat$pavd_z_20_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()
fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_20_plus_t.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_20_plus_t - dat$pavd_z_20_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()

fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_40_plus_g.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_40_plus_g - dat$pavd_z_40_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()
fp_fig <- file.path(dir_val_all_shots_fig, "boxplot_pavd_z_40_plus_t.png")
png(filename = fp_fig,  width = 3*480, height = 2*480, pointsize = 12*2)
boxplot((dat$pavd_z_40_plus_t - dat$pavd_z_40_plus) ~ dat$LCMAP_CU_V13_LCPRI)
dev.off()


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Save Tables
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

if(!dir.exists(dir_val_all_shots_tab)) dir.create(dir_val_all_shots_tab)

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
	"cover", "sim_cover",
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
		this_pred_name <- cw[cw$pred_name == met, "sim_name"]
		this_obs_name <- paste0(met)

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



fp_val_all_shots_stats_tab <- file.path(dir_val_all_shots_tab, "tab_all_shots_stats.csv")

write.csv(tab_shot_stats, fp_val_all_shots_stats_tab, row.names = FALSE)

# -----------------------------------------------------------------------------
# Save dataframe
# -----------------------------------------------------------------------------


fp_dat <- file.path(dir_val_all_shots_dat, paste0("all_shot_level_data.csv"))
write.csv(dat, fp_dat, row.names = FALSE)

