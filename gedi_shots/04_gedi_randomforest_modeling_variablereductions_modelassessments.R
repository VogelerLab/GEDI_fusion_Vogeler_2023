###############################################################################
###                                                                         ###
### GEDI-fusion RandomForest Analysis                                       ###
###  Reconfigured by jody vogeler 8/24/22                                   ###
###      (Modified from some pieces of original code                        ###
###       from Neal Swayze and Patrick Fekety as well as adding new pieces) ###
### POC: jody.vogeler@colostate.edu                                         ###
###############################################################################

###_________________________###
### INSTALL / LOAD PACKAGES ###
###_________________________###

library(pacman)
p_load(data.table, raster, sf, sp, rgdal, maptools, tidyverse, ggplot2,
       randomForest, caret, e1071, rfUtilities, ranger, caTools, progress, foreach,
       doParallel, Boruta, cowplot, gridGraphics, viridis)

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Define functions
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#Purpose:
#	Identify variables that are highly correlated
#Inputs:
#	data - (data.frame or matrix) Data that will be checked for correlations
#	corValue - (num) Cutoff value for 'highly correlated'. Default is 0.9
#Output:
#	A data.frame of highly correlated variables and the correlation value

#this function calculates a correlation matrix and returns correlations >=0.9 and less than 1
correlationFunction = function(data, corValue=0.9, method="pearson"){
  temp = abs(cor(data, method=method))	#absolute value of the correlation matrix
  ans = NULL					#answer dataframe
  for (i in 1:ncol(temp)){
    for(j in 1:nrow(temp)){
      if(i>j) {
        if(temp[j,i] >= corValue & temp[j,i] != 1) ans=rbind(ans,c(rownames(temp)[j],colnames(temp)[i],round(temp[j,i],5)))
      }
    }
  }
  #rm(temp,i,j)
  if(length(ans)>0) {
    colnames(ans) = c("X-var1", "X-Var2", "Correlation")
    ans = as.data.frame(ans)
    ans = ans[order(ans["Correlation"], decreasing=T),]
    return(ans)
    
  } else {
    print("No Highly Correlated Variables")
  }
}

###___________________________###
### DEFINE DESIRED PARAMETERS ###
###___________________________###

### Set your input directory
rootDir <- ("E:/GEDI")

### Define the number of random samples to use for RF modeling training
num_samples = 60000

### Desired number of trees to use in RandomForest Models
number_trees = 500

### Set options to not use scientific notation
options(scipen = 100)

### Determine if RF should drop highly correlated variables or not
drop_correlated = FALSE

### Define desired GEDI response variables
desired_metrics = c("rh50", "rh75", "rh98", "fhd_normal", "cover", "pavd_z5_10m",
                   "pavd_z_20_plus", "pavd_z_40_plus")

# set model run to use in directory structure to capture new model runs
run = "run04"

#seed_rfutil <- sample(1:1000000,1)
rfseed = 195010

### Start the clock
total_start_time <- Sys.time()

###____________________________###
### AUTO CONFIGURE DIRECTORIES ###
###____________________________###

### Master Directory Structure Outline
######################################
dir.create(file.path(rootDir, "01_gedi_2019"))
dir.create(file.path(rootDir, "02_gedi_2020"))
dir.create(file.path(rootDir, "03_gedi_gee_tiles"))
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run))
dir.create(file.path(rootDir, "05_gedi_predicted_maps", run))

### 01_gedi_2019 Master Directory Structure Outline
###################################################
dir.create(file.path(rootDir, "01_gedi_2019/01_study_area_input"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files/01_GEDI_downloads_2A"))
dir.create(file.path(rootDir, "01_gedi_2019/02_download_files/01_GEDI_downloads_2B"))
dir.create(file.path(rootDir, "01_gedi_2019/03_gedi_filtered_data_csv"))
dir.create(file.path(rootDir, "01_gedi_2019/04_gedi_filtered_data_shp"))
dir.create(file.path(rootDir, "01_gedi_2019/05_gedi_filtered_data_random_samples"))
dir.create(file.path(rootDir, "01_gedi_2019/06_gedi_summary_plots"))
dir.create(file.path(rootDir, "01_gedi_2019/07_gedi_converted_las_files"))

### 02_gedi_2020 Master Directory Structure Outline
###################################################
dir.create(file.path(rootDir, "02_gedi_2020/01_study_area_input"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files/01_GEDI_downloads_2A"))
dir.create(file.path(rootDir, "02_gedi_2020/02_download_files/01_GEDI_downloads_2B"))
dir.create(file.path(rootDir, "02_gedi_2020/03_gedi_filtered_data_csv"))
dir.create(file.path(rootDir, "02_gedi_2020/04_gedi_filtered_data_shp"))
dir.create(file.path(rootDir, "02_gedi_2020/05_gedi_filtered_data_random_samples"))
dir.create(file.path(rootDir, "02_gedi_2020/06_gedi_summary_plots"))
dir.create(file.path(rootDir, "02_gedi_2020/07_gedi_converted_las_files"))

### 03_gedi_gee_tiles Master Directory Structure Outline
########################################################
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run, "01_gedi_2016_ortho_tiles"))
in_tile_2016 <- file.path(rootDir, "03_gedi_gee_tiles", run,"01_gedi_2016_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run,"02_gedi_2017_ortho_tiles"))
in_tile_2017 <- file.path(rootDir, "03_gedi_gee_tiles", run,"02_gedi_2017_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run,"03_gedi_2018_ortho_tiles"))
in_tile_2018 <- file.path(rootDir, "03_gedi_gee_tiles", run,"03_gedi_2018_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run,"04_gedi_2019_ortho_tiles"))
in_tile_2019 <- file.path(rootDir, "03_gedi_gee_tiles", run,"04_gedi_2019_ortho_tiles")
dir.create(file.path(rootDir, "03_gedi_gee_tiles", run,"05_gedi_2020_ortho_tiles"))
in_tile_2020 <- file.path(rootDir, "03_gedi_gee_tiles", run,"05_gedi_2020_ortho_tiles")

### 04_gedi_rf_modeling Master Directory Structure Outline
##########################################################
dir.create(file.path(rootDir, "04_gedi_rf_modeling", run))
rf_model_results_dir <- file.path(rootDir, "04_gedi_rf_modeling", run)
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"01_extracted_pixel_values"))
extracted_pixel_value_dir <- file.path(rootDir, "04_gedi_rf_modeling", run, "01_extracted_pixel_values")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"02_merged_pixel_values"))
merged_pixel_values_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"02_merged_pixel_values")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"03_input_model_variables"))
predictor_datasets_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"03_input_model_variables")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"04_rf_model_objects"))
rf_model_object_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"04_rf_model_objects")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"05_rf_model_importance_plots"))
rf_importance_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"05_rf_model_importance_plots")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"06_rf_model_results"))
rf_result_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"06_rf_model_results")
dir.create(file.path(rootDir, "04_gedi_rf_modeling",run,"07_rf_model_plot_comparisons"))
rf_plot_comp_dir <- file.path(rootDir, "04_gedi_rf_modeling",run,"07_rf_model_plot_comparisons")

###_______________________________________________________________###
### DATA PREP, ORGANIZATION AND MERGING OF 2019 and 2020 DATASETS ###
###_______________________________________________________________###

### Read in the desired dataframe for 2019
setwd(merged_pixel_values_dir)
data_2019 = fread("rf_ready_extracted_predictor_data_2019.csv") #Read in the merged 2019-2020 extracted pixel data
data_2019 = as.data.frame(data_2019)
str(data_2019)
summary(data_2019)

### Read in the desired dataframe for 2020
setwd(merged_pixel_values_dir)
data_2020 = fread("rf_ready_extracted_predictor_data_2020.csv")
data_2020 = as.data.frame(data_2020)
str(data_2020)
summary(data_2020)

### Define function to modify the input dataframes for RF modeling
prep_data_for_rf = function(input_dataframe, year_of_data){
  
  ### Add year of data to the dataframe for sampling
  year = rep(year_of_data, nrow(input_dataframe))
  data = cbind(input_dataframe, year)
  
  ### Return the merged dataframes
  return(data)
  
}

### Run the function for 2019
data_2019 = prep_data_for_rf(data_2019, year_of_data = "2019")
summary(data_2019)

### Run the function for 2020
data_2020 = prep_data_for_rf(data_2020, year_of_data = "2020")
summary(data_2020)

### Bind the 2019 and 2020 full dataframes together
data = rbind(data_2019, data_2020)
names(data)
head(data)
summary(data)
nrow(data)


###_______________________________________________________________________###
### RUN CORRELATION FUNCTIONS AND DECIDE METRICS TO RETAIN                ###
###_______________________________________________________________________###

l8_preds = c("blue", "green", "red", "nir", "swir1", "swir2", "ndvi", "nbr", "evi", "b5b4r", "tcb", "tcg", "tcw")
s1_preds = c("s1_vv_median", "s1_vh_median", "vh_vv_ratio", "norm_diff", "rvi")
topo_preds = c("elevation", "slope", "aspect", "eastness", "northness", "topo_diversity_srtm", "chili_srtm", "mTPI_srtm", "landform_srtm")
bio_preds = c("cmd", "dd5", "map", "mat", "mcmt")

#This is from JE's website: http://evansmurphy.wix.com/evansspatial#!random-forest-sdm/c1iuu
#To improve model fit we will test for multicolinearity using the 
#�multi.collinear� function in the rfUtilities library. If any variables are 
#identified as multicolinear we will remove them from the model.

#Re-order the xData such that those most correlated with a test response varaible are first. We will use rh98 for now. 
l8_preds <- l8_preds[order(abs(cor(x=data[l8_preds], y=data["rh98"], method="spearman")), decreasing=TRUE)]
cor(x=data[l8_preds], y=data["rh98"], method="spearman")

s1_preds <- s1_preds[order(abs(cor(x=data[s1_preds], y=data["rh98"], method="spearman")), decreasing=TRUE)]
cor(x=data[s1_preds], y=data["rh98"], method="spearman")

topo_preds <- topo_preds[order(abs(cor(x=data[topo_preds], y=data["rh98"], method="spearman")), decreasing=TRUE)]
cor(x=data[topo_preds], y=data["rh98"], method="spearman")

bio_preds <- bio_preds[order(abs(cor(x=data[bio_preds], y=data["rh98"], method="spearman")), decreasing=TRUE)]
cor(x=data[bio_preds], y=data["rh98"], method="spearman")

# ----------------------------------------------------------------------------
# Perform a cursury check for correlated variables
# ----------------------------------------------------------------------------
# Look for additional correlated variables for all predictor sets

# Landsat
correlationFunction(data[,l8_preds], corValue=0.95, method="spearman")
cor(x=data[l8_preds], y=data["rh98"], method="spearman")
# There were multiple correlated variables. Decided to drop all raw bands and a few indices resulting in tcb, tcg, tcw, ndvi, nbr.
# l8_preds2 = c("ndvi", "nbr", "tcb", "tcg", "tcw")

# S1
correlationFunction(data[,s1_preds], corValue=0.95, method="spearman")
cor(x=data[s1_preds], y=data["rh98"], method="spearman")
# rvi, norm_diff, and vh_vv_ratio were highly correlated. They also were similar in their correlations with rh98, but decided to keep rvi out them 

# Topo
correlationFunction(data[,topo_preds], corValue=0.95, method="spearman")
cor(x=data[topo_preds], y=data["rh98"], method="spearman")
# No highly correlated topo variables

# Bioclimatic
correlationFunction(data[,bio_preds], corValue=0.95, method="spearman")
cor(x=data[bio_preds], y=data["rh98"], method="spearman")
# No highly correlated bio variables



###________________________________________________________________________###
### SUBSET THE MAIN DATAFRAME INTO DESIRED PREDICTOR VARIABLE COMBINATIONS ###
###________________________________________________________________________###

### Assigning the variables into response and groups of predictors

# This now is driven by the correlation analyses complete above. 
#     Within predictor sets, for metrics that were highly correlated, we chose a single one to retain. 
#     We weighted indices more highly than raw values for Landsat, but weighted basic topo variables over derived metrics. 

response_variables = data %>% dplyr::select(any_of(desired_metrics)) 
s1_pred_variables = subset(data, select = c(s1_vv_median, s1_vh_median, rvi)) 
topo_pred_variables = subset(data, select = c(elevation, slope, aspect, eastness, northness, topo_diversity_srtm, chili_srtm, mTPI_srtm, landform_srtm))
l8_pred_variables = subset(data, select = c(ndvi, nbr, tcb, tcg, tcw))
dist_pred_variables = subset(data, select = c(time_since_disturbance))
bio_pred_variables = subset(data, select = c(cmd, dd5, map, mat, mcmt))
year = subset(data, select = c(year))

names(response_variables)
names(s1_pred_variables)
names(topo_pred_variables)
names(l8_pred_variables)
names(dist_pred_variables)
names(bio_pred_variables)
names(year)

### Creating combinations of predictor variables 
s1 =                  cbind(response_variables, s1_pred_variables, year)
l8 =                  cbind(response_variables, l8_pred_variables, year)
dist =                cbind(response_variables, dist_pred_variables, year)
topo_bio =            cbind(response_variables, topo_pred_variables, bio_pred_variables, year)
s1_topo_bio =         cbind(response_variables, s1_pred_variables, topo_pred_variables, bio_pred_variables, year)
l8_topo_bio =         cbind(response_variables, l8_pred_variables, topo_pred_variables, bio_pred_variables, year)
l8_topo_bio_dist =    cbind(response_variables, l8_pred_variables, topo_pred_variables, bio_pred_variables, dist_pred_variables, year)
l8_topo_bio_s1 =      cbind(response_variables, l8_pred_variables, topo_pred_variables, bio_pred_variables, s1_pred_variables, year)
l8_topo_bio_s1_dist = cbind(response_variables, l8_pred_variables, topo_pred_variables, bio_pred_variables, s1_pred_variables, dist_pred_variables, year)

### Writing out the desired predictor variables
setwd(predictor_datasets_dir)
fwrite(s1, file = "s1.csv", sep = ",")
fwrite(l8, file = "l8.csv", sep = ",")
fwrite(dist, file = "dist.csv", sep = ",")
fwrite(topo_bio, file = "topo_bio.csv", sep = ",")
fwrite(s1_topo_bio, file = "s1_topo_bio.csv", sep = ",")
fwrite(l8_topo_bio, file = "l8_topo_bio.csv", sep = ",")
fwrite(l8_topo_bio_dist, file = "l8_topo_bio_dist.csv", sep =",")
fwrite(l8_topo_bio_s1, file = "l8_topo_bio_s1.csv", sep = ",")
fwrite(l8_topo_bio_s1_dist, file = "l8_topo_bio_s1_dist.csv", sep = ",")

### Clean up after yourself
rm(response_variables,s1_pred_variables,topo_pred_variables,l8_pred_variables,bio_pred_variables, 
   dist_pred_variables, data_2019, data_2020, data, year, 
   s1, l8, dist, topo_bio, s1_topo_bio, l8_topo_bio, l8_topo_bio_dist, l8_topo_bio_s1, l8_topo_bio_s1_dist)

gc()

###_________________________________________________________###
### DEFINE FUNCTION TO RUN RANDOM FOREST MODELS IN PARALELL ###
###_________________________________________________________###

rf_modeler <- function(desired_metrics, num_samples, number_trees, drop_correlated,
                       predictor_datasets_dir, rf_model_object_dir, 
                       rf_importance_dir, rf_result_dir, rf_plot_comp_dir){
  
  ###_____________________________________________________###
  ### Get a list of desired response variables to analyze ###
  ###_____________________________________________________###
  
  list_of_desired_metrics = as.list(desired_metrics)
  list_of_desired_metrics
  
  ###___________________________________________________________________________________________________###
  ### Start the overall sequential foreach loop to process through the list of desired response metrics ###
  ###___________________________________________________________________________________________________###
  
  final_rf_results = foreach(j = 1:length(list_of_desired_metrics), .combine = rbind) %do% {
    
    ### Get the desired response metric
    desired_response = list_of_desired_metrics[[j]]
    desired_response
    
    ### Get a list of predictor combination CSVs to process
    setwd(predictor_datasets_dir)
    predictor_file_list <- list.files(setwd(predictor_datasets_dir), pattern = "csv$")
    predictor_file_list
    
    ###____________________________________________________________________________________________________________###
    ### Start the parallel foreach loop to run the RF models on the predictor combination foreach desired response ###
    ###____________________________________________________________________________________________________________###
    
    ### Register the number of cores for parallel processing
    num_cores = detectCores()
    num_cores = num_cores - 4
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    
    model_results <- foreach(i=1:length(predictor_file_list), .combine =rbind, .inorder = FALSE) %dopar% {
      
      ### Load the required libraries
      library(ggplot2)
      library(data.table)
      library(ggpointdensity)
      library(viridis)
      library(randomForest)
      library(caret)
      library(dplyr)
      
      ### Get the first file name from the index
      setwd(predictor_datasets_dir)
      file_name = predictor_file_list[[i]] 
      file_name
      
      ### Set the starting time for each model iteration
      start_time = Sys.time() 
      
      ### Get the name of the first predictor dataset in the index, drop the file extension
      file_name_out = tools::file_path_sans_ext(file_name)
      file_name_out
      
      ### Read in the data from the desired index
      data = fread(file_name) 
      length(data)
      data = as.data.frame(data)
      summary(data)
      
      ###___________________________________________###
      ### SUBSET THE DATA AND SELECT TOP PREDICTORS ###
      ###___________________________________________###
      
      ### Subset the data into response and predictors
      response = subset(data, select = c(desired_response))
      year = subset(data, select = c(year))
      predictor_length = length(desired_metrics) + 1
      predictors = data[,predictor_length:length(data)]
      predictors = as.data.frame(predictors)
      predictors = subset(predictors, select = -c(year))
      head(response)
      head(predictors)
      head(year)
      
      ###______________________________________________________________________###
      ### CONDITIONAL DROP CORRELATED VARIABLES GREATER THAN DESIRED THRESHOLD ###
      ###______________________________________________________________________###
      
      if(drop_correlated == TRUE) {
        
        ### Generate Correlation Matrix
        class(predictors)
        cor_matrix <- cor(predictors)                     
        cor_matrix
        
        # Modify correlation matrix
        cor_matrix_rm <- cor_matrix                  
        cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
        diag(cor_matrix_rm) <- 0
        cor_matrix_rm
        
        ### Drop correlated variables above desired threshold
        selected_predictors <- predictors[ , !apply(cor_matrix_rm, 2, function(x) any(x > 0.9))]
        head(selected_predictors) 
        
        ### Bind the selected predictor variables back to the response variable
        rf_select_variables <- cbind(response, selected_predictors, year)
        
      }
      
      if(drop_correlated == FALSE) {rf_select_variables <- cbind(response, predictors, year)}
      
      rf_select_variables
      
      ###_______________________________________________________________________###
      ### SAMPLE THE DESIRED NUMBER OF TRAINING POINTS FROM RF SELECT VARIABLES ###
      ###_______________________________________________________________________###
      
      set.seed(195010)
      training_2019 = filter(rf_select_variables, year == 2019)
      training_2019 = slice_sample(training_2019, n = (num_samples/2))
      training_2020 = filter(rf_select_variables, year == 2020)
      training_2020 = slice_sample(training_2020, n = (num_samples/2))
      training = rbind(training_2019, training_2020)
      testing = anti_join(rf_select_variables, training)
      
      ### Drop year from the training and testing, its not a predictor variable
      training = subset(training, select = -c(year))
      testing = subset(testing, select = -c(year))
      
      nrow(training)
      nrow(testing)
      head(training)
      head(testing)
      
      ### Run the random forest model - base random forest model
      set.seed(195010)
      final_rf_model <- randomForest(x=training[,2:ncol(training),F], y=training[,1], data = training, ntree = number_trees)
      final_rf_model
      
      ###__________________________________________________###
      ### RUN 10 FOLD CROSS VALIDATION RANDOM FOREST MODEL ###
      ###__________________________________________________###
      
      ### Save out the best RF parameter model to disk for later use
      setwd(rf_model_object_dir)
      model_name <- paste0(desired_response, "_", file_name_out, ".rds")
      saveRDS(final_rf_model, file = model_name)
      
      ###________________________________________________________________###
      ### GET RANDOM FOREST RESULTS, COMPARE TRAINING AND TESTING ERRORS ###
      ###________________________________________________________________##$
      
      ### Plot the variable importance
      setwd(rf_importance_dir)
      plot_file_name_out = paste0(desired_response, "_", file_name_out, "_", num_samples, "_samples", ".jpg")
      jpeg(plot_file_name_out)
      varImpPlot(final_rf_model)
      dev.off()
      
      ### Get the top 10 variables for the random forest model
      model_importance = final_rf_model$importance
      model_importance <- cbind((as.data.frame(rownames(model_importance))), model_importance)
      colnames(model_importance) <- c("variable", "incNodePurity") #Change the column names
      rownames(model_importance) <- NULL
      model_importance <- model_importance[order(-model_importance$incNodePurity),]
      model_importance
      
      ### Write out the model importance to the model importance folder
      setwd(rf_importance_dir)
      out_name = paste0(desired_response, "_", file_name_out, "_", num_samples, "_samples_model_importance", ".csv")
      write.csv(model_importance, file = out_name)
      
      ### Calculate training OBE model statistics - Ntree, mtry, RSQ, RMSE, MAE
      fin_model_ntree <- final_rf_model$ntree
      fin_model_mtry <- final_rf_model$mtry
      training_predicted <- final_rf_model$predicted
      training_observed <- training[,1]
      training_error = training_predicted - training_observed
      fit <- lm(training_predicted ~ training_observed)
      rsq_training = summary(fit)$adj.r.squared
      rmse_training = sqrt(mean(training_error^2))
      perrmse_training = (rmse_training/(mean(training_observed)))*100
      mae_training = mean(abs(training_error))
      bias_training = mean(training_error)
      perbias_training = (bias_training/(mean(training_observed)))*100
      mean_training_observed = mean(training_observed)
      
      ### Calculating testing model statistics - RSQ, RMSE, MAE
      testing_predicted <- predict(final_rf_model, testing) #Predict cover values for witheld validation data
      testing_observed <- testing[,1]
      testing_error <- testing_predicted  - testing_observed
      fit <- lm(testing_predicted ~ testing_observed)
      rsq_testing = summary(fit)$adj.r.squared
      rmse_testing = sqrt(mean(testing_error^2))
      perrmse_testing = (rmse_testing/(mean(testing_observed)))*100
      mae_testing = mean(abs(testing_error))
      bias_testing = mean(testing_error)
      perbias_testing = (bias_testing/(mean(testing_observed)))*100
      mean_testing_observed = mean(testing_observed)
      
      ### Calculate the difference between training and testing results
      dif_train_test_rsq = rsq_training - rsq_testing 
      dif_train_test_rmse = rmse_training - rmse_testing 
      dif_train_test_mae = mae_training - mae_testing 
      
      ### Calculate the time taken for processing
      end_time <- Sys.time()
      time_taken <- paste0(round(as.numeric(difftime(time1 = end_time, time2 = start_time, units = "hours")), 3))
      out_time <- paste0(time_taken)
      out_time
      
      ###___________________________________________________________###
      ### APPEND FINAL RF MODEL RESULTS, RETURN TO THE FOREACH LOOP ###
      ###___________________________________________________________###
      
      ### Appending model results to a row within a dataframe
      final_results <- cbind(desired_response, file_name_out, fin_model_ntree, fin_model_mtry, 
                             rsq_training, rsq_testing, dif_train_test_rsq,dif_train_test_rmse,dif_train_test_mae, 
                             rmse_training, rmse_testing, perrmse_training, mean_training_observed, perrmse_testing,  
                             mae_training, mae_testing, bias_training, bias_testing, 
                             perbias_training, perbias_testing, mean_testing_observed,
                             out_time)
      
      final_results = as.data.frame(final_results)
      
      ### Change final column names
      colnames(final_results) <- c("response_metric", "variable_combinations", "ntree" , "mtry", 
                                   "rsq_training", "rsq_testing", "train-test_rsq","train-test_rmse","train-test_mae",
                                   "rmse_training", "rmse_testing", "per_rmse_training", "per_rmse_testing", "mean_training_observed",
                                   "mae_training", "mae_testing", "bias_training", "bias_testing", "per_bias_training", "per_bias_testing",
                                   "mean_testing_observed","processing_time")
      
      final_results
      
      ### Return final rf data to foreach loop to rbind into master dataframe
      return(final_results)
    
  }
  
    ### Stop the processing cluster
    stopCluster(cl)
    
    model_results
    
    ### Sort the final results by highest R^2
    model_results <- model_results[order(model_results$rsq_training, decreasing = TRUE), ]
    model_results
  
    ### Return the model results to the sequential foreach loop
    return(model_results)
  
  }
  
  final_rf_results
  
  ### Return the master RF results
  return(final_rf_results)
  
}

###______________________________________________________________###
### AUTO RUN RANDOM FOREST MODELS FOR DESIRED RESPONSE VARIABLES ###
###______________________________________________________________###

final_rf_model_results = rf_modeler(desired_metrics, num_samples, number_trees, drop_correlated,
           predictor_datasets_dir, rf_model_object_dir, 
           rf_importance_dir, rf_result_dir, rf_plot_comp_dir)

final_rf_model_results

### Write out the random forest modeling results to the disk
final_rf_model_results
out_name = paste0("final_randomForest_model_results_", num_samples, ".csv")
setwd(rf_result_dir)
write.csv(final_rf_model_results, file = out_name)

###___________________________________________________________###
### GET THE TOP DESIRED MODEL COMBINATIONS FOR MAP VALIDATION ###
###___________________________________________________________###

global_models = filter(final_rf_model_results, variable_combinations == "l8_topo_bio_s1_dist")
global_models

sub_global = filter(final_rf_model_results, variable_combinations == "l8_topo_bio_s1")
sub_global

l8_topo_bio_model = filter(final_rf_model_results, variable_combinations == "l8_topo_bio")
l8_topo_bio_model

#top_models <- rbind(global_models, sub_global, l8_topo_model)
top_models = global_models
setwd(rf_result_dir)
out_name = paste0("top_rf_model_results_", num_samples, ".csv")
write.csv(top_models, file = out_name)

### Append .RDS to each model name to prep for map-level prediction
list <- setNames(split(top_models, seq(nrow(top_models))), rownames(top_models))
best_models = foreach(i = 1:length(list), .combine = rbind) %do% {
  
  model <- list[[i]]
  response_metric  <- subset(model, select = response_metric)
  variable_combinations <- subset(model, select = variable_combinations)
  model_name <- paste0(response_metric, "_", variable_combinations, ".rds")
  
  return(model_name)
  
}

### Prepare best model names to be written to disk
best_models = as.data.frame(best_models)
rownames(best_models) = NULL
setwd(rf_result_dir)
fwrite(best_models, file = "top_rf_model_object_names.csv", sep = ",")

### Write out user script processing time to rf_results directory
total_end_time = Sys.time()
user_time_taken = paste0(round(as.numeric(difftime(time1 = total_end_time, time2 = total_start_time, units = "hours")), 3))
user_time_taken = paste0(user_time_taken, " hours of processing time")
write.table(user_time_taken, "user_randomForest_processing_time.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')
user_time_taken

### Write out total machine processing time to 
final_rf_model_results$processing_time = as.numeric(final_rf_model_results$processing_time)
machine_processing_time = sum(final_rf_model_results$processing_time)
machine_processing_time = paste0(machine_processing_time, " hours of processing time")
write.table(machine_processing_time, "machine_randomForest_processing_time.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')

###__________________________________________________________________________________________________###
### DEFINE FUNCTION TO PLOT MODEL RESULTS FOR EACH DESIRED RESPONSE METRIC AND PREDICTOR COMBINATION ###
###__________________________________________________________________________________________________###

### Define function to plot model accuracy
plot_model_accuracy = function(desired_metrics, input_dataframe, rf_plot_comp_dir) {
  
  ### Get a list of desired response variables
  list_of_desired_metrics = as.list(desired_metrics)
  list_of_desired_metrics
  
  ### Register parallel session
  cores = detectCores()
  cores = cores/2
  cl = makeCluster(cores)
  registerDoParallel(cl)
  
  ### Foreach loop to plot each metric across predictor combinations
  foreach(i = 1:length(list_of_desired_metrics)) %dopar% {
    
    ### Load required packages
    library(ggplot2)
    library(viridis)
    library(dplyr)
    library(reshape2)
    library(cowplot)
    
    ### Get the desired response variable
    desired_response = list_of_desired_metrics[[i]]
    desired_response
    
    ### Filter the input dataframe to the desired input response
    desired_data = filter(input_dataframe, response_metric == desired_response)
    desired_data = subset(desired_data, select = c(variable_combinations, rsq_training, rsq_testing, rmse_training, rmse_testing, mae_training, mae_testing))
    
    desired_data$variable_combinations = as.factor(desired_data$variable_combinations)
    desired_data$rsq_training = as.numeric(desired_data$rsq_training)
    desired_data$rsq_testing = as.numeric(desired_data$rsq_testing)
    desired_data$rmse_training = as.numeric(desired_data$rmse_training)
    desired_data$rmse_testing = as.numeric(desired_data$rmse_testing)
    desired_data$mae_training = as.numeric(desired_data$mae_training)
    desired_data$mae_testing = as.numeric(desired_data$mae_testing)
    desired_data$bias_training = as.numeric(desired_data$bias_training)
    desired_data$bias_testing = as.numeric(desired_data$bias_testing)
    
    ##############################
    ### PREP DATA FOR PLOTTING ###
    ##############################
    
    rsq = subset(desired_data, select = c(variable_combinations, rsq_testing)) 
    rsq = reshape2::melt(rsq, id.vars = c("variable_combinations"))
    
    rmse = subset(desired_data, select = c(variable_combinations, rmse_testing)) 
    rmse = reshape2::melt(rmse, id.vars = c("variable_combinations"))
    
    mae = subset(desired_data, select = c(variable_combinations, mae_testing)) 
    mae = reshape2::melt(mae, id.vars = c("variable_combinations"))
    
    bias = subset(desired_data, select = c(variable_combinations, bias_testing)) 
    bias = reshape2::melt(bias, id.vars = c("variable_combinations"))
    
    #################
    ### BOX PLOTS ###
    #################
    
    plot_name = paste0("RF Model Comparison ~ ", desired_response)
    plot_name
    
    rsq_box_plot = ggplot(rsq, aes(x=reorder(variable_combinations, -value), y = value, color = variable_combinations)) +
      geom_boxplot() + 
      ylab("R^2") + 
      xlab("") +
      scale_y_continuous(limits = c(0, 1)) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
    
    rmse_box_plot = ggplot(rmse, aes(x=reorder(variable_combinations, +value), y = value, color = variable_combinations)) +
      geom_boxplot() + 
      ylab("Root Mean Square Error") + 
      xlab("") +
      scale_y_continuous(limits = c(min(rmse$value), max(rmse$value))) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
    
    mae_box_plot = ggplot(mae, aes(x=reorder(variable_combinations, +value), y = value, color = variable_combinations)) +
      geom_boxplot() + 
      ylab("Mean Absolute Error") + 
      xlab("") +
      scale_y_continuous(limits = c(min(mae$value), max(mae$value)))
    
    bias_box_plot = ggplot(bias, aes(x=reorder(variable_combinations, +value), y = value, color = variable_combinations)) +
      geom_boxplot() + 
      ylab("Bias") + 
      xlab("") +
      scale_y_continuous(limits = c(min(bias$value), max(bias$value)))
    
    ### Grab legend from the rsq plot, drop the legends
    legend = get_legend(rsq_box_plot)
    rsq_box_plot = rsq_box_plot + theme(legend.position='none')
    rmse_box_plot = rmse_box_plot + theme(legend.position='none')
    mae_box_plot = mae_box_plot + theme(legend.position='none')
    bias_box_plot = bias_box_plot + theme(legend.position='none')
    
    ### Plotting the different components with legend
    ggdraw(plot_grid(plot_grid(rsq_box_plot, rmse_box_plot, mae_box_plot, bias_box_plot, ncol=1, align='v', labels = c(plot_name), label_size = 12),
                     plot_grid(NULL, legend, NULL, ncol=1, align='h'), rel_widths=c(1, 0.15)))
    
    
    setwd(rf_plot_comp_dir)
    graph_name = paste0(plot_name,"_boxplot.png")
    ggsave(graph_name, width = 40, height = 20, units = "cm", dpi = 500)
    
    #####################
    ### SCATTER PLOTS ###
    #####################
    
    rsq_plot = ggplot(rsq, aes(x=reorder(variable_combinations, -value), y = value, color = variable_combinations)) +
      geom_point(size = 2.5) +
      #scale_fill_viridis(option="magma") +
      ylab(bquote(R^2)) + 
      xlab("") +
      scale_y_continuous(limits = c(0.25, 1)) + 
      theme_light()
    
    rmse_plot = ggplot(rmse, aes(x=reorder(variable_combinations, +value), y = value, color = variable_combinations)) +
      geom_point(size = 2.5) +
      #scale_fill_viridis(option="magma") +
      ylab("Root Mean Square Error") + 
      xlab("") +
      scale_y_continuous(limits = c(min(rmse$value), max(rmse$value))) +
      theme_light()
    
    mae_plot = ggplot(mae, aes(x=reorder(variable_combinations, +value), y = value, color = variable_combinations)) +
      geom_point(size = 2.5) +
      #scale_fill_viridis(option="magma") +
      ylab("Mean Absolute Error") + 
      xlab("") +
      scale_y_continuous(limits = c(min(mae$value), max(mae$value))) +
      theme_light()
    
    bias_plot = ggplot(bias, aes(x=reorder(variable_combinations, +value), y = value, color = variable_combinations)) +
      geom_point(size = 2.5) +
      #scale_fill_viridis(option="magma") +
      ylab("Bias") + 
      xlab("") +
      scale_y_continuous(limits = c(min(bias$value), max(bias$value))) +
      theme_light()
    
    ### Grab legend from the rsq plot, drop the legends
    legend = get_legend(rsq_plot)
    rsq_plot <- rsq_plot + theme(legend.position='none')
    rmse_plot <- rmse_plot + theme(legend.position='none')
    mae_plot <- mae_plot + theme(legend.position='none')
    bias_plot <- bias_plot + theme(legend.position='none')
    
    ### Plotting the different components with legend
    ggdraw(plot_grid(plot_grid(rsq_plot, rmse_plot, mae_plot, bias_plot, ncol=1, align='v', labels = c(plot_name), label_size = 12),
                     plot_grid(NULL, legend, NULL, ncol=1, align='h'), rel_widths=c(1, 0.15)))
    
    setwd(rf_plot_comp_dir)
    graph_name = paste0(plot_name,"_point_plot.png")
    ggsave(graph_name, width = 40, height = 20, units = "cm", dpi = 500)
    
    ##########################
    ### EXPERIMENTAL PLOTS ###
    ##########################
    
    rsq_bar_plot = ggplot(rsq, aes(x=reorder(variable_combinations, -value), y = value, fill = variable_combinations)) + 
      geom_col() + 
      #scale_fill_viridis(option="magma") +
      #scale_fill_brewer(palette = "Dark2") +
      ylab(bquote(R^2)) + 
      xlab("") +
      scale_y_continuous(limits = c(0, 1)) + 
      theme_light()
    
    rmse_bar_plot =ggplot(rmse, aes(x=reorder(variable_combinations, +value), y = value, fill = variable_combinations)) + 
      geom_col() + 
      #scale_fill_viridis(option="magma") +
      #scale_fill_brewer(palette = "Dark2") +
      ylab("Root Mean Square Error") + 
      xlab("") +
      scale_y_continuous(limits = c(0, max(rmse$value))) +
      #scale_y_continuous(limits = c(min(rmse$value), max(rmse$value))) +
      theme_light()
    
    mae_bar_plot = ggplot(mae, aes(x=reorder(variable_combinations, +value), y = value, fill = variable_combinations)) + 
      geom_col() + 
      #scale_fill_viridis(option="magma") +
      #scale_fill_brewer(palette = "Dark2") + 
      ylab("Mean Absolute Error") + 
      xlab("") +
      scale_y_continuous(limits = c(0, max(mae$value))) +
      #scale_y_continuous(limits = c(min(mae$value), max(mae$value))) +
      theme_light()
    
    ### Grab legend from the rsq plot, drop the legends
    legend = get_legend(rsq_bar_plot)
    rsq_plot <- rsq_bar_plot + theme(legend.position='none')
    rmse_plot <- rmse_bar_plot + theme(legend.position='none')
    mae_plot <- mae_bar_plot + theme(legend.position='none')
    
    ### Plotting the different components with legend
    ggdraw(plot_grid(plot_grid(rsq_plot, rmse_plot, mae_plot, ncol=1, align='v'),
                     plot_grid(NULL, legend, NULL, ncol=1, align='h'), rel_widths=c(1, 0.15)))
    
    setwd(rf_plot_comp_dir)
    graph_name = paste0(plot_name,"_bar_plot.png")
    ggsave(graph_name, width = 40, height = 20, units = "cm", dpi = 500)
    
    
  }
  
  ### Stop the parallel processing cluster
  stopCluster(cl)
  
}

### Run the function to plot model accuracy
plot_model_accuracy(desired_metrics, final_rf_model_results, rf_plot_comp_dir)
  
  
  