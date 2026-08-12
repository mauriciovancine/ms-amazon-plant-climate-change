#' ----
#' title: sdm - common - pos-modeling
#' author: mauricio vancine
#' date: 03/07/2026
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(flexsdm) # pak::pak("sjevelazco/flexsdm")

# options
options(scipen = 1000)
set.seed(42)

# import data -------------------------------------------------------------

## occurrences ----
occ <- readr::read_csv("01_data/01_occurrences/01_occurrences/01_filtered/occ_tanguro_oppc_adjusted_common.csv") %>% 
    dplyr::select(scientificName, decimalLongitude, decimalLatitude) %>% 
    dplyr::rename(species = scientificName, x = decimalLongitude, y = decimalLatitude)
occ

## variables ----

### climate current ----
var_proj_climate_c <- dir("01_data/02_variables/01_climate/current/01_scale/", 
                          pattern = "am_1km_scale.tif", full.names = TRUE) %>% 
    terra::rast()
var_proj_climate_c

names(var_proj_climate_c)
names(var_proj_climate_c) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var_proj_climate_c)

plot(var_proj_climate_c[[1]])

### climate future ----
var_proj_climate_f <- dir("01_data/02_variables/01_climate/future/01_scale/", 
                          pattern = "am_1km_scale.tif", full.names = TRUE) %>% 
    terra::rast()
var_proj_climate_f

var_proj_climate_f_names_sep <- dir("01_data/02_variables/01_climate/future/01_scale/",
                                    pattern = "am_1km_scale.tif", full.names = TRUE) %>% 
    basename() %>% 
    stringr::str_replace_all("CHELSA_", "") %>% 
    stringr::str_replace_all("_V.2.1_am_1km_scale.tif", "") %>% 
    stringr::str_split(pattern = "_", simplify = TRUE)
var_proj_climate_f_names_sep

var_proj_climate_f_names <- paste(var_proj_climate_f_names_sep[, 3],
                                  var_proj_climate_f_names_sep[, 1], 
                                  var_proj_climate_f_names_sep[, 2],
                                  var_proj_climate_f_names_sep[, 4],
                                  sep = "_")
var_proj_climate_f_names

names(var_proj_climate_f)
names(var_proj_climate_f) <- var_proj_climate_f_names
names(var_proj_climate_f)

fut <- paste(var_proj_climate_f_names_sep[, 1], 
             var_proj_climate_f_names_sep[, 2],
             var_proj_climate_f_names_sep[, 4], 
             sep = "_") %>% 
    unique()
fut

plot(var_proj_climate_f[[1]])

### soil ----
var_proj_soil <- dir("01_data/02_variables/02_soil/01_scale/", 
                     pattern = "am_1km_scale.tif", full.names = TRUE) %>% 
    terra::rast()
var_proj_soil

names(var_proj_soil)
names(var_proj_soil) <- stringr::str_split(names(var_proj_soil), "_", simplify = TRUE)[,1]
names(var_proj_soil)

plot(var_proj_soil[[1]])

### topography
var_proj_topography <- dir("01_data/02_variables/03_topography/01_scale/", 
                           pattern = "am_1km_scale.tif", full.names = TRUE) %>% 
    terra::rast()
var_proj_topography

names(var_proj_topography)
names(var_proj_topography) <- stringr::str_split(names(var_proj_topography), "_", simplify = TRUE)[,1]
names(var_proj_topography)

plot(var_proj_topography[[1]])

### combine ----
var_proj_c <- c(var_proj_climate_c, var_proj_soil, var_proj_topography)
var_proj_c

var_proj_f <- c(var_proj_climate_f, var_proj_soil, var_proj_topography) 
var_proj_f

### clean memory ----
rm(var_proj_climate_c, var_proj_climate_f, var_proj_soil, var_proj_topography)

# pos-modeling ------------------------------------------------------------

## prediction models
pred_models <- FALSE

## chunks ----
n_chunks <- 5

## post-modeling ----
for(i in sort(unique(occ$species))){
    
    # information ----
    print(i)
    sp_name <- paste0(sub(" ", "_", tolower(i)))
    
    # directories ----
    sp_dir_model <- paste0("02_output/01_sdm_common/", sp_name, "/02_modeling/")
    sp_dir_pos_model <- paste0("02_output/01_sdm_common/", sp_name, "/03_pos_modeling/")
    
    dir.create(path = sp_dir_pos_model)
    
    # models ----
    good_models <- readr::read_rds(paste0(sp_dir_model, "03_05_mod_good_models_", sp_name, ".rds"))
    ensemble_good_models <- readr::read_rds(paste0(sp_dir_model, "03_07_mod_ensemble_good_models_", sp_name, ".rds"))
    
    # variables ----
    var_names <- readr::read_csv(paste0(sp_dir_model, "03_03_mod_importance_", sp_name, "_data.csv")) %>% 
        dplyr::distinct(predictors) %>% 
        dplyr::pull()
    var_proj_c_sel <- var_proj_c[[var_names]]
    
    ## predict models ----
    if(pred_models){
        
        if(length(good_models) > 0){
            
            for(alg in names(good_models)){
                
                mod_prd <- flexsdm::sdm_predict(
                    models = good_models[[alg]], 
                    pred = var_proj_c_sel, 
                    nchunk = n_chunks,
                    thr = "max_sorensen",
                    clamp = TRUE,
                    con_thr = TRUE, 
                    pred_type = "cloglog") %>% 
                    round(3)
                
                terra::writeRaster(
                    mod_prd[[1]][[1]],
                    filename = paste0(sp_dir_pos_model, "04_01_pred_model_good_models_", alg, "_", sp_name, "_current.tif"),
                    overwrite = TRUE,
                    filetype = "GTiff")
                
                terra::writeRaster(
                    mod_prd[[1]][[2]],
                    filename = paste0(sp_dir_pos_model, "04_01_pred_model_good_models_", alg, "_", sp_name, "_current_thr.tif"),
                    overwrite = TRUE,
                    filetype = "GTiff")
            }
        }
    }
    
    ## predict ensemble ----
    if(length(ensemble_good_models) > 1){
        
        ens_prd <- flexsdm::sdm_predict(
            models = ensemble_good_models,
            pred = var_proj_c_sel,
            nchunk = n_chunks,
            thr = "max_sorensen",
            con_thr = TRUE,
            clamp = TRUE,
            pred_type = "cloglog")
        
    } else{
        
        ens_prd <- flexsdm::sdm_predict(
            models = ensemble_good_models[[1]],
            pred = var_proj_c_sel,
            nchunk = n_chunks,
            thr = "max_sorensen",
            con_thr = TRUE,
            clamp = TRUE,
            pred_type = "cloglog")
        
    }
    
    terra::writeRaster(
        ens_prd[[1]][[1]],
        filename = paste0(sp_dir_pos_model, "04_02_pred_ensemble_good_models_", sp_name, "_current.tif"),
        overwrite = TRUE,
        filetype = "GTiff")
    
    terra::writeRaster(
        ens_prd[[1]][[2]],
        filename = paste0(sp_dir_pos_model, "04_02_pred_ensemble_good_models_", sp_name, "_current_thr.tif"),
        overwrite = TRUE,
        filetype = "GTiff")
    
    # clean
    gc(verbose = FALSE, reset = TRUE)
    
}

# future ----
for(i in sort(unique(occ$species))){
    
    # information ----
    print(i)
    sp_name <- paste0(sub(" ", "_", tolower(i)))
    
    # directories ----
    sp_dir_model <- paste0("02_output/01_sdm_common/", sp_name, "/02_modeling/")
    sp_dir_pos_model <- paste0("02_output/01_sdm_common/", sp_name, "/03_pos_modeling/")
    
    dir.create(path = sp_dir_pos_model)
    
    # models ----
    good_models <- readr::read_rds(paste0(sp_dir_model, "03_05_mod_good_models_", sp_name, ".rds"))
    ensemble_good_models <- readr::read_rds(paste0(sp_dir_model, "03_07_mod_ensemble_good_models_", sp_name, ".rds"))
    
    # variables ----
    for(f in fut){}
    
    ## select variables ----
    var_names <- as.character(ensemble_good_models$predictors[1, ])
    var_names_f <- paste(grep("bio", var_names, value = TRUE), f, sep = "_")
    var_names_sel <- c(var_names_f, grep("bio", var_names, invert = TRUE, value = TRUE))
    var_proj_f_sel <- terra::subset(var_proj_f, var_names_sel)
    names(var_proj_f_sel) <- var_names
    
    ## predict models ----
    if(pred_models){
        
        if(length(good_models) > 0){
            
            for(alg in names(good_models)){
                
                mod_prd <- flexsdm::sdm_predict(
                    models = good_models[[alg]], 
                    pred = var_proj_f_sel, 
                    nchunk = n_chunks,
                    thr = "max_sorensen",
                    clamp = TRUE,
                    con_thr = TRUE, 
                    pred_type = "cloglog")
                
                terra::writeRaster(
                    mod_prd[[1]][[1]],
                    filename = paste0(sp_dir_pos_model, "04_03_pred_model_good_models_", alg, "_", sp_name, "_future_", f, ".tif"),
                    overwrite = TRUE,
                    filetype = "GTiff")
                
                terra::writeRaster(
                    mod_prd[[1]][[2]],
                    filename = paste0(sp_dir_pos_model, "04_03_pred_model_good_models_", alg, "_", sp_name, "_future_", f, "_thr.tif"),
                    overwrite = TRUE,
                    filetype = "GTiff")
            }
        }
    }
    
    ## predict ensemble ----
    if(length(ensemble_good_models) > 0){
        
        ens_prd <- flexsdm::sdm_predict(
            models = ensemble_good_models,
            pred = var_proj_f_sel,
            nchunk = n_chunks,
            thr = "max_sorensen",
            con_thr = TRUE,
            clamp = TRUE,
            pred_type = "cloglog")
        
        terra::writeRaster(
            ens_prd[[1]][[1]],
            filename = paste0(sp_dir_pos_model, "04_04_pred_ensemble_good_models_", sp_name, "_future_", f, ".tif"),
            overwrite = TRUE,
            filetype = "GTiff")
        
        terra::writeRaster(
            ens_prd[[1]][[2]],
            filename = paste0(sp_dir_pos_model, "04_04_pred_ensemble_good_models_", sp_name, "_future_", f, "_thr.tif"),
            overwrite = TRUE,
            filetype = "GTiff")
    }
    
}

# shape -------------------------------------------------------------------

## parameters ----
shape_quantile_values <- c(.7, .8, .9)

## shape ----
extr <- flexsdm::extra_eval(
    training_data = occ_i_filt_psa_part_data,
    pr_ab = "pr_ab", 
    projection_data = var_proj_sel_current,
    metric = "mahalanobis",
    univar_comb = FALSE,
    aggreg_factor = 1)

#### truncate ----
predict_ensemble_trunc <- flexsdm::extra_truncate(
    suit = predict_ensemble_current, 
    extra = extr, 
    threshold = shape_values_q)

for(s in 1:length(shape_values_q)){
    terra::writeRaster(predict_ensemble_trunc[[s]], paste0(sp_dir, "05_pred_ensemble_cont_thr_", sp_name, "_current_truncate_shape_", shape_names[s], ".tif"), overwrite = TRUE)  
}

shape_values <- terra::global(extr, fun = quantile, probs = shape_quantile_values, na.rm = TRUE)
shape_values_q <- as.numeric(shape_values)
shape_values <- round(shape_values, 2)
shape_names <- sub("[.]", "", sub("X", "p", names(shape_values)))
names(shape_values) <- shape_names
shape_values$type <- "mahalanobis"

terra::writeRaster(extr, paste0(sp_dir, "05_shape_", sp_name, ".tif"), overwrite = TRUE)
readr::write_csv(shape_values, paste0(sp_dir, "05_shape_metric_", sp_name, ".csv"))


# end ---------------------------------------------------------------------
