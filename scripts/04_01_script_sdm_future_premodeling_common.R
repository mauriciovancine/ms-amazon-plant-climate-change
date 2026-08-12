#' ----
#' title: sdm - common - pre-modeling
#' author: mauricio vancine
#' date: 05/07/2026
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(terra)
library(tmap) 
library(cols4all)
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

## limits ----
lim_train <- terra::vect("01_data/02_variables/00_limits/neotropic/neotropic_dissolved_fill_holes.shp")
lim_train
plot(lim_train)

lim_proj <- terra::vect("01_data/02_variables/00_limits/amazon/am_limit_raisg/LimRAISG.shp")
lim_proj
plot(lim_proj)

# plot
tm_shape(terra::vect(occ, geom = c("x", "y"), crs = "EPSG:4326"), bbox = sf::st_bbox(lim_train)) +
    tm_dots(fill = "gray") +
    tm_shape(lim_train) +
    tm_borders() +
    tm_shape(lim_proj) +
    tm_borders(col = "red")

## variables ----

### climate ----
var_train_climate <- dir("01_data/02_variables/01_climate/current/01_scale/", 
                         pattern = "neo_1km_scale.tif", full.names = TRUE) %>% 
    terra::rast()
var_train_climate

names(var_train_climate)
names(var_train_climate) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var_train_climate)

plot(var_train_climate[[1]])

### soil ----
var_train_soil <- dir("01_data/02_variables/02_soil/01_scale/", 
                      pattern = "neo_1km_scale.tif", full.names = TRUE) %>% 
    terra::rast()
var_train_soil

names(var_train_soil)
names(var_train_soil) <- stringr::str_split(stringr::str_split(dir("01_data/02_variables/02_soil/01_scale", pattern = "neo_1km_scale.tif", full.names = TRUE), "_", simplify = TRUE)[,5], "/", simplify = TRUE)[,2]
names(var_train_soil)

plot(var_train_soil[[1]])

### topography ----
var_train_topography <- dir("01_data/02_variables/03_topography/01_scale/", 
                            pattern = "neo_1km_scale.tif", full.names = TRUE) %>% 
    terra::rast()
var_train_topography

names(var_train_topography)
names(var_train_topography) <- stringr::str_split(names(var_train_topography), "_", simplify = TRUE)[,1]
names(var_train_topography)

plot(var_train_topography[[1]])

### combine ----
var_train <- c(var_train_climate, var_train_soil, var_train_topography)
var_train

plot(var_train[[1]])

## clean memory ----
rm(var_train_climate, var_train_soil, var_train_topography)

# pre-modeling ----------------------------------------------------------------

## pc config ----
parallel::detectCores(logical = FALSE)
parallel::detectCores(logical = TRUE)
sapply(ps::ps_system_memory(), function(x) round(x/1024^3, 2))
n_cores <- parallel::detectCores(logical = TRUE) - 2

## parameters ----
ca_method <- "bmcp" # buffer, mcp, bmcp, mask
ca_buffer <- 500000

occ_filt_env_nbins <- seq(2, 20, 2)
occ_filt_rep <- 5
env_layer_fact <- 5

psa_multi <- 2
psa_method <- "geoenv_const" # 
psa_width <- "10000"

bg_number <- 10000  
bg_method <- "thickening" # random, thickening, biased

part_n <- 5
part_min_occ <- 5

## pre-modeling ----
for(i in sort(unique(occ$species))[c(72)]){}
    
    # information
    print(i)
    sp_name <- paste0(sub(" ", "_", tolower(i)))
    
    # directories
    sp_dir_sp <- paste0("02_output/01_sdm_common/", sp_name, "/")
    sp_dir_pre_model <- paste0("02_output/01_sdm_common/", sp_name, "/01_pre_modeling/")
    
    dir.create(path = sp_dir_sp)
    dir.create(path = sp_dir_pre_model)
    
    ### species filter ----
    occ_i <- occ %>% 
        dplyr::filter(species == i) %>% 
        tibble::rowid_to_column(var = "id") %>% 
        dplyr::mutate(pr_ab = 1, occ_filter = "total") %>% 
        dplyr::select(id, x, y, pr_ab, occ_filter)
    
    ## calibration area ----
    ca <- flexsdm::calib_area(
        data = occ_i, 
        x = "x", 
        y = "y", 
        method = c(method = ca_method, width = ca_buffer),
        crs = crs(var_train))
    
    terra::writeVector(ca, paste0(sp_dir_pre_model, "02_01_var_ca_", sp_name, ".gpkg"), overwrite = TRUE)
    
    map_occ_ca <- tm_shape(lim_train) +
        tm_polygons() +
        tm_shape(lim_proj) +
        tm_borders(col = "blue") +
        tm_shape(ca) +
        tm_borders(col = "red") +
        tm_shape(terra::vect(occ_i[, -5], geom = c("x", "y"), crs = "EPSG:4326")) +
        tm_dots(fill_alpha = .3) +
        tm_title(text = i, fontface = "italic")
    tmap::tmap_save(map_occ_ca, paste0(sp_dir_pre_model, "02_01_var_ca_", sp_name, "_map.png"))
    
    ## variable adjustment ----
    var_train_ca <- terra::crop(var_train, ca, mask = TRUE)
    
    ## variable collinearity ----
    var_train_ca_colin <- flexsdm::correct_colinvar(env_layer = var_train_ca, method = c("vif", th = "2"))
    var_train_ca_sel_names <- var_train_ca_colin$vif_table$Variables
    var_train_ca_sel <- var_train_ca_colin$env_layer
    
    readr::write_csv(var_train_ca_colin$vif_table, paste0(sp_dir_pre_model, "02_02_var_vif_", sp_name, ".csv"))
    
    rm(var_train_ca)
    gc(verbose = FALSE, reset = TRUE)
    
    ## variable aggregate ----
    env_layer <- terra::aggregate(x = var_train_ca_sel, fact = env_layer_fact, cores = n_cores)
    ext(env_layer) <- ext(ca)
    
    ## occurrences filtering ----
    occ_i_filt_res <- flexsdm::occfilt_env(
        data = occ_i, 
        x = "x", 
        y = "y", 
        id = "id",
        env_layer = env_layer, 
        nbins = occ_filt_env_nbins)
    
    occ_i_filt_sel <- flexsdm::occfilt_select(
        occ_list = occ_i_filt_res,
        x = "x",
        y = "y",
        env_layer = env_layer,
        filter_prop = TRUE)
    
    occ_i_filt <- dplyr::mutate(occ_i_filt_sel$occ, pr_ab = 1, occ_filter = "filtered")
    
    readr::write_csv(occ_i_filt_sel$occ, paste0(sp_dir_pre_model, "01_01_occ_", sp_name, "_filter.csv"))    
    readr::write_csv(occ_i_filt_sel$filter_prop, paste0(sp_dir_pre_model, "01_01_occ_", sp_name, "_filter_prop.csv"))    
    
    map_occ_filt <- tm_shape(lim_train) +
        tm_polygons() +
        tm_shape(lim_proj) +
        tm_borders(col = "blue") +
        tm_shape(ca) +
        tm_borders(col = "red") +
        tm_shape(terra::vect(rbind(occ_i[, 1:5], occ_i_filt), geom = c("x", "y"), crs = "EPSG:4326")) +
        tm_dots(size = .25,
                fill = "occ_filter",
                fill.scale = tm_scale_categorical(values = c("steelblue", "black")),
                fill.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom"))) +
        tm_title(text = i, fontface = "italic")
    tmap::tmap_save(map_occ_filt, paste0(sp_dir_pre_model, "01_01_occ_", sp_name, "_filter_map.png"))
    
    ## data sampling ----
    
    ### pseudo-absence ----
    psa <- flexsdm::sample_pseudoabs(
        data = occ_i_filt,
        x = "x",
        y = "y",
        n = nrow(occ_i_filt) * psa_multi,
        method = c(
            method = "geoenv_const", 
            width = psa_width, 
            env = env_layer),
        rlayer = env_layer,
        calibarea = ca)
    readr::write_csv(psa, paste0(sp_dir_pre_model, "01_02_occ_", sp_name, "_psa.csv"))    
    
    map_psa <- tm_shape(lim_train) +
        tm_polygons() +
        tm_shape(lim_proj) +
        tm_borders(col = "blue") +
        tm_shape(ca) +
        tm_borders(col = "red") +
        tm_shape(terra::vect(psa, geom = c("x", "y"), crs = "EPSG:4326")) +
        tm_dots(fill = "gray50") +
        tm_shape(terra::vect(occ_i_filt, geom = c("x", "y"), crs = "EPSG:4326")) +
        tm_dots() +
        tm_title(text = i, fontface = "italic")
    tmap::tmap_save(map_psa, paste0(sp_dir_pre_model, "01_02_occ_", sp_name, "_psa_map.png"))
    
    ### background ----
    bg <- flexsdm::sample_background(
        data = occ_i_filt,
        x = "x",
        y = "y",
        n = bg_number,
        method = bg_method,
        rlayer = env_layer,
        calibarea = ca)
    readr::write_csv(bg, paste0(sp_dir_pre_model, "01_03_occ_", sp_name, "_bg.csv"))   
    
    map_bg <- tm_shape(lim_train) +
        tm_polygons() +
        tm_shape(lim_proj) +
        tm_borders(col = "blue") +
        tm_shape(ca) +
        tm_borders(col = "red") +
        tm_shape(terra::vect(bg, geom = c("x", "y"), crs = "EPSG:4326")) +
        tm_dots(fill = "gray50") +
        tm_shape(terra::vect(occ_i_filt, geom = c("x", "y"), crs = "EPSG:4326")) +
        tm_dots() +
        tm_title(text = i, fontface = "italic")
    tmap::tmap_save(map_bg, paste0(sp_dir_pre_model, "01_03_occ_", sp_name, "_bg_map.png"))
    
    ## data partitioning ----
    
    ### pseudo-absence ----
    occ_i_filt_psa <- rbind(occ_i_filt[, 2:4], psa)
    
    # blocks
    occ_i_filt_psa_part <- flexsdm::part_sblock(
        data = occ_i_filt_psa,
        env_layer = env_layer,
        pr_ab = "pr_ab",
        x = "x",
        y = "y",
        n_part = part_n,
        min_res_mult = 20,
        max_res_mult = 300,
        num_grids = 60,
        min_occ = part_min_occ,
        prop = .9)
    part_type <- "block"
    
    # latitudinal bands
    if(length(occ_i_filt_psa_part) < 3){
        occ_i_filt_psa_part <- flexsdm::part_sband(
            data = occ_i_filt_psa,
            env_layer = env_layer,
            x = "x",
            y = "y",
            pr_ab = "pr_ab",
            type = "lat",
            n_part = part_n,
            min_bands = 4,
            max_bands = 60,
            min_occ = part_min_occ,
            prop = .9)
        part_type <- "band_lat"
    }
    
    # longitudinal bands
    if(length(occ_i_filt_psa_part) < 3){
        occ_i_filt_psa_part <- flexsdm::part_sband(
            data = occ_i_filt_psa,
            env_layer = env_layer,
            x = "x",
            y = "y",
            pr_ab = "pr_ab",
            type = "lon",
            n_part = part_n,
            min_bands = 4,
            max_bands = 60,
            min_occ = part_min_occ,
            prop = .9)
        part_type <- "band_lon"
    }
    
    if(any("best_part_info" == names(occ_i_filt_psa_part))){
        
        occ_i_filt_psa_part_occ <- occ_i_filt_psa_part$part
        
        occ_i_filt_psa_part_info <- occ_i_filt_psa_part$best_part_info %>% 
            dplyr::mutate(method = part_type) %>% 
            dplyr::relocate(method, .before = 1)
        
        readr::write_csv(occ_i_filt_psa_part_occ, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part.csv"))
        readr::write_csv(occ_i_filt_psa_part_info, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part_info.csv"))    
        
        occ_i_filt_psa_part_grid_ca <- flexsdm::get_block(
            env_layer = env_layer,
            best_grid = occ_i_filt_psa_part$grid)
        
        terra::writeRaster(occ_i_filt_psa_part$grid, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part_rast_grid.tif"), overwrite = TRUE)
        terra::writeRaster(occ_i_filt_psa_part_grid_ca, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part_rast_grid_ca.tif"), overwrite = TRUE)
        
        n_part_color <- cols4all::c4a("magma", n = max(occ_i_filt_psa_part_occ$.part))
        n_part_color_occs <- cols4all::c4a("set1", n = max(occ_i_filt_psa_part_occ$.part))
        map_occ_i_filt_psa_part <- tm_shape(lim_train) +
            tm_polygons() +
            tm_shape(lim_proj) +
            tm_borders(col = "blue") +
            tm_shape(occ_i_filt_psa_part_grid_ca) +
            tm_raster(col.legend = tm_legend(show = FALSE),
                      col.scale = tm_scale_categorical(values = n_part_color)) +
            tm_shape(ca) +
            tm_borders(col = "red") +
            tm_shape(terra::vect(occ_i_filt_psa_part_occ, geom = c("x", "y"), crs = "EPSG:4326")) +
            tm_dots(fill = ".part",
                    shape = "pr_ab",
                    size = .5,
                    fill.scale = tm_scale_categorical(values = n_part_color_occs),
                    fill.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom")),
                    shape.scale = tm_scale_categorical(values = c(17, 16)),
                    shape.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom"))) +
            tm_title(text = i, fontface = "italic")
        tmap::tmap_save(map_occ_i_filt_psa_part, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part_map.png"))
        
    }
    
    # kfold
    if(length(occ_i_filt_psa_part) < 3){
        
        occ_i_filt_psa_part_occ <- flexsdm::part_random(
            data = occ_i_filt_psa,
            pr_ab = "pr_ab",
            method = c(method = "kfold", folds = part_n))
        
        occ_i_filt_psa_part_info <- tibble::tibble(method = "kfold", folds = part_n)
        
        readr::write_csv(occ_i_filt_psa_part_occ, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part.csv"))    
        readr::write_csv(occ_i_filt_psa_part_info, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part_info.csv"))
        
        n_part_color_occs <- cols4all::c4a("set1", n = max(occ_i_filt_psa_part_occ$.part))
        map_occ_i_filt_psa_part <- tm_shape(lim_train) +
            tm_polygons() +
            tm_shape(lim_proj) +
            tm_borders(col = "blue") +
            tm_shape(ca) +
            tm_borders(col = "red") +
            tm_shape(terra::vect(occ_i_filt_psa_part, geom = c("x", "y"), crs = "EPSG:4326")) +
            tm_dots(fill = ".part",
                    shape = "pr_ab",
                    size = .5,
                    fill.scale = tm_scale_categorical(values = n_part_color_occs),
                    fill.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom")),
                    shape.scale = tm_scale_categorical(values = c(17, 16)),
                    shape.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom"))) +
            tm_title(text = i, fontface = "italic")
        tmap::tmap_save(map_occ_i_filt_psa_part, paste0(sp_dir_pre_model, "01_04_01_occ_", sp_name, "_psa_part_map.png"))
        
    }
    
    ### background ----
    if(any("best_part_info" == names(occ_i_filt_psa_part))){
        
        bg_part <- flexsdm::sdm_extract(
            data = bg,
            x = "x",
            y = "y",
            env_layer = occ_i_filt_psa_part$grid)
        
        readr::write_csv(bg_part, paste0(sp_dir_pre_model, "01_04_02_occ_", sp_name, "_bg_part.csv"))    
        
        n_part_color_bg <- cols4all::c4a("magma", n = max(occ_i_filt_psa_part$part$.part))
        map_bg_part <- tm_shape(lim_train) +
            tm_polygons() +
            tm_shape(lim_proj) +
            tm_borders(col = "blue") +
            tm_shape(occ_i_filt_psa_part_grid_ca) +
            tm_raster(col.legend = tm_legend(show = FALSE),
                      col.scale = tm_scale_categorical(values = n_part_color_bg)) +
            tm_shape(ca) +
            tm_borders(col = "red") +
            tm_shape(terra::vect(bg_part, geom = c("x", "y"), crs = "EPSG:4326")) +
            tm_dots(fill = ".part",
                    size = .5,
                    fill.scale = tm_scale_categorical(values = n_part_color_occs),
                    fill.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom"))) +
            tm_title(text = i, fontface = "italic")
        tmap::tmap_save(map_bg_part, paste0(sp_dir_pre_model, "01_04_02_occ_", sp_name, "_bg_part_map.png"))
        
    }
    
    # kfold
    if(length(occ_i_filt_psa_part) < 3){
        
        bg_part <- flexsdm::part_random(
            data = bg,
            pr_ab = "pr_ab",
            method = c( method = "kfold", folds = part_n))
        
        readr::write_csv(bg_part, paste0(sp_dir_pre_model, "01_04_02_occ_", sp_name, "_bg_part.csv"))    
        
        n_part_color_bg <- cols4all::c4a("set1", n = max(bg_part$.part))
        map_bg_part <- tm_shape(lim_train) +
            tm_polygons() +
            tm_shape(lim_proj) +
            tm_borders(col = "blue") +
            tm_shape(ca) +
            tm_borders(col = "red") +
            tm_shape(terra::vect(bg_part, geom = c("x", "y"), crs = "EPSG:4326")) +
            tm_dots(fill = ".part",
                    shape = "pr_ab",
                    size = .5,
                    fill.scale = tm_scale_categorical(values = n_part_color_bg),
                    fill.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom")),
                    shape.scale = tm_scale_categorical(values = c(17, 16)),
                    shape.legend = tm_legend(position = tm_pos_in(pos.h = "left", pos.v = "bottom"))) +
            tm_title(text = i, fontface = "italic")
        tmap::tmap_save(map_bg_part, paste0(sp_dir_pre_model, "01_04_02_occ_", sp_name, "_bg_part_map.png"))
    }
    
    ## extracting environmental values ----
    occ_i_filt_psa_part_data <- flexsdm::sdm_extract(
        data = occ_i_filt_psa_part_occ,
        x = "x",
        y = "y",
        env_layer = var_train_ca_sel,
        filter_na = TRUE)
    readr::write_csv(occ_i_filt_psa_part_data, paste0(sp_dir_pre_model, "02_03_01_var_data_", sp_name, "_occ_psa.csv"))    
    
    bg_part_data  <- flexsdm::sdm_extract(
        data = bg_part,
        x = "x",
        y = "y",
        env_layer = var_train_ca_sel,
        filter_na = TRUE)
    readr::write_csv(bg_part_data, paste0(sp_dir_pre_model, "02_03_02_var_data_", sp_name, "_bg.csv"))   
    
    ## clean memory ----
    rm(var_train_ca_sel)
    gc(verbose = FALSE, reset = TRUE)
    
}

# end ---------------------------------------------------------------------