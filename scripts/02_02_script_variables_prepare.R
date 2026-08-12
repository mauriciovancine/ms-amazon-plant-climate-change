#' ---
#' title: variables - prepare
#' author: mauricio vancine
#' date: 2025-11-12
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(parallel)

# parameters
ncores <- parallel::detectCores() - 2
ncores

# limit -------------------------------------------------------------------

# limits
lim_neo <- terra::vect("01_data/02_variables/00_limits/neotropic/neotropic_dissolved_fill_holes.shp")
lim_neo
plot(lim_neo)

lim_am <- terra::vect("01_data/02_variables/00_limits/amazon/am_limit_raisg/LimRAISG.shp") %>% 
  terra::project(crs(lim_neo))
lim_am
plot(lim_am)

# raster neotropical
# r_neo <- terra::rast("01_data/02_variables/02_soil/bdod_0-5cm_mean_30s.tif") %>% 
#   terra::crop(lim_neo, mask = TRUE)
# r_neo <- terra::ifel(r_neo > 0, 1, NA)

# r_am <- terra::rast("01_data/02_variables/02_soil/bdod_0-5cm_mean_30s.tif") %>%
#   terra::crop(lim_am, mask = TRUE)
# r_am <- terra::ifel(r_am > 0, 1, NA)

# export
# terra::writeRaster(r_neo, "01_data/02_variables/00_limits/r_neo.tif")
# terra::writeRaster(r_am, "01_data/02_variables/00_limits/r_am.tif")

# raster default
r_neo <- terra::rast("01_data/02_variables/00_limits/r_neo.tif")
r_neo
plot(r_neo)

r_am <- terra::rast("01_data/02_variables/00_limits/r_am.tif")
r_am
plot(r_am)

# climate -----------------------------------------------------------------

# list files 
climate_var_current <- dir(path = "01_data/02_variables/01_climate/current/00_raw/", 
                           pattern = ".tif$", full.names = TRUE)
climate_var_current

# crop and mask
for(i in 1:length(climate_var_current)){
  
  name_am_1km <- sub(".tif", "_am_1km.tif", basename(climate_var_current[i]))
  name_neo_1km <- sub(".tif", "_neo_1km.tif", basename(climate_var_current[i]))
  
  name_am_10km <- sub(".tif", "_am_10km.tif", basename(climate_var_current[i]))
  name_neo_10km <- sub(".tif", "_neo_10km.tif", basename(climate_var_current[i]))
  
  r <- terra::rast(climate_var_current[i])
  
  r_am_1km <- terra::crop(r, r_am, mask = TRUE)
  r_neo_1km <- terra::crop(r, r_neo, mask = TRUE)
  
  r_am_10km <- terra::aggregate(r_am_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  r_neo_10km <- terra::aggregate(r_neo_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  
  terra::writeRaster(r_am_1km, paste0("01_data/02_variables/01_climate/current/", name_am_1km))
  terra::writeRaster(r_neo_1km, paste0("01_data/02_variables/01_climate/current/", name_neo_1km))
  terra::writeRaster(r_am_10km, paste0("01_data/02_variables/01_climate/current/", name_am_10km))
  terra::writeRaster(r_neo_10km, paste0("01_data/02_variables/01_climate/current/", name_neo_10km))
  
}

# list files 
climate_var_future <- dir(path = "01_data/02_variables/01_climate/future/00_raw/", 
                          pattern = ".tif$", full.names = TRUE) %>% 
  stringr::str_subset("CHELSA_mpi-esm1-2-hr_ssp370_bio13_2071-2100")
climate_var_future

# crop and mask
for(i in 1:length(climate_var_future)){
  
  print(i)
  
  name_am_10km <- sub(".tif", "_am_10km.tif", basename(climate_var_future[i]))
  # name_neo_10km <- sub(".tif", "_neo_10km.tif", basename(climate_var_future[i]))
  
  r <- terra::rast(climate_var_future[i])
  
  r_am_1km <- terra::crop(r, r_am, mask = TRUE)
  # r_neo_1km <- terra::crop(r, r_neo, mask = TRUE)
  
  r_am_10km <- terra::aggregate(r_am_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  # r_neo_10km <- terra::aggregate(r_neo_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  
  terra::writeRaster(r_am_10km, paste0("01_data/02_variables/01_climate/future/", name_am_10km))
  # terra::writeRaster(r_neo_10km, paste0("01_data/02_variables/01_climate/future/", name_neo_10km)) 
  
}

# soil --------------------------------------------------------------------

# list files 
soil_var <- dir(path = "01_data/02_variables/02_soil/00_raw/", 
                pattern = ".tif$", full.names = TRUE)
soil_var

soil_var_name <- soil_var %>% 
  basename() %>% 
  stringr::str_split("_", simplify = TRUE) %>% 
  .[, 1] %>% 
  unique()
soil_var_name

# crop and mask
for(i in soil_var_name){
  
  soil_files <- stringr::str_subset(soil_var, i)
  
  r <- terra::rast(soil_files)
  
  r_am_1km <- terra::crop(r, r_am, mask = TRUE) %>% 
    terra::mean()
  r_neo_1km <- terra::crop(r, r_neo, mask = TRUE) %>% 
    terra::mean()
  
  r_am_10km <- terra::aggregate(r_am_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  r_neo_10km <- terra::aggregate(r_neo_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  
  name_am_1km <- paste0(i, "_mean_am_1km.tif")
  name_neo_1km <- paste0(i, "_mean_neo_1km.tif")
  
  name_am_10km <- paste0(i, "_mean_am_10km.tif")
  name_neo_10km <- paste0(i, "_mean_neo_10km.tif")
  
  terra::writeRaster(r_am_1km, paste0("01_data/02_variables/02_soil/", name_am_1km))
  terra::writeRaster(r_neo_1km, paste0("01_data/02_variables/02_soil/", name_neo_1km))
  terra::writeRaster(r_am_10km, paste0("01_data/02_variables/02_soil/", name_am_10km))
  terra::writeRaster(r_neo_10km, paste0("01_data/02_variables/02_soil/", name_neo_10km))
  
}

# topography --------------------------------------------------------------

# list files 
topography_var <- dir(path = "01_data/02_variables/03_topography/00_raw/", 
                      pattern = ".tif$", full.names = TRUE)
topography_var

# crop and mask
for(i in 1:length(topography_var_current)){
  
  name_am_1km <- sub(".tif", "_am_1km.tif", basename(topography_var[i]))
  name_neo_1km <- sub(".tif", "_neo_1km.tif", basename(topography_var[i]))
  
  name_am_10km <- sub(".tif", "_am_10km.tif", basename(topography_var[i]))
  name_neo_10km <- sub(".tif", "_neo_10km.tif", basename(topography_var[i]))
  
  r <- terra::rast(topography_var[i])
  
  if(names(r) == "aspectsine_1KMmd_GMTEDmd"){
    r[r == -9999] <- NA 
  }

  r_am_1km <- terra::crop(r, r_am, mask = TRUE)
  r_neo_1km <- terra::crop(r, r_neo, mask = TRUE)
  
  r_am_10km <- terra::aggregate(r_am_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  r_neo_10km <- terra::aggregate(r_neo_1km, fact = 10, fun = "median", na.rm = TRUE, cores = ncores)
  
  terra::writeRaster(r_am_1km, paste0("01_data/02_variables/03_topography/", name_am_1km), overwrite = TRUE)
  terra::writeRaster(r_neo_1km, paste0("01_data/02_variables/03_topography/", name_neo_1km), overwrite = TRUE)
  terra::writeRaster(r_am_10km, paste0("01_data/02_variables/03_topography/", name_am_10km), overwrite = TRUE)
  terra::writeRaster(r_neo_10km, paste0("01_data/02_variables/03_topography/", name_neo_10km), overwrite = TRUE)
  
}

# end ---------------------------------------------------------------------
