#' ---
#' title: var - download - variables
#' author: mauricio vancine
#' date: 2025-11-11
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(rvest)
library(parallelly)
library(doParallel)
library(foreach)

# options
options(timeout = 1e6)

# climate -----------------------------------------------------------------

# url 
climate_url <- "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/bioclim/"
climate_url

bios <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
bios

years <- c("1981-2010", "2011-2040", "2041-2070", "2071-2100")
years

ssps <- c("ssp126", "ssp370", "ssp585")
ssps

gcms <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")
gcms

# names
climate_names_current <- paste0("CHELSA_", bios, "_", years[1], "_V.2.1.tif")
climate_names_current

climate_link_current <- paste0(bios, "/", years[1], "/CHELSA_", bios, "_", years[1], "_V.2.1.tif")
climate_link_current

climate_names_future <- expand.grid(
  "CHELSA",
  gcm = gcms,
  ssp = ssps,
  bios = bios,
  year = years[-1],
  "V.2.1.tif") %>% 
  apply(MARGIN = 1, FUN = paste0, collapse = "_")
climate_names_future

climate_link_future <- paste0(
  stringr::str_split(climate_names_future, "_", simplify = TRUE)[, 4], "/",
  stringr::str_split(climate_names_future, "_", simplify = TRUE)[, 5], "/",
  stringr::str_to_upper(stringr::str_split(climate_names_future, "_", simplify = TRUE)[, 2]), "/",
  stringr::str_split(climate_names_future, "_", simplify = TRUE)[, 3], "/",
  climate_names_future)
climate_link_future

# directory
setwd("01_data/02_variables/01_climate/current/00_raw")

# downloaded
climate_var_current <- dir(pattern = "1981-2010")
climate_var_current

# download
for(i in 1:length(climate_names_current)){
  
  if(!file.exists(climate_var_current[i])){
    
    download.file(url = paste0(climate_url, climate_link_current[i]), 
                  destfile = climate_names_current[i], mode = "wb")
    
  }
  
}

climate_var_future <- dir(pattern = "ssp")
climate_var_future

doParallel::registerDoParallel(parallelly::availableCores(omit = 2))
foreach::foreach(i=1:length(climate_names_future)) %dopar% {

  if(!file.exists(climate_names_future[i])){
    
    download.file(url = paste0(climate_url, climate_link_future[i]), 
                  destfile = climate_names_future[i], mode = "wb")
    
  }
  
}
doParallel::stopImplicitCluster()

# soil --------------------------------------------------------------------

# url 
soil_url <- "https://geodata.ucdavis.edu/geodata/soil/soilgrids/"
soil_url

# names
soil_names <- rvest::read_html(soil_url) %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("\\.tif$")
soil_names

# download
soil_var <- dir()
soil_var

for(i in soil_names){
  
  if(!file.exists(i)){
    
    download.file(url = paste0(soil_url, i), 
                  destfile = i, mode = "wb")
    
  }
  
}

# topography --------------------------------------------------------------

# url 
topography_url <- "https://data.earthenv.org/topography/"
topography_url

# names
topography_names <- paste0(c("slope", "aspectsine", "pcurv", "tcurv", "tpi", 
                             "tri", "vrm"), "_1KMmd_GMTEDmd.tif")
topography_names

# download
topography_var <- dir()
topography_var

for(i in topography_names){
  
  if(!file.exists(i)){
    
    download.file(url = paste0(topography_url, i), 
                  destfile = i, mode = "wb")
    
  }
  
}

# end ---------------------------------------------------------------------
