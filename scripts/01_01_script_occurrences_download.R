#' ----
#' title: obj2 - occurrences - download
#' author: mauricio vancine
#' date: 2026-04-13
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(vroom)
library(sf)
library(rnaturalearth)
library(BIEN)
library(jabotR) # remotes::install_github("DBOSlab/jabotR")

# options
sf::sf_use_s2(FALSE)

# import data -------------------------------------------------------------

# limit
lim_neo <- sf::st_read("01_data/00_limits/neotropic/neotropic_dissolved_fill_holes.shp")
lim_neo
plot(lim_neo)

# countries
countries <- rnaturalearth::ne_countries(scale = 10)
countries

# country filter
countries_neo <- countries[lim_neo,]
countries_neo$admin

# download atlantic epiphytes ----------------------------------------------

# download
download.file(url = "https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2541&file=ecy2541-sup-0002-DataS1.zip", 
              destfile = "01_data/01_occurrences/00_raw/01_at_epiphytes/ecy2541-sup-0002-DataS1.zip", method = "wb")

unzip(zipfile = "01_data/01_occurrences/00_raw/01_at_epiphytes/ecy2541-sup-0002-DataS1.zip", 
      exdir = "01_data/01_occurrences/00_raw/01_at_epiphytes")


# download bien -----------------------------------------------------------

# directory
setwd("02_goal_large_scale/01_data/01_occurrence/01_raw/01_bien/")

# download
for(i in sort(unique(countries_neo$admin))){
  
  print(i)
  occ_bien <- BIEN::BIEN_occurrence_country(country = i)
  vroom::vroom_write(occ_bien, 
                     paste0("occ_bien_", gsub(" ", "_", tolower(i)), ".csv"), 
                     delim = ",")
  
}

# download jabot ----------------------------------------------------------

# dowload
jabotR::jabot_download(
  verbose = TRUE, 
  dir = "01_data/01_occurrences/01_raw/09_jabot")

# end ---------------------------------------------------------------------
