#' ----
#' title: obj2 - occurrences - space filter
#' author: mauricio vancine
#' date: 2026-06-21
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)
library(rnaturalearth)
library(bdc)
library(CoordinateCleaner)

# options
sf::sf_use_s2(FALSE)

# source
source("00_scripts/80_source_function_bdc_create_report.R")

# data
rnaturalearth_ocean_10 <- rnaturalearth::ne_download(
  scale = 10, type = 'land', category = 'physical', returnclass = "sf") %>% 
  sf::st_union() %>% 
  sf::st_buffer(2000/30/3600)
rnaturalearth_ocean_10

# atlantic epiphytes ------------------------------------------------------

# import data
occ_atlantic_epiphytes <- arrow::read_parquet("01_data/01_occurrences/06_filter_taxonomy/occ_atlantic_epiphytes_pf_time_taxonomy_output.parquet")
occ_atlantic_epiphytes

readr::write_csv(occ_atlantic_epiphytes, "01_data/01_occurrences/occ_atlantic_epiphytes_spat.csv")

## 7.1 flagging common spatial issues ----
occ_atlantic_epiphytes_space <- bdc::bdc_coordinates_precision(
  data = occ_atlantic_epiphytes,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_atlantic_epiphytes_space

## 7.2 flagging bias spatial issues ----
occ_atlantic_epiphytes_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_atlantic_epiphytes_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 1000m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_atlantic_epiphytes_space

occ_atlantic_epiphytes_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_atlantic_epiphytes_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_atlantic_epiphytes_space

## summary ----
occ_atlantic_epiphytes_space <- bdc::bdc_summary_col(data = occ_atlantic_epiphytes_space)
occ_atlantic_epiphytes_space

## report ----
occ_atlantic_epiphytes_space_report <- bdc_create_report(
  data = occ_atlantic_epiphytes_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_atlantic_epiphytes_space_report

## filter ----  
occ_atlantic_epiphytes_space_output <- occ_atlantic_epiphytes_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_atlantic_epiphytes_space_output

## export ----
arrow::write_parquet(occ_atlantic_epiphytes_space_output, "01_data/01_occurrences/07_filter_spatial/occ_atlantic_epiphytes_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_atlantic_epiphytes_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_atlantic_epiphytes_pf_time_taxonomy_space_report.parquet")

# bien -----------------------------------------------------------------

# import data
occ_bien <- arrow::read_parquet("01_data/01_occurrences/06_filter_taxonomy/occ_bien_pf_time_taxonomy_output.parquet")
occ_bien

## 7.1 flagging common spatial issues ----
occ_bien_space <- bdc::bdc_coordinates_precision(
  data = occ_bien,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_bien_space

## 7.2 flagging bias spatial issues ----
occ_bien_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_bien_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    # countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_bien_space

occ_bien_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_bien_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_bien_space

## summary ----
occ_bien_space <- bdc::bdc_summary_col(data = occ_bien_space)
occ_bien_space

## report ----
occ_bien_space_report <- bdc_create_report(
  data = occ_bien_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_bien_space_report

## filter ----  
occ_bien_space_output <- occ_bien_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_bien_space_output

## export ----
arrow::write_parquet(occ_bien_space_output, "01_data/01_occurrences/07_filter_spatial/occ_bien_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_bien_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_bien_pf_time_taxonomy_space_report.parquet")

# dryflor -----------------------------------------------------------------

# import data
occ_dryflor <- arrow::read_parquet("01_data/01_occurrences/06_filter_taxonomy/occ_dryflor_pf_time_taxonomy_output.parquet")
occ_dryflor

## 7.1 flagging common spatial issues ----
occ_dryflor_space <- bdc::bdc_coordinates_precision(
  data = occ_dryflor,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_dryflor_space

## 7.2 flagging bias spatial issues ----
occ_dryflor_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_dryflor_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_dryflor_space

occ_dryflor_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_dryflor_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_dryflor_space

## summary ----
occ_dryflor_space <- bdc::bdc_summary_col(data = occ_dryflor_space)
occ_dryflor_space

## report ----
occ_dryflor_space_report <- bdc_create_report(
  data = occ_dryflor_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_dryflor_space_report

## filter ----  
occ_dryflor_space_output <- occ_dryflor_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_dryflor_space_output

## export ----
arrow::write_parquet(occ_dryflor_space_output, "01_data/01_occurrences/07_filter_spatial/occ_dryflor_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_dryflor_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_dryflor_pf_time_taxonomy_space_report.parquet")

# gbif ------------------------------------------------------

for(g in 0:64){
  
  # import data
  occ_gbif <- arrow::read_parquet(
    paste0("01_data/01_occurrences/06_filter_taxonomy/occ_gbif_part", 
           ifelse(g < 10, paste0("0", g), g), "_pf_time_taxonomy_output.parquet"))
  occ_gbif
  
  ## 7.1 flagging common spatial issues ----
  occ_gbif_space <- bdc::bdc_coordinates_precision(
    data = occ_gbif,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = c(0, 1, 2))
  occ_gbif_space
  
  ## 7.2 flagging bias spatial issues ----
  occ_gbif_space <-
    CoordinateCleaner::clean_coordinates(
      x =  occ_gbif_space,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = "scientificName",
      countries = ,
      tests = c(
        "capitals",     # records within 1km around country and province centroids
        "centroids",    # records within 1km of capitals centroids
        "duplicates",   # duplicated records
        "equal",        # records with equal coordinates
        "gbif",         # records within 1 degree (~111km) of GBIF headsquare
        "institutions", # records within 1km of zoo and herbaria
        "outliers",     # outliers
        "zeros",        # records with coordinates 0,0
        "urban"         # records within urban areas
      ),
      capitals_rad = 1000,
      centroids_rad = 1000,
      centroids_detail = "both", # test both country and province centroids
      inst_rad = 1000, # remove zoo and herbaria within 100m
      outliers_method = "quantile",
      outliers_mtp = 5,
      outliers_td = 500,
      outliers_size = 10,
      range_rad = 0,
      zeros_rad = 0.5,
      capitals_ref = NULL,
      centroids_ref = NULL,
      country_ref = NULL,
      country_refcol = "countryCode",
      inst_ref = NULL,
      range_ref = NULL,
      # seas_ref = continent_border,
      # seas_scale = 110,
      urban_ref = NULL,
      value = "spatialvalid" # result of tests are appended in separate columns
    ) %>% 
    tibble::as_tibble()
  occ_gbif_space
  
  occ_gbif_space$.sea <- sf::st_intersects(
    x = sf::st_as_sf(occ_gbif_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
    y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
    as.logical()
  occ_gbif_space
  
  ## summary ----
  occ_gbif_space <- bdc::bdc_summary_col(data = occ_gbif_space)
  occ_gbif_space
  
  ## report ----
  occ_gbif_space_report <- bdc_create_report(
    data = occ_gbif_space,
    database_id = "database_id",
    workflow_step = "space",
    save_report = FALSE)
  occ_gbif_space_report
  
  ## filter ----  
  occ_gbif_space_output <- occ_gbif_space %>%
    dplyr::filter(.summary == TRUE) %>%
    bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
  occ_gbif_space_output
  
  ## export ----
  arrow::write_parquet(occ_gbif_space_output, 
                       paste0("01_data/01_occurrences/07_filter_spatial/occ_gbif_part",
                              ifelse(g < 10, paste0("0", g), g), "_pf_time_taxonomy_space_output.parquet"))
  arrow::write_parquet(occ_gbif_space_report$x$data, 
                       paste0("01_data/01_occurrences/07_filter_spatial/occ_gbif_part",
                              ifelse(g < 10, paste0("0", g), g), "_pf_time_taxonomy_space_report.parquet"))
  
  
}

# idigbio -----------------------------------------------------------------

# import data
occ_idigbio_files <- dir(path = "01_data/01_occurrences/06_filter_taxonomy", 
                         pattern = "idigbio", full.names = TRUE) %>% 
  stringr::str_subset("output.parquet")
occ_idigbio_files

occ_idigbio <- purrr::map_dfr(occ_idigbio_files, arrow::read_parquet)
occ_idigbio

## 7.1 flagging common spatial issues ----
occ_idigbio_space <- bdc::bdc_coordinates_precision(
  data = occ_idigbio,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_idigbio_space

## 7.2 flagging bias spatial issues ----
occ_idigbio_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_idigbio_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_idigbio_space

occ_idigbio_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_idigbio_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_idigbio_space

## summary ----
occ_idigbio_space <- bdc::bdc_summary_col(data = occ_idigbio_space)
occ_idigbio_space

## report ----
occ_idigbio_space_report <- bdc_create_report(
  data = occ_idigbio_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_idigbio_space_report

## filter ----  
occ_idigbio_space_output <- occ_idigbio_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_idigbio_space_output

## export ----
arrow::write_parquet(occ_idigbio_space_output, "01_data/01_occurrences/07_filter_spatial/occ_idigbio_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_idigbio_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_idigbio_pf_time_taxonomy_space_report.parquet")

# neotroptree -----------------------------------------------------------------

# import data
occ_neotroptree <- arrow::read_parquet("01_data/01_occurrences/06_filter_taxonomy/occ_neotroptree_pf_time_taxonomy_output.parquet")
occ_neotroptree

## 7.1 flagging common spatial issues ----
occ_neotroptree_space <- bdc::bdc_coordinates_precision(
  data = occ_neotroptree,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_neotroptree_space

## 7.2 flagging bias spatial issues ----
occ_neotroptree_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_neotroptree_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_neotroptree_space

occ_neotroptree_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_neotroptree_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_neotroptree_space

## summary ----
occ_neotroptree_space <- bdc::bdc_summary_col(data = occ_neotroptree_space)
occ_neotroptree_space

## report ----
occ_neotroptree_space_report <- bdc_create_report(
  data = occ_neotroptree_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_neotroptree_space_report

## filter ----  
occ_neotroptree_space_output <- occ_neotroptree_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_neotroptree_space_output

## export ----
arrow::write_parquet(occ_neotroptree_space_output, "01_data/01_occurrences/07_filter_spatial/occ_neotroptree_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_neotroptree_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_neotroptree_pf_time_taxonomy_space_report.parquet")


# sibbr -----------------------------------------------------------------

# import data
occ_sibbr <- arrow::read_parquet("01_data/01_occurrences/06_filter_taxonomy/occ_sibbr_pf_time_taxonomy_output.parquet")
occ_sibbr

## 7.1 flagging common spatial issues ----
occ_sibbr_space <- bdc::bdc_coordinates_precision(
  data = occ_sibbr,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_sibbr_space

## 7.2 flagging bias spatial issues ----
occ_sibbr_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_sibbr_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_sibbr_space

occ_sibbr_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_sibbr_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_sibbr_space

## summary ----
occ_sibbr_space <- bdc::bdc_summary_col(data = occ_sibbr_space)
occ_sibbr_space

## report ----
occ_sibbr_space_report <- bdc_create_report(
  data = occ_sibbr_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_sibbr_space_report

## filter ----  
occ_sibbr_space_output <- occ_sibbr_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_sibbr_space_output

## export ----
arrow::write_parquet(occ_sibbr_space_output, "01_data/01_occurrences/07_filter_spatial/occ_sibbr_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_sibbr_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_sibbr_pf_time_taxonomy_space_report.parquet")


# specieslink -----------------------------------------------------------------

# import data
occ_specieslink <- arrow::read_parquet("01_data/01_occurrences/06_filter_taxonomy/occ_specieslink_pf_time_taxonomy_output.parquet")
occ_specieslink

## 7.1 flagging common spatial issues ----
occ_specieslink_space <- bdc::bdc_coordinates_precision(
  data = occ_specieslink,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_specieslink_space

## 7.2 flagging bias spatial issues ----
occ_specieslink_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_specieslink_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_specieslink_space

occ_specieslink_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_specieslink_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_specieslink_space

## summary ----
occ_specieslink_space <- bdc::bdc_summary_col(data = occ_specieslink_space)
occ_specieslink_space

## report ----
occ_specieslink_space_report <- bdc_create_report(
  data = occ_specieslink_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_specieslink_space_report

## filter ----  
occ_specieslink_space_output <- occ_specieslink_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_specieslink_space_output

## export ----
arrow::write_parquet(occ_specieslink_space_output, "01_data/01_occurrences/07_filter_spatial/occ_specieslink_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_specieslink_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_specieslink_pf_time_taxonomy_space_report.parquet")

# jabot -----------------------------------------------------------------

# import data
occ_jabot <- arrow::read_parquet("01_data/01_occurrences/06_filter_taxonomy/occ_jabot_pf_time_taxonomy_output.parquet")
occ_jabot

## 7.1 flagging common spatial issues ----
occ_jabot_space <- bdc::bdc_coordinates_precision(
  data = occ_jabot,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  ndec = c(0, 1, 2))
occ_jabot_space

## 7.2 flagging bias spatial issues ----
occ_jabot_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_jabot_space,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = ,
    tests = c(
      "capitals",     # records within 1km around country and province centroids
      "centroids",    # records within 1km of capitals centroids
      "duplicates",   # duplicated records
      "equal",        # records with equal coordinates
      "gbif",         # records within 1 degree (~111km) of GBIF headsquare
      "institutions", # records within 1km of zoo and herbaria
      "outliers",     # outliers
      "zeros",        # records with coordinates 0,0
      "urban"         # records within urban areas
    ),
    capitals_rad = 1000,
    centroids_rad = 1000,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 1000, # remove zoo and herbaria within 100m
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 500,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    # seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>% 
  tibble::as_tibble()
occ_jabot_space

occ_jabot_space$.sea <- sf::st_intersects(
  x = sf::st_as_sf(occ_jabot_space, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326),
  y = rnaturalearth_ocean_10, sparse = FALSE) %>% 
  as.logical()
occ_jabot_space

## summary ----
occ_jabot_space <- bdc::bdc_summary_col(data = occ_jabot_space)
occ_jabot_space

## report ----
occ_jabot_space_report <- bdc_create_report(
  data = occ_jabot_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_jabot_space_report

## filter ----  
occ_jabot_space_output <- occ_jabot_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_jabot_space_output

## export ----
arrow::write_parquet(occ_jabot_space_output, "01_data/01_occurrences/07_filter_spatial/occ_jabot_pf_time_taxonomy_space_output.parquet")
arrow::write_parquet(occ_jabot_space_report$x$data, "01_data/01_occurrences/07_filter_spatial/occ_jabot_pf_time_taxonomy_space_report.parquet")

# end ---------------------------------------------------------------------