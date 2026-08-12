#' ----
#' title: obj2 - occurrences - integrated
#' author: mauricio vancine
#' date: 2026-04-13
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)
library(CoordinateCleaner)

# import data ------------------------------------------------------------

## atlantic epiphytes ----
occ_atlantic_epiphytes <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_atlantic_epiphytes_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_atlantic_epiphytes

## bien ----
occ_bien <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_bien_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_bien

## dryflor ----
occ_dryflor <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_dryflor_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_dryflor

## gbif ----
occ_gbif <- NULL
# for(g in 0:64){
# 
#   print(g)
# 
#   # import data
#   occ_gbif_i <- arrow::read_parquet(
#     paste0("01_data/01_occurrences/07_filter_spatial/occ_gbif_part",
#            ifelse(g < 10, paste0("0", g), g), "_pf_time_taxonomy_space_output.parquet")) %>%
#     dplyr::mutate(verbatimEventDate = as.character(verbatimEventDate))
# 
#   # bind
#   occ_gbif <- dplyr::bind_rows(occ_gbif, occ_gbif_i)
# 
# }

occ_gbif <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_gbif_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_gbif

## idigbio ----
occ_idigbio <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_idigbio_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_idigbio

## neotroptree ----
occ_neotroptree <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_neotroptree_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_neotroptree

## sibbr ----
occ_sibbr <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_sibbr_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_sibbr

## specieslink ----
occ_specieslink <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_specieslink_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_specieslink

## jabot ----
occ_jabot <- arrow::read_parquet("01_data/01_occurrences/07_filter_spatial/occ_jabot_pf_time_taxonomy_space_output.parquet") %>% 
  dplyr::mutate(occurrenceID = as.character(occurrenceID),
                verbatimEventDate = as.character(verbatimEventDate),
                coordinatePrecision = as.character(coordinatePrecision))
occ_jabot

# integrate -----------------------------------------------------------------

## integrate
occ_integrated <- dplyr::bind_rows(
  occ_atlantic_epiphytes,
  occ_bien,
  occ_dryflor,
  occ_gbif,
  occ_idigbio,
  occ_neotroptree,
  occ_sibbr,
  occ_specieslink,
  occ_jabot)
occ_integrated

## export 
arrow::write_parquet(occ_integrated, "01_data/01_occurrences/08_integrated/occ_integrated_pf_time_taxonomy_space_integrated.parquet")

## remove
rm(
  occ_atlantic_epiphytes,
  occ_bien,
  occ_dryflor,
  occ_gbif,
  occ_idigbio,
  occ_neotroptree,
  occ_sibbr,
  occ_specieslink,
  occ_jabot)

# clean -------------------------------------------------------------------

## import ----
occ_integrated <- arrow::read_parquet("01_data/01_occurrences/08_integrated/occ_integrated_pf_time_taxonomy_space_integrated.parquet")
occ_integrated

## flagging bias spatial issues ----
occ_integrated_space <-
  CoordinateCleaner::clean_coordinates(
    x =  occ_integrated,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = "duplicates") %>% 
  tibble::as_tibble()
occ_integrated_space

## summary ----
occ_integrated_space <- bdc::bdc_summary_col(data = occ_integrated_space)
occ_integrated_space

## report ----
occ_integrated_space_report <- bdc::bdc_create_report(
  data = occ_integrated_space,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_integrated_space_report

## filter ----  
occ_integrated_space_output <- occ_integrated_space %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_integrated_space_output

## spatial
occ_integrated_space_output_sf <- sf::st_as_sf(occ_integrated_space_output, 
                                               coords = c("decimalLongitude", "decimalLatitude"),
                                               crs = 4326)

## export ----
arrow::write_parquet(occ_integrated_space_output, "01_data/01_occurrences/08_integrated/occ_integrated_pf_time_taxonomy_space_integrated_output.parquet")
arrow::write_parquet(occ_integrated_space_report$x$data, "01_data/01_occurrences/08_integrated/occ_integrated_pf_time_taxonomy_space_integrated_report.parquet")

sf::st_write(occ_integrated_space_output_sf, "01_data/01_occurrences/08_integrated/occ_integrated_pf_time_taxonomy_space_integrated_output.gpkg")

# end ---------------------------------------------------------------------