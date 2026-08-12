#' ----
#' title: obj2 - occurrences - species list amazon based on species lists
#' author: mauricio vancine
#' date: 2026-06-22
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)
library(terra)
library(flexsdm)
library(tmap)

# species list ------------------------------------------------------------

## species lists -----
sp_list_domingos_etal_2017 <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_domingos_etal_2017_taxonomy_output.csv") %>% 
  dplyr::select(scientificName) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::mutate(reference = "Domingos et al. (2017)")
sp_list_domingos_etal_2017

sp_list_ter_steege_etal_2013 <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2013_taxonomy_output.csv") %>% 
  dplyr::select(scientificName) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::mutate(reference = "ter Steege et al. (2013)")
sp_list_ter_steege_etal_2013

sp_list_ter_steege_etal_2015 <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2015_taxonomy_output.csv") %>% 
  dplyr::select(scientificName) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::mutate(reference = "ter Steege et al. (2015)")
sp_list_ter_steege_etal_2015

sp_list_ter_steege_etal_2016 <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2016_taxonomy_output.csv") %>% 
  dplyr::select(scientificName) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::mutate(reference = "ter Steege et al. (2016)")
sp_list_ter_steege_etal_2016

sp_list_ter_steege_etal_2019a <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019a_taxonomy_output.csv") %>% 
  dplyr::select(scientificName) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::mutate(reference = "ter Steege et al. (2019a)")
sp_list_ter_steege_etal_2019a

sp_list_ter_steege_etal_2019b <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019b_taxonomy_output.csv") %>% 
  dplyr::select(scientificName) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::mutate(reference = "ter Steege et al. (2019b)")
sp_list_ter_steege_etal_2019b

sp_list_ter_steege_etal_2020 <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2020_taxonomy_output.csv") %>% 
  dplyr::select(scientificName) %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::mutate(reference = "ter Steege et al. (2020)")
sp_list_ter_steege_etal_2020

## species list ----
sp_list_literature <- dplyr::bind_rows(
  sp_list_domingos_etal_2017,
  sp_list_ter_steege_etal_2013,
  sp_list_ter_steege_etal_2015,
  sp_list_ter_steege_etal_2016,
  sp_list_ter_steege_etal_2019a,
  sp_list_ter_steege_etal_2019b,
  sp_list_ter_steege_etal_2020) %>% 
  dplyr::mutate(value = 1) %>% 
  tidyr::pivot_wider(names_from = reference, values_from = value, values_fn = sum) %>% 
  dplyr::mutate(across(c(
    `Domingos et al. (2017)`, 
    `ter Steege et al. (2013)`, 
    `ter Steege et al. (2015)`, 
    `ter Steege et al. (2016)`, 
    `ter Steege et al. (2019a)`, 
    `ter Steege et al. (2019b)`,
    `ter Steege et al. (2020)`),
    ~ case_when(is.na(.) ~ 0, . > 1 ~ 1, TRUE ~ .)))
sp_list_literature

# occurrences --------------------------------------------------------------

## import
occ <- arrow::read_parquet("01_data/01_occurrences/08_integrated/occ_integrated_pf_time_taxonomy_space_output.parquet")
occ

## amazon limit ----
limit_amazon <- sf::st_read("01_data/02_variables/00_limits/amazon/am_limit_raisg/LimRAISG.shp") %>% 
  sf::st_transform(4326)
limit_amazon

tm_shape(limit_amazon) +
  tm_polygons()

# filter ------------------------------------------------------------------

# filter
occ_filter_sp_list_amazon <- occ %>% 
  dplyr::filter(scientificName %in% sp_list_literature$scientificName) %>% 
  dplyr::add_count(scientificName, name = "n_occ")
occ_filter_sp_list_amazon
occ_filter_sp_list_amazon %>% count(scientificName, sort = TRUE)

## raster id ----
var_id <- terra::rast("01_data/02_variables/02_soil/bdod_mean_neo_10km.tif")
var_id[!is.na(var_id)] <- as.data.frame(var_id, cells = TRUE)[, 1]
names(var_id) <- "ncell"
var_id
plot(var_id)

## oppc ----
occ_filter_sp_list_amazon_oppc <- occ_filter_sp_list_amazon %>% 
  flexsdm::sdm_extract(x = "decimalLongitude", y = "decimalLatitude", 
                       env_layer = var_id, filter_na = FALSE) 
occ_filter_sp_list_amazon_oppc

occ_filter_sp_list_amazon_oppc_filtered <- occ_filter_sp_list_amazon_oppc %>% 
  tidyr::drop_na(ncell) %>% 
  dplyr::distinct(scientificName, ncell, .keep_all = TRUE) %>% 
  dplyr::group_by(scientificName) %>% 
  dplyr::mutate(oppc_n = n()) %>% 
  dplyr::ungroup()
occ_filter_sp_list_amazon_oppc_filtered
occ_filter_sp_list_amazon_oppc_filtered %>% count(scientificName, sort = TRUE)

# amazon ------------------------------------------------------------------

## count occs inside amazon ----
occ_filter_sp_list_amazon_oppc_filtered_sf <- occ_filter_sp_list_amazon_oppc_filtered %>% 
  dplyr::mutate(lon = decimalLongitude, lat = decimalLatitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_filter_sp_list_amazon_oppc_filtered_sf

occ_filter_sp_list_amazon_oppc_filtered_inside_amazon <- occ_filter_sp_list_amazon_oppc_filtered_sf %>% 
  sf::st_intersects(limit_amazon, sparse = FALSE) %>% 
  as.numeric()
occ_filter_sp_list_amazon_oppc_filtered_inside_amazon

## add column and percentage ----
occ_filter_sp_list_amazon_oppc_filtered_amazon <- occ_filter_sp_list_amazon_oppc_filtered_sf %>%
  sf::st_drop_geometry() %>% 
  dplyr::mutate(amazon = occ_filter_sp_list_amazon_oppc_filtered_inside_amazon) %>% 
  dplyr::group_by(scientificName) %>%
  dplyr::mutate(
    amazon_n = sum(amazon, na.rm = TRUE),
    amazon_per = round(mean(amazon, na.rm = TRUE) * 100, 2)) %>% 
  dplyr::ungroup()
occ_filter_sp_list_amazon_oppc_filtered_amazon

# common and rare species -------------------------------------------------

## common and rare species ----
occ_filter_sp_list_amazon_oppc_filtered_amazon_model <- occ_filter_sp_list_amazon_oppc_filtered_amazon %>% 
  dplyr::filter(oppc_n >= 15)
occ_filter_sp_list_amazon_oppc_filtered_amazon_model

occ_filter_sp_list_amazon_oppc_filtered_amazon_model %>% count(scientificName, sort = TRUE)
occ_filter_sp_list_amazon_oppc_filtered_amazon_model %>% count(genus, sort = TRUE)

occ_filter_sp_list_amazon_oppc_filtered_amazon_common <- occ_filter_sp_list_amazon_oppc_filtered_amazon %>% 
  dplyr::filter(oppc_n >= 50)
occ_filter_sp_list_amazon_oppc_filtered_amazon_common
occ_filter_sp_list_amazon_oppc_filtered_amazon_common %>% count(scientificName, sort = TRUE)

occ_filter_sp_list_amazon_oppc_filtered_amazon_rare <- occ_filter_sp_list_amazon_oppc_filtered_amazon %>% 
  dplyr::filter(oppc_n >= 15 & oppc_n < 49)
occ_filter_sp_list_amazon_oppc_filtered_amazon_rare
occ_filter_sp_list_amazon_oppc_filtered_amazon_rare %>% count(scientificName, sort = TRUE)

## species list ----
occ_filter_sp_list_amazon_oppc_filtered_amazon_model_species_list <- occ_filter_sp_list_amazon_oppc_filtered_amazon_model %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::distinct(scientificName)
occ_filter_sp_list_amazon_oppc_filtered_amazon_model_species_list

# export ------------------------------------------------------------------

## export ----
vroom::vroom_write(occ_filter_sp_list_amazon_oppc_filtered_amazon_model_species_list, "01_data/01_occurrences/09_species_list_amazon/species_list_amazon_species_lists_oppc_model.csv")

arrow::write_parquet(occ_filter_sp_list_amazon, "01_data/01_occurrences/10_occ_list_amazon/occ_amazon.parquet")
arrow::write_parquet(occ_filter_sp_list_amazon_oppc_filtered, "01_data/01_occurrences/10_occ_list_amazon/occ_amazon_oppc_10km.parquet")

arrow::write_parquet(occ_filter_sp_list_amazon_oppc_filtered_amazon_model, "01_data/01_occurrences/10_occ_list_amazon/occ_amazon_oppc_10km_model.parquet")
arrow::write_parquet(occ_filter_sp_list_amazon_oppc_filtered_amazon_common, "01_data/01_occurrences/10_occ_list_amazon/occ_amazon_oppc_10km_common.parquet")
arrow::write_parquet(occ_filter_sp_list_amazon_oppc_filtered_amazon_rare, "01_data/01_occurrences/10_occ_list_amazon/occ_amazon_oppc_10km_rare.parquet")

# end ---------------------------------------------------------------------
