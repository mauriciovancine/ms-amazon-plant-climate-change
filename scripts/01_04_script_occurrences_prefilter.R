#' ----
#' title: obj2 - occurrences - prefilter data
#' author: mauricio vancine
#' date: 2026-04-13
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(data.table)
library(arrow)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires) # remotes::install_github("ropensci/rnaturalearthhires")
library(rworldmap)
library(bdc)

# options
sf::sf_use_s2(FALSE)

# 2. pre-filter -----------------------------------------------------------

# ## limit ----
# lim <- sf::st_read("01_data/02_variables/00_limits/neotropic/neotropic_dissolved_fill_holes.shp")
# lim
# plot(lim$geometry)
# 
# ## countries ----
# ct <- countries10
# ct
# 
# ## coutries limit ----
# ct_lim <- ct[lim, ]
# ct_lim
# plot(ct_lim$geometry)
# 
# ct_lim_names <- sort(ct_lim$ADMIN)
# ct_lim_names

# atlantic epiphytes ------------------------------------------------------

## import database ----
occ_atlantic_epiphytes <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_atlantic_epiphytes_stand.parquet")
occ_atlantic_epiphytes

## round coordinates ----
occ_atlantic_epiphytes_round <- occ_atlantic_epiphytes %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_atlantic_epiphytes_round

## 2.1 records missing species names ----
occ_atlantic_epiphytes_pf <- bdc::bdc_scientificName_empty(
  data = occ_atlantic_epiphytes_round, 
  sci_name = "scientificName")
occ_atlantic_epiphytes_pf

## 2.2 records lacking information on geographic coordinates ----
occ_atlantic_epiphytes_pf <- bdc::bdc_coordinates_empty(
  data = occ_atlantic_epiphytes_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_atlantic_epiphytes_pf

## 2.3 records with out-of-range coordinates
occ_atlantic_epiphytes_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_atlantic_epiphytes_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_atlantic_epiphytes_pf

## 2.4 records from doubtful sources ----
occ_atlantic_epiphytes_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_atlantic_epiphytes_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_atlantic_epiphytes_pf

## duplicates ----
occ_atlantic_epiphytes_pf <- occ_atlantic_epiphytes_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_atlantic_epiphytes_pf

## 2.5 getting country names from valid coordinates ----
# occ_atlantic_epiphytes_pf <- bdc::bdc_country_from_coordinates(
#   data = occ_atlantic_epiphytes_pf,
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country")
# occ_atlantic_epiphytes_pf

## 2.6 standardizing country names and getting country code information ----
# occ_atlantic_epiphytes_pf <- bdc::bdc_country_standardized(
#   data = occ_atlantic_epiphytes_pf,
#   country = "country")
# occ_atlantic_epiphytes_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_atlantic_epiphytes_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_atlantic_epiphytes_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_atlantic_epiphytes_pf

## 2.8 records outside a region of interest ----
# occ_atlantic_epiphytes_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_atlantic_epiphytes_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_atlantic_epiphytes_pf

## summary ----
occ_atlantic_epiphytes_pf <- bdc::bdc_summary_col(data = occ_atlantic_epiphytes_pf)
occ_atlantic_epiphytes_pf

## report ----
occ_atlantic_epiphytes_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_atlantic_epiphytes_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_atlantic_epiphytes_pf_report_prefilter$x$data

occ_atlantic_epiphytes_pf_report_space <- bdc::bdc_create_report(
  data = occ_atlantic_epiphytes_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_atlantic_epiphytes_pf_report_space$x$data

occ_atlantic_epiphytes_pf_report <- rbind(
  occ_atlantic_epiphytes_pf_report_prefilter$x$data[1:4,],
  occ_atlantic_epiphytes_pf_report_space$x$data[1,],
  occ_atlantic_epiphytes_pf_report_prefilter$x$data[7,])
occ_atlantic_epiphytes_pf_report

## filtering ----
occ_atlantic_epiphytes_pf_output <- occ_atlantic_epiphytes_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_atlantic_epiphytes_pf_output

## export ----
arrow::write_parquet(occ_atlantic_epiphytes_pf_output, "01_data/01_occurrences/04_prefilter/occ_atlantic_epiphytes_pf_output.parquet")
arrow::write_parquet(occ_atlantic_epiphytes_pf_report, "01_data/01_occurrences/04_prefilter/occ_atlantic_epiphytes_pf_report.parquet")


# bien --------------------------------------------------------------------

## import database ----
occ_bien <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_bien_stand.parquet")
occ_bien

## round coordinates ----
occ_bien_round <- occ_bien %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_bien_round

## 2.1 records missing species names ----
occ_bien_pf <- bdc::bdc_scientificName_empty(
  data = occ_bien_round, 
  sci_name = "scientificName")
occ_bien_pf

## 2.2 records lacking information on geographic coordinates ----
occ_bien_pf <- bdc::bdc_coordinates_empty(
  data = occ_bien_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_bien_pf

## 2.3 records with out-of-range coordinates
occ_bien_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_bien_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_bien_pf

## 2.4 records from doubtful sources ----
occ_bien_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_bien_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_bien_pf

## duplicates ----
occ_bien_pf <- occ_bien_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_bien_pf

## 2.5 getting country names from valid coordinates ----
# occ_bien_pf <- bdc::bdc_country_from_coordinates(
#   data = occ_bien_pf,
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country")
# occ_bien_pf

## 2.6 standardizing country names and getting country code information ----
# occ_bien_pf <- bdc::bdc_country_standardized(
#   data = occ_bien_pf,
#   country = "country")
# occ_bien_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_bien_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_bien_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_bien_pf

## 2.8 records outside a region of interest ----
# occ_bien_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_bien_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_bien_pf

## summary ----
occ_bien_pf <- bdc::bdc_summary_col(data = occ_bien_pf)
occ_bien_pf

## report ----
occ_bien_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_bien_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_bien_pf_report_prefilter$x$data

occ_bien_pf_report_space <- bdc::bdc_create_report(
  data = occ_bien_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_bien_pf_report_space$x$data

occ_bien_pf_report <- rbind(
  occ_bien_pf_report_prefilter$x$data[1:4,],
  occ_bien_pf_report_space$x$data[1,],
  occ_bien_pf_report_prefilter$x$data[7,])
occ_bien_pf_report

## filtering ----
occ_bien_pf_output <- occ_bien_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_bien_pf_output

## export ----
arrow::write_parquet(occ_bien_pf_output, "01_data/01_occurrences/04_prefilter/occ_bien_pf_output.parquet")
arrow::write_parquet(occ_bien_pf_report, "01_data/01_occurrences/04_prefilter/occ_bien_pf_report.parquet")


# gbif --------------------------------------------------------------------

for(g in 0:64){
  
  print(g)
  
  ## import database ----
  occ_gbif <- arrow::read_parquet(
  paste0("01_data/01_occurrences/03_standardization/occ_gbif_part",
         ifelse(g < 10, paste0("0", g), g), "_stand.parquet"))
  occ_gbif
  
  ## round coordinates ----
  occ_gbif_round <- occ_gbif %>% 
    dplyr::mutate(
      decimalLongitude = round(decimalLongitude, 4),
      decimalLatitude = round(decimalLatitude, 4))
  occ_gbif_round
  
  ## 2.1 records missing species names ----
  occ_gbif_pf <- bdc::bdc_scientificName_empty(
    data = occ_gbif_round, 
    sci_name = "scientificName")
  occ_gbif_pf
  
  ## 2.2 records lacking information on geographic coordinates ----
  occ_gbif_pf <- bdc::bdc_coordinates_empty(
    data = occ_gbif_pf,
    lat = "decimalLatitude",
    lon = "decimalLongitude")
  occ_gbif_pf
  
  ## 2.3 records with out-of-range coordinates
  occ_gbif_pf <- bdc::bdc_coordinates_outOfRange(
    data = occ_gbif_pf,
    lat = "decimalLatitude",
    lon = "decimalLongitude")
  occ_gbif_pf
  
  ## 2.4 records from doubtful sources ----
  occ_gbif_pf <- bdc::bdc_basisOfRecords_notStandard(
    data = occ_gbif_pf,
    basisOfRecord = "basisOfRecord",
    names_to_keep = "all")
  occ_gbif_pf
  
  ## duplicates ----
  occ_gbif_pf <- occ_gbif_pf %>% 
    dplyr::mutate(
      .dpl = !duplicated(
        dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
  occ_gbif_pf
  
  ## 2.5 getting country names from valid coordinates ----
  # occ_gbif_pf <- bdc::bdc_country_from_coordinates(
  #   data = occ_gbif_pf,
  #   lat = "decimalLatitude",
  #   lon = "decimalLongitude",
  #   country = "country")
  # occ_gbif_pf
  
  ## 2.6 standardizing country names and getting country code information ----
  # occ_gbif_pf <- bdc::bdc_country_standardized(
  #   data = occ_gbif_pf,
  #   country = "country")
  # occ_gbif_pf
  # 
  ## 2.7 correcting latitude and longitude transposed ----
  # occ_gbif_pf <- bdc::bdc_coordinates_transposed(
  #   data = occ_gbif_pf,
  #   id = "database_id",
  #   sci_names = "scientificName",
  #   lat = "decimalLatitude",
  #   lon = "decimalLongitude",
  #   country = "country_suggested",
  #   countryCode = "countryCode",
  #   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
  #   save_outputs = FALSE)
  # occ_gbif_pf
  
  ## 2.8 records outside a region of interest ----
  # occ_gbif_pf <- bdc::bdc_coordinates_country_inconsistent(
  #   data = occ_gbif_pf,
  #   country_name = ct_lim_names,
  #   country = "country_suggested",
  #   lon = "decimalLongitude",
  #   lat = "decimalLatitude",
  #   dist = 0.1) # in decimal degrees (~11 km at the equator)
  # occ_gbif_pf
  
  ## summary ----
  occ_gbif_pf <- bdc::bdc_summary_col(data = occ_gbif_pf)
  occ_gbif_pf
  
  ## report ----
  occ_gbif_pf_report_prefilter <- bdc::bdc_create_report(
    data = occ_gbif_pf,
    database_id = "database_id",
    workflow_step = "prefilter",
    save_report = FALSE)
  occ_gbif_pf_report_prefilter$x$data
  
  occ_gbif_pf_report_space <- bdc::bdc_create_report(
    data = occ_gbif_pf,
    database_id = "database_id",
    workflow_step = "space",
    save_report = FALSE)
  occ_gbif_pf_report_space$x$data
  
  occ_gbif_pf_report <- rbind(
    occ_gbif_pf_report_prefilter$x$data[1:4,],
    occ_gbif_pf_report_space$x$data[1,],
    occ_gbif_pf_report_prefilter$x$data[7,])
  occ_gbif_pf_report
  
  ## filtering ----
  occ_gbif_pf_output <- occ_gbif_pf %>%
    dplyr::filter(.summary == TRUE) %>%
    bdc::bdc_filter_out_flags(
      data = ., 
      col_to_remove = "all")
  occ_gbif_pf_output
  
  ## export ----
  arrow::write_parquet(occ_gbif_pf_output, paste0("01_data/01_occurrences/04_prefilter/occ_gbif_part",
                                                 ifelse(g < 10, paste0("0", g), g), "_pf_output.parquet")) 
  arrow::write_parquet(occ_gbif_pf_report, paste0("01_data/01_occurrences/04_prefilter/occ_gbif_part",
                                                         ifelse(g < 10, paste0("0", g), g), "_pf_report.parquet")) 
                       
}

# dryflor --------------------------------------------------------------------

## import database ----
occ_dryflor <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_dryflor_stand.parquet")
occ_dryflor

## round coordinates ----
occ_dryflor_round <- occ_dryflor %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_dryflor_round

## 2.1 records missing species names ----
occ_dryflor_pf <- bdc::bdc_scientificName_empty(
  data = occ_dryflor_round, 
  sci_name = "scientificName")
occ_dryflor_pf

## 2.2 records lacking information on geographic coordinates ----
occ_dryflor_pf <- bdc::bdc_coordinates_empty(
  data = occ_dryflor_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_dryflor_pf

## 2.3 records with out-of-range coordinates
occ_dryflor_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_dryflor_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_dryflor_pf

## 2.4 records from doubtful sources ----
occ_dryflor_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_dryflor_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_dryflor_pf

## duplicates ----
occ_dryflor_pf <- occ_dryflor_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_dryflor_pf

## 2.5 getting country names from valid coordinates ----
# occ_dryflor_pf <- bdc::bdc_country_from_coordinates(
#   data = occ_dryflor_pf,
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country")
# occ_dryflor_pf

## 2.6 standardizing country names and getting country code information ----
# occ_dryflor_pf <- bdc::bdc_country_standardized(
#   data = occ_dryflor_pf,
#   country = "country")
# occ_dryflor_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_dryflor_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_dryflor_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_dryflor_pf

## 2.8 records outside a region of interest ----
# occ_dryflor_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_dryflor_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_dryflor_pf

## summary ----
occ_dryflor_pf <- bdc::bdc_summary_col(data = occ_dryflor_pf)
occ_dryflor_pf

## report ----
occ_dryflor_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_dryflor_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_dryflor_pf_report_prefilter$x$data

occ_dryflor_pf_report_space <- bdc::bdc_create_report(
  data = occ_dryflor_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_dryflor_pf_report_space$x$data

occ_dryflor_pf_report <- rbind(
  occ_dryflor_pf_report_prefilter$x$data[1:4,],
  occ_dryflor_pf_report_space$x$data[1,],
  occ_dryflor_pf_report_prefilter$x$data[7,])
occ_dryflor_pf_report

## filtering ----
occ_dryflor_pf_output <- occ_dryflor_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_dryflor_pf_output

## export ----
arrow::write_parquet(occ_dryflor_pf_output, "01_data/01_occurrences/04_prefilter/occ_dryflor_pf_output.parquet")
arrow::write_parquet(occ_dryflor_pf_report, "01_data/01_occurrences/04_prefilter/occ_dryflor_pf_report.parquet")

# idigbio --------------------------------------------------------------------

## import database ----
occ_idigbio <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_idigbio_stand.parquet")
occ_idigbio

## round coordinates ----
occ_idigbio_round <- occ_idigbio %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_idigbio_round

## 2.1 records missing species names ----
occ_idigbio_pf <- bdc::bdc_scientificName_empty(
  data = occ_idigbio_round, 
  sci_name = "scientificName")
occ_idigbio_pf

## 2.2 records lacking information on geographic coordinates ----
occ_idigbio_pf <- bdc::bdc_coordinates_empty(
  data = occ_idigbio_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_idigbio_pf

## 2.3 records with out-of-range coordinates
occ_idigbio_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_idigbio_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_idigbio_pf

## 2.4 records from doubtful sources ----
occ_idigbio_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_idigbio_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_idigbio_pf

## duplicates ----
occ_idigbio_pf <- occ_idigbio_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_idigbio_pf

## 2.5 getting country names from valid coordinates ----
# occ_idigbio_pf <- bdc::bdc_country_from_coordinates(
#   data = occ_idigbio_pf,
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country")
# occ_idigbio_pf

## 2.6 standardizing country names and getting country code information ----
# occ_idigbio_pf <- bdc::bdc_country_standardized(
#   data = occ_idigbio_pf,
#   country = "country")
# occ_idigbio_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_idigbio_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_idigbio_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_idigbio_pf

## 2.8 records outside a region of interest ----
# occ_idigbio_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_idigbio_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_idigbio_pf

## summary ----
occ_idigbio_pf <- bdc::bdc_summary_col(data = occ_idigbio_pf)
occ_idigbio_pf

## report ----
occ_idigbio_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_idigbio_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_idigbio_pf_report_prefilter$x$data

occ_idigbio_pf_report_space <- bdc::bdc_create_report(
  data = occ_idigbio_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_idigbio_pf_report_space$x$data

occ_idigbio_pf_report <- rbind(
  occ_idigbio_pf_report_prefilter$x$data[1:4,],
  occ_idigbio_pf_report_space$x$data[1,],
  occ_idigbio_pf_report_prefilter$x$data[7,])
occ_idigbio_pf_report

## filtering ----
occ_idigbio_pf_output <- occ_idigbio_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_idigbio_pf_output

## export ----
arrow::write_parquet(occ_idigbio_pf_output, "01_data/01_occurrences/04_prefilter/occ_idigbio_pf_output.parquet")
arrow::write_parquet(occ_idigbio_pf_report, "01_data/01_occurrences/04_prefilter/occ_idigbio_pf_report.parquet")

# jabot --------------------------------------------------------------------

## import database ----
occ_jabot <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_jabot_stand.parquet")
occ_jabot

## round coordinates ----
occ_jabot_round <- occ_jabot %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_jabot_round

## 2.1 records missing species names ----
occ_jabot_pf <- bdc::bdc_scientificName_empty(
  data = occ_jabot_round, 
  sci_name = "scientificName")
occ_jabot_pf

## 2.2 records lacking information on geographic coordinates ----
occ_jabot_pf <- bdc::bdc_coordinates_empty(
  data = occ_jabot_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_jabot_pf

## 2.3 records with out-of-range coordinates
occ_jabot_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_jabot_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_jabot_pf

## 2.4 records from doubtful sources ----
occ_jabot_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_jabot_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_jabot_pf

## duplicates ----
occ_jabot_pf <- occ_jabot_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_jabot_pf

## 2.5 getting country names from valid coordinates ----
# occ_jabot_pf <- bdc::bdc_country_from_coordinates(
#   data = occ_jabot_pf,
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country")
# occ_jabot_pf

## 2.6 standardizing country names and getting country code information ----
# occ_jabot_pf <- bdc::bdc_country_standardized(
#   data = occ_jabot_pf,
#   country = "country")
# occ_jabot_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_jabot_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_jabot_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_jabot_pf

## 2.8 records outside a region of interest ----
# occ_jabot_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_jabot_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_jabot_pf

## summary ----
occ_jabot_pf <- bdc::bdc_summary_col(data = occ_jabot_pf)
occ_jabot_pf

## report ----
occ_jabot_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_jabot_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_jabot_pf_report_prefilter$x$data

occ_jabot_pf_report_space <- bdc::bdc_create_report(
  data = occ_jabot_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_jabot_pf_report_space$x$data

occ_jabot_pf_report <- rbind(
  occ_jabot_pf_report_prefilter$x$data[1:4,],
  occ_jabot_pf_report_space$x$data[1,],
  occ_jabot_pf_report_prefilter$x$data[7,])
occ_jabot_pf_report

## filtering ----
occ_jabot_pf_output <- occ_jabot_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_jabot_pf_output

## export ----
arrow::write_parquet(occ_jabot_pf_output, "01_data/01_occurrences/04_prefilter/occ_jabot_pf_output.parquet")
arrow::write_parquet(occ_jabot_pf_report, "01_data/01_occurrences/04_prefilter/occ_jabot_pf_report.parquet")

# neotroptree --------------------------------------------------------------------

## import database ----
occ_neotroptree <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_neotroptree_stand.parquet")
occ_neotroptree

## round coordinates ----
occ_neotroptree_round <- occ_neotroptree %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_neotroptree_round

## 2.1 records missing species names ----
occ_neotroptree_pf <- bdc::bdc_scientificName_empty(
  data = occ_neotroptree_round, 
  sci_name = "scientificName")
occ_neotroptree_pf

## 2.2 records lacking information on geographic coordinates ----
occ_neotroptree_pf <- bdc::bdc_coordinates_empty(
  data = occ_neotroptree_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_neotroptree_pf

## 2.3 records with out-of-range coordinates
occ_neotroptree_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_neotroptree_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_neotroptree_pf

## 2.4 records from doubtful sources ----
occ_neotroptree_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_neotroptree_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_neotroptree_pf

## 2.5 getting country names from valid coordinates ----
occ_neotroptree_pf <- bdc::bdc_country_from_coordinates(
  data = occ_neotroptree_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country")
occ_neotroptree_pf

## duplicates ----
occ_neotroptree_pf <- occ_neotroptree_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_neotroptree_pf

## 2.6 standardizing country names and getting country code information ----
# occ_neotroptree_pf <- bdc::bdc_country_standardized(
#   data = occ_neotroptree_pf,
#   country = "country")
# occ_neotroptree_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_neotroptree_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_neotroptree_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_neotroptree_pf

## 2.8 records outside a region of interest ----
# occ_neotroptree_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_neotroptree_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_neotroptree_pf

## summary ----
occ_neotroptree_pf <- bdc::bdc_summary_col(data = occ_neotroptree_pf)
occ_neotroptree_pf

## report ----
occ_neotroptree_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_neotroptree_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_neotroptree_pf_report_prefilter$x$data

occ_neotroptree_pf_report_space <- bdc::bdc_create_report(
  data = occ_neotroptree_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_neotroptree_pf_report_space$x$data

occ_neotroptree_pf_report <- rbind(
  occ_neotroptree_pf_report_prefilter$x$data[1:4,],
  occ_neotroptree_pf_report_space$x$data[1,],
  occ_neotroptree_pf_report_prefilter$x$data[7,])
occ_neotroptree_pf_report

## filtering ----
occ_neotroptree_pf_output <- occ_neotroptree_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_neotroptree_pf_output

## export ----
arrow::write_parquet(occ_neotroptree_pf_output, "01_data/01_occurrences/04_prefilter/occ_neotroptree_pf_output.parquet")
arrow::write_parquet(occ_neotroptree_pf_report, "01_data/01_occurrences/04_prefilter/occ_neotroptree_pf_report.parquet")


# sibbr --------------------------------------------------------------------

## import database ----
occ_sibbr <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_sibbr_stand.parquet")
occ_sibbr

## round coordinates ----
occ_sibbr_round <- occ_sibbr %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_sibbr_round

## 2.1 records missing species names ----
occ_sibbr_pf <- bdc::bdc_scientificName_empty(
  data = occ_sibbr_round, 
  sci_name = "scientificName")
occ_sibbr_pf

## 2.2 records lacking information on geographic coordinates ----
occ_sibbr_pf <- bdc::bdc_coordinates_empty(
  data = occ_sibbr_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_sibbr_pf

## 2.3 records with out-of-range coordinates
occ_sibbr_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_sibbr_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_sibbr_pf

## 2.4 records from doubtful sources ----
occ_sibbr_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_sibbr_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_sibbr_pf

## duplicates ----
occ_sibbr_pf <- occ_sibbr_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_sibbr_pf

## 2.5 getting country names from valid coordinates ----
# occ_sibbr_pf <- bdc::bdc_country_from_coordinates(
#   data = occ_sibbr_pf,
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country")
# occ_sibbr_pf

## 2.6 standardizing country names and getting country code information ----
# occ_sibbr_pf <- bdc::bdc_country_standardized(
#   data = occ_sibbr_pf,
#   country = "country")
# occ_sibbr_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_sibbr_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_sibbr_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_sibbr_pf

## 2.8 records outside a region of interest ----
# occ_sibbr_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_sibbr_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_sibbr_pf

## summary ----
occ_sibbr_pf <- bdc::bdc_summary_col(data = occ_sibbr_pf)
occ_sibbr_pf

## report ----
occ_sibbr_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_sibbr_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_sibbr_pf_report_prefilter$x$data

occ_sibbr_pf_report_space <- bdc::bdc_create_report(
  data = occ_sibbr_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_sibbr_pf_report_space$x$data

occ_sibbr_pf_report <- rbind(
  occ_sibbr_pf_report_prefilter$x$data[1:4,],
  occ_sibbr_pf_report_space$x$data[1,],
  occ_sibbr_pf_report_prefilter$x$data[7,])
occ_sibbr_pf_report

## filtering ----
occ_sibbr_pf_output <- occ_sibbr_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_sibbr_pf_output

## export ----
arrow::write_parquet(occ_sibbr_pf_output, "01_data/01_occurrences/04_prefilter/occ_sibbr_pf_output.parquet")
arrow::write_parquet(occ_sibbr_pf_report, "01_data/01_occurrences/04_prefilter/occ_sibbr_pf_report.parquet")


# specieslink --------------------------------------------------------------------

## import database ----
occ_specieslink <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_specieslink_stand.parquet")
occ_specieslink

## round coordinates ----
occ_specieslink_round <- occ_specieslink %>% 
  dplyr::mutate(
    decimalLongitude = round(decimalLongitude, 4),
    decimalLatitude = round(decimalLatitude, 4))
occ_specieslink_round

## 2.1 records missing species names ----
occ_specieslink_pf <- bdc::bdc_scientificName_empty(
  data = occ_specieslink_round, 
  sci_name = "scientificName")
occ_specieslink_pf

## 2.2 records lacking information on geographic coordinates ----
occ_specieslink_pf <- bdc::bdc_coordinates_empty(
  data = occ_specieslink_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_specieslink_pf

## 2.3 records with out-of-range coordinates
occ_specieslink_pf <- bdc::bdc_coordinates_outOfRange(
  data = occ_specieslink_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")
occ_specieslink_pf

## 2.4 records from doubtful sources ----
occ_specieslink_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = occ_specieslink_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = "all")
occ_specieslink_pf

## duplicates ----
occ_specieslink_pf <- occ_specieslink_pf %>% 
  dplyr::mutate(
    .dpl = !duplicated(
      dplyr::select(., scientificName, decimalLongitude, decimalLatitude)))
occ_specieslink_pf

## 2.5 getting country names from valid coordinates ----
# occ_specieslink_pf <- bdc::bdc_country_from_coordinates(
#   data = occ_specieslink_pf,
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country")
# occ_specieslink_pf

## 2.6 standardizing country names and getting country code information ----
# occ_specieslink_pf <- bdc::bdc_country_standardized(
#   data = occ_specieslink_pf,
#   country = "country")
# occ_specieslink_pf

## 2.7 correcting latitude and longitude transposed ----
# occ_specieslink_pf <- bdc::bdc_coordinates_transposed(
#   data = occ_specieslink_pf,
#   id = "database_id",
#   sci_names = "scientificName",
#   lat = "decimalLatitude",
#   lon = "decimalLongitude",
#   country = "country_suggested",
#   countryCode = "countryCode",
#   border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
#   save_outputs = FALSE)
# occ_specieslink_pf

## 2.8 records outside a region of interest ----
# occ_specieslink_pf <- bdc::bdc_coordinates_country_inconsistent(
#   data = occ_specieslink_pf,
#   country_name = ct_lim_names,
#   country = "country_suggested",
#   lon = "decimalLongitude",
#   lat = "decimalLatitude",
#   dist = 0.1) # in decimal degrees (~11 km at the equator)
# occ_specieslink_pf

## summary ----
occ_specieslink_pf <- bdc::bdc_summary_col(data = occ_specieslink_pf)
occ_specieslink_pf

## report ----
occ_specieslink_pf_report_prefilter <- bdc::bdc_create_report(
  data = occ_specieslink_pf,
  database_id = "database_id",
  workflow_step = "prefilter",
  save_report = FALSE)
occ_specieslink_pf_report_prefilter$x$data

occ_specieslink_pf_report_space <- bdc::bdc_create_report(
  data = occ_specieslink_pf,
  database_id = "database_id",
  workflow_step = "space",
  save_report = FALSE)
occ_specieslink_pf_report_space$x$data

occ_specieslink_pf_report <- rbind(
  occ_specieslink_pf_report_prefilter$x$data[1:4,],
  occ_specieslink_pf_report_space$x$data[1,],
  occ_specieslink_pf_report_prefilter$x$data[7,])
occ_specieslink_pf_report

## filtering ----
occ_specieslink_pf_output <- occ_specieslink_pf %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(
    data = ., 
    col_to_remove = "all")
occ_specieslink_pf_output

## export ----
arrow::write_parquet(occ_specieslink_pf_output, "01_data/01_occurrences/04_prefilter/occ_specieslink_pf_output.parquet")
arrow::write_parquet(occ_specieslink_pf_report, "01_data/01_occurrences/04_prefilter/occ_specieslink_pf_report.parquet")

# end ---------------------------------------------------------------------