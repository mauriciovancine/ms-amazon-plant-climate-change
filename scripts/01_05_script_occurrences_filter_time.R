#' ----
#' title: obj2 - occurrences - time filter
#' author: mauricio vancine
#' date: 2026-04-13
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)
library(bdc)

# parameter 
year <- 1985

# atlantic epiphytes ------------------------------------------------------

## import database ----
occ_atlantic_epiphytes <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_atlantic_epiphytes_pf_output.parquet")
occ_atlantic_epiphytes

## 5.1 records lacking event date information ----
occ_atlantic_epiphytes_time <- bdc::bdc_eventDate_empty(
  data = occ_atlantic_epiphytes, 
  eventDate = "verbatimEventDate")
occ_atlantic_epiphytes_time

## 5.2 extract year from event date ----
occ_atlantic_epiphytes_time <- bdc::bdc_year_from_eventDate(
  data = occ_atlantic_epiphytes_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_atlantic_epiphytes_time

## 5.3 records with out-of-range collecting year ----
occ_atlantic_epiphytes_time <- bdc::bdc_year_outOfRange(
  data = occ_atlantic_epiphytes_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_atlantic_epiphytes_time

## summary
occ_atlantic_epiphytes_time <- bdc:::bdc_summary_col(data = occ_atlantic_epiphytes_time)
occ_atlantic_epiphytes_time

## report ----
occ_atlantic_epiphytes_time_report <- bdc::bdc_create_report(
  data = occ_atlantic_epiphytes_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_atlantic_epiphytes_time_report

## filtering the database ----
occ_atlantic_epiphytes_time_output <- occ_atlantic_epiphytes_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_atlantic_epiphytes_time_output

## export ----
arrow::write_parquet(occ_atlantic_epiphytes_time_output, "01_data/01_occurrences/05_filter_time/occ_atlantic_epiphytes_pf_time_output.parquet")
arrow::write_parquet(occ_atlantic_epiphytes_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_atlantic_epiphytes_pf_time_report.parquet")

# bien ------------------------------------------------------

## import database ----
occ_bien <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_bien_pf_output.parquet")
occ_bien

## 5.1 records lacking event date information ----
occ_bien_time <- bdc::bdc_eventDate_empty(
  data = occ_bien, 
  eventDate = "verbatimEventDate")
occ_bien_time

## 5.2 extract year from event date ----
occ_bien_time <- bdc::bdc_year_from_eventDate(
  data = occ_bien_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_bien_time

## 5.3 records with out-of-range collecting year ----
occ_bien_time <- bdc::bdc_year_outOfRange(
  data = occ_bien_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_bien_time

## summary
occ_bien_time <- bdc:::bdc_summary_col(data = occ_bien_time)
occ_bien_time

## report ----
occ_bien_time_report <- bdc::bdc_create_report(
  data = occ_bien_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_bien_time_report

## filtering the database ----
occ_bien_time_output <- occ_bien_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_bien_time_output

## export ----
arrow::write_parquet(occ_bien_time_output, "01_data/01_occurrences/05_filter_time/occ_bien_pf_time_output.parquet")
arrow::write_parquet(occ_bien_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_bien_pf_time_report.parquet")

# dryflor ---------------------------------------------------------------

## import database ----
occ_dryflor <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_dryflor_pf_output.parquet")
occ_dryflor

## 5.1 records lacking event date information ----
occ_dryflor_time <- bdc::bdc_eventDate_empty(
  data = occ_dryflor, 
  eventDate = "verbatimEventDate")
occ_dryflor_time

## 5.2 extract year from event date ----
occ_dryflor_time <- bdc::bdc_year_from_eventDate(
  data = occ_dryflor_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_dryflor_time

## 5.3 records with out-of-range collecting year ----
occ_dryflor_time <- bdc::bdc_year_outOfRange(
  data = occ_dryflor_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_dryflor_time

## summary
occ_dryflor_time <- bdc:::bdc_summary_col(data = occ_dryflor_time)
occ_dryflor_time

## report ----
occ_dryflor_time_report <- bdc::bdc_create_report(
  data = occ_dryflor_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_dryflor_time_report

## filtering the database ----
occ_dryflor_time_output <- occ_dryflor_time %>%
  dplyr::filter(.summary == FALSE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_dryflor_time_output

## export ----
arrow::write_parquet(occ_dryflor_time_output, "01_data/01_occurrences/05_filter_time/occ_dryflor_pf_time_output.parquet")
arrow::write_parquet(occ_dryflor_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_dryflor_pf_time_report.parquet")

# gbif ---------------------------------------------------------------

for(g in 0:64){
  
  print(g)
  
  ## import database ----
  occ_gbif <- arrow::read_parquet(
    paste0("01_data/01_occurrences/04_prefilter/occ_gbif_part", 
           ifelse(g < 10, paste0("0", g), g), "_pf_output.parquet"))
  occ_gbif
  
  ## 5.1 records lacking event date information ----
  occ_gbif_time <- bdc::bdc_eventDate_empty(
    data = occ_gbif, 
    eventDate = "verbatimEventDate")
  occ_gbif_time
  
  ## 5.2 extract year from event date ----
  occ_gbif_time <- bdc::bdc_year_from_eventDate(
    data = occ_gbif_time, 
    eventDate = "verbatimEventDate") %>% 
    tibble::as_tibble()
  occ_gbif_time
  
  ## 5.3 records with out-of-range collecting year ----
  occ_gbif_time <- bdc::bdc_year_outOfRange(
    data = occ_gbif_time,
    eventDate = "year",
    year_threshold = year) %>% 
    tibble::as_tibble()
  occ_gbif_time
  
  ## summary
  occ_gbif_time <- bdc:::bdc_summary_col(data = occ_gbif_time)
  occ_gbif_time
  
  ## report ----
  occ_gbif_time_report <- bdc::bdc_create_report(
    data = occ_gbif_time,
    database_id = "database_id",
    workflow_step = "time",
    save_report = FALSE)
  occ_gbif_time_report
  
  ## filtering the database ----
  occ_gbif_time_output <- occ_gbif_time %>%
    dplyr::filter(.summary == TRUE) %>%
    bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
  occ_gbif_time_output
  
  ## export ----
  arrow::write_parquet(occ_gbif_time_output,  
                       paste0("01_data/01_occurrences/05_filter_time/occ_gbif_part",
                              ifelse(g < 10, paste0("0", g), g), "_pf_time_output.parquet"))
  arrow::write_parquet(occ_gbif_time_report$x$data,  
                       paste0("01_data/01_occurrences/05_filter_time/occ_gbif_part",
                              ifelse(g < 10, paste0("0", g), g), "_pf_time_report.parquet"))
  
}

# idigbio ---------------------------------------------------------------

## import database ----
occ_idigbio <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_idigbio_pf_output.parquet")
occ_idigbio

## 5.1 records lacking event date information ----
occ_idigbio_time <- bdc::bdc_eventDate_empty(
  data = occ_idigbio, 
  eventDate = "verbatimEventDate")
occ_idigbio_time

## 5.2 extract year from event date ----
occ_idigbio_time <- bdc::bdc_year_from_eventDate(
  data = occ_idigbio_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_idigbio_time

## 5.3 records with out-of-range collecting year ----
occ_idigbio_time <- bdc::bdc_year_outOfRange(
  data = occ_idigbio_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_idigbio_time

## summary
occ_idigbio_time <- bdc:::bdc_summary_col(data = occ_idigbio_time)
occ_idigbio_time

## report ----
occ_idigbio_time_report <- bdc::bdc_create_report(
  data = occ_idigbio_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_idigbio_time_report

## filtering the database ----
occ_idigbio_time_output <- occ_idigbio_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_idigbio_time_output

## export ----
arrow::write_parquet(occ_idigbio_time_output, "01_data/01_occurrences/05_filter_time/occ_idigbio_pf_time_output.parquet")
arrow::write_parquet(occ_idigbio_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_idigbio_pf_time_report.parquet")

# neotroptree ---------------------------------------------------------------

## import database ----
occ_neotroptree <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_neotroptree_pf_output.parquet")
occ_neotroptree

## 5.1 records lacking event date information ----
occ_neotroptree_time <- bdc::bdc_eventDate_empty(
  data = occ_neotroptree, 
  eventDate = "verbatimEventDate")
occ_neotroptree_time

## 5.2 extract year from event date ----
occ_neotroptree_time <- bdc::bdc_year_from_eventDate(
  data = occ_neotroptree_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_neotroptree_time

## 5.3 records with out-of-range collecting year ----
occ_neotroptree_time <- bdc::bdc_year_outOfRange(
  data = occ_neotroptree_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_neotroptree_time

## summary
occ_neotroptree_time <- bdc:::bdc_summary_col(data = occ_neotroptree_time)
occ_neotroptree_time

## report ----
occ_neotroptree_time_report <- bdc::bdc_create_report(
  data = occ_neotroptree_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_neotroptree_time_report

## filtering the database ----
occ_neotroptree_time_output <- occ_neotroptree_time %>%
  dplyr::filter(.summary == FALSE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_neotroptree_time_output

## export ----
arrow::write_parquet(occ_neotroptree_time_output, "01_data/01_occurrences/05_filter_time/occ_neotroptree_pf_time_output.parquet")
arrow::write_parquet(occ_neotroptree_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_neotroptree_pf_time_report.parquet")

# sibbr ------------------------------------------------------

## import database ----
occ_sibbr <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_sibbr_pf_output.parquet")
occ_sibbr

## 5.1 records lacking event date information ----
occ_sibbr_time <- bdc::bdc_eventDate_empty(
  data = occ_sibbr, 
  eventDate = "verbatimEventDate")
occ_sibbr_time

## 5.2 extract year from event date ----
occ_sibbr_time <- bdc::bdc_year_from_eventDate(
  data = occ_sibbr_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_sibbr_time

## 5.3 records with out-of-range collecting year ----
occ_sibbr_time <- bdc::bdc_year_outOfRange(
  data = occ_sibbr_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_sibbr_time

## summary
occ_sibbr_time <- bdc:::bdc_summary_col(data = occ_sibbr_time)
occ_sibbr_time

## report ----
occ_sibbr_time_report <- bdc::bdc_create_report(
  data = occ_sibbr_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_sibbr_time_report

## filtering the database ----
occ_sibbr_time_output <- occ_sibbr_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_sibbr_time_output

## export ----
arrow::write_parquet(occ_sibbr_time_output, "01_data/01_occurrences/05_filter_time/occ_sibbr_pf_time_output.parquet")
arrow::write_parquet(occ_sibbr_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_sibbr_pf_time_report.parquet")

# specieslink ------------------------------------------------------

## import database ----
occ_specieslink <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_specieslink_pf_output.parquet")
occ_specieslink

## 5.1 records lacking event date information ----
occ_specieslink_time <- bdc::bdc_eventDate_empty(
  data = occ_specieslink, 
  eventDate = "verbatimEventDate")
occ_specieslink_time

## 5.2 extract year from event date ----
occ_specieslink_time <- bdc::bdc_year_from_eventDate(
  data = occ_specieslink_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_specieslink_time

## 5.3 records with out-of-range collecting year ----
occ_specieslink_time <- bdc::bdc_year_outOfRange(
  data = occ_specieslink_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_specieslink_time

## summary
occ_specieslink_time <- bdc:::bdc_summary_col(data = occ_specieslink_time)
occ_specieslink_time

## report ----
occ_specieslink_time_report <- bdc::bdc_create_report(
  data = occ_specieslink_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_specieslink_time_report

## filtering the database ----
occ_specieslink_time_output <- occ_specieslink_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_specieslink_time_output

## export ----
arrow::write_parquet(occ_specieslink_time_output, "01_data/01_occurrences/05_filter_time/occ_specieslink_pf_time_output.parquet")
arrow::write_parquet(occ_specieslink_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_specieslink_pf_time_report.parquet")

# jabot ------------------------------------------------------

## import database ----
occ_jabot <- arrow::read_parquet("01_data/01_occurrences/04_prefilter/occ_jabot_pf_output.parquet")
occ_jabot

## 5.1 records lacking event date information ----
occ_jabot_time <- bdc::bdc_eventDate_empty(
  data = occ_jabot, 
  eventDate = "verbatimEventDate")
occ_jabot_time

## 5.2 extract year from event date ----
occ_jabot_time <- bdc::bdc_year_from_eventDate(
  data = occ_jabot_time, 
  eventDate = "verbatimEventDate") %>% 
  tibble::as_tibble()
occ_jabot_time

## 5.3 records with out-of-range collecting year ----
occ_jabot_time <- bdc::bdc_year_outOfRange(
  data = occ_jabot_time,
  eventDate = "year",
  year_threshold = year) %>% 
  tibble::as_tibble()
occ_jabot_time

## summary
occ_jabot_time <- bdc:::bdc_summary_col(data = occ_jabot_time)
occ_jabot_time

## report ----
occ_jabot_time_report <- bdc::bdc_create_report(
  data = occ_jabot_time,
  database_id = "database_id",
  workflow_step = "time",
  save_report = FALSE)
occ_jabot_time_report

## filtering the database ----
occ_jabot_time_output <- occ_jabot_time %>%
  dplyr::filter(.summary == TRUE) %>%
  bdc::bdc_filter_out_flags(data = ., col_to_remove = "all")
occ_jabot_time_output

## export ----
arrow::write_parquet(occ_jabot_time_output, "01_data/01_occurrences/05_filter_time/occ_jabot_pf_time_output.parquet")
arrow::write_parquet(occ_jabot_time_report$x$data, "01_data/01_occurrences/05_filter_time/occ_jabot_pf_time_report.parquet")

# end ---------------------------------------------------------------------
