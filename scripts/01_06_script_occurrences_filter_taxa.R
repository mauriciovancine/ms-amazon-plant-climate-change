#' ----
#' title: obj2 - occurrences - taxonomy filter
#' author: mauricio vancine
#' date: 2026-05-21
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)
library(bdc)
library(rgnparser)

# options
options(timeout = 1e6)

# atlantic epiphytes ------------------------------------------------------

# import data
occ_atlantic_epiphytes <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_atlantic_epiphytes_pf_time_output.parquet")
occ_atlantic_epiphytes

## 6.1 clean and parse species names ----
occ_atlantic_epiphytes_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_atlantic_epiphytes$scientificName, 
  save_outputs = FALSE)
occ_atlantic_epiphytes_parse_names

occ_atlantic_epiphytes_parse_names <- occ_atlantic_epiphytes_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_atlantic_epiphytes_parse_names

occ_atlantic_epiphytes <- dplyr::bind_cols(occ_atlantic_epiphytes, 
                                           occ_atlantic_epiphytes_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_atlantic_epiphytes

## 6.2 names harmonization ----
occ_atlantic_epiphytes_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = occ_atlantic_epiphytes$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = 23, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
occ_atlantic_epiphytes_query_names

occ_atlantic_epiphytes_taxonomy <- occ_atlantic_epiphytes %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., occ_atlantic_epiphytes_query_names)
occ_atlantic_epiphytes_taxonomy

## report ----
occ_atlantic_epiphytes_taxonomy_report <- bdc::bdc_create_report(
  data = occ_atlantic_epiphytes_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
occ_atlantic_epiphytes_taxonomy_report

## unresolved names ----
occ_atlantic_epiphytes_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = occ_atlantic_epiphytes_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
occ_atlantic_epiphytes_taxonomy_unresolved_names

## filter ----
occ_atlantic_epiphytes_taxonomy_output <- bdc::bdc_filter_out_names(
  data = occ_atlantic_epiphytes_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
occ_atlantic_epiphytes_taxonomy_output

## subspecies and variation ----
occ_atlantic_epiphytes_taxonomy_output <- occ_atlantic_epiphytes_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " "))
occ_atlantic_epiphytes_taxonomy_output

## export ----
arrow::write_parquet(occ_atlantic_epiphytes_taxonomy_output, "01_data/01_occurrences/06_filter_taxonomy/occ_atlantic_epiphytes_pf_time_taxonomy_output.parquet")
arrow::write_parquet(occ_atlantic_epiphytes_taxonomy_report$x$data, "01_data/01_occurrences/06_filter_taxonomy/occ_atlantic_epiphytes_pf_time_taxonomy_report.parquet")
arrow::write_parquet(occ_atlantic_epiphytes_taxonomy_unresolved_names, "01_data/01_occurrences/06_filter_taxonomy/occ_atlantic_epiphytes_pf_time_taxonomy_unresolved_names.parquet")

# bien ------------------------------------------------------

# import data
occ_bien <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_bien_pf_time_output.parquet")
occ_bien

## 6.1 clean and parse species names ----
occ_bien_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_bien$scientificName, 
  save_outputs = FALSE)
occ_bien_parse_names

occ_bien_parse_names <- occ_bien_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_bien_parse_names

occ_bien <- dplyr::bind_cols(occ_bien, 
                             occ_bien_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_bien

## 6.2 names harmonization ----
occ_bien_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = occ_bien$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = 23, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
occ_bien_query_names

occ_bien_taxonomy <- occ_bien %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., occ_bien_query_names)
occ_bien_taxonomy

## report ----
occ_bien_taxonomy_report <- bdc::bdc_create_report(
  data = occ_bien_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
occ_bien_taxonomy_report

## unresolved names ----
occ_bien_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = occ_bien_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
occ_bien_taxonomy_unresolved_names

## filter ----
occ_bien_taxonomy_output <- bdc::bdc_filter_out_names(
  data = occ_bien_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
occ_bien_taxonomy_output

## subspecies and variation ----
occ_bien_taxonomy_output <- occ_bien_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " "))
occ_bien_taxonomy_output

## export ----
arrow::write_parquet(occ_bien_taxonomy_output, "01_data/01_occurrences/06_filter_taxonomy/occ_bien_pf_time_taxonomy_output.parquet")
arrow::write_parquet(occ_bien_taxonomy_report$x$data, "01_data/01_occurrences/06_filter_taxonomy/occ_bien_pf_time_taxonomy_report.parquet")
arrow::write_parquet(occ_bien_taxonomy_unresolved_names, "01_data/01_occurrences/06_filter_taxonomy/occ_bien_pf_time_taxonomy_unresolved_names.parquet")

# dryflor ------------------------------------------------------

# import data
occ_dryflor <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_dryflor_pf_time_output.parquet")
occ_dryflor

## 6.1 clean and parse species names ----
occ_dryflor_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_dryflor$scientificName, 
  save_outputs = FALSE)
occ_dryflor_parse_names

occ_dryflor_parse_names <- occ_dryflor_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_dryflor_parse_names

occ_dryflor <- dplyr::bind_cols(occ_dryflor, 
                                occ_dryflor_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_dryflor

## 6.2 names harmonization ----
occ_dryflor_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = occ_dryflor$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = 23, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
occ_dryflor_query_names

occ_dryflor_taxonomy <- occ_dryflor %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., occ_dryflor_query_names)
occ_dryflor_taxonomy

## report ----
occ_dryflor_taxonomy_report <- bdc::bdc_create_report(
  data = occ_dryflor_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
occ_dryflor_taxonomy_report

## unresolved names ----
occ_dryflor_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = occ_dryflor_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
occ_dryflor_taxonomy_unresolved_names

## filter ----
occ_dryflor_taxonomy_output <- bdc::bdc_filter_out_names(
  data = occ_dryflor_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
occ_dryflor_taxonomy_output

## subspecies and variation ----
occ_dryflor_taxonomy_output <- occ_dryflor_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " "))
occ_dryflor_taxonomy_output

## export ----
arrow::write_parquet(occ_dryflor_taxonomy_output, "01_data/01_occurrences/06_filter_taxonomy/occ_dryflor_pf_time_taxonomy_output.parquet")
arrow::write_parquet(occ_dryflor_taxonomy_report$x$data, "01_data/01_occurrences/06_filter_taxonomy/occ_dryflor_pf_time_taxonomy_report.parquet")
arrow::write_parquet(occ_dryflor_taxonomy_unresolved_names, "01_data/01_occurrences/06_filter_taxonomy/occ_dryflor_pf_time_taxonomy_unresolved_names.parquet")

# gbif ------------------------------------------------------

for(g in 21:64){
  
  # import data
  occ_gbif <- arrow::read_parquet(
    paste0("01_data/01_occurrences/05_filter_time/occ_gbif_part", 
           ifelse(g < 10, paste0("0", g), g), "_pf_time_output.parquet"))
  occ_gbif
  
  ## 6.1 clean and parse species names ----
  occ_gbif_parse_names <- bdc::bdc_clean_names(
    sci_names = occ_gbif$scientificName, 
    save_outputs = FALSE)
  occ_gbif_parse_names
  
  occ_gbif_parse_names <- occ_gbif_parse_names %>% 
    dplyr::select(.uncer_terms, names_clean)
  occ_gbif_parse_names
  
  occ_gbif <- dplyr::bind_cols(occ_gbif, 
                               occ_gbif_parse_names) %>% 
    dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
    dplyr::filter(names_clean_n >= 2)
  occ_gbif
  
  ## 6.2 names harmonization ----
  occ_gbif_query_names <- bdc::bdc_query_names_taxadb(
    sci_name            = occ_gbif$names_clean,
    replace_synonyms    = TRUE, # replace synonyms by accepted names?
    suggest_names       = TRUE, # try to found a candidate name for misspelled names?
    suggestion_distance = 0.9, # distance between the searched and suggested names
    db                  = "gbif", # taxonomic database
    rank_name           = "Plantae", # a taxonomic rank
    rank                = "kingdom", # name of the taxonomic rank
    parallel            = TRUE, # should parallel processing be used?
    ncores              = 23, # number of cores to be used in the parallelization process
    export_accepted     = FALSE # save names linked to multiple accepted names
  )
  occ_gbif_query_names
  
  occ_gbif_taxonomy <- occ_gbif %>%
    dplyr::rename(verbatim_scientificName = scientificName) %>%
    dplyr::select(-names_clean) %>%
    dplyr::bind_cols(., occ_gbif_query_names)
  occ_gbif_taxonomy
  
  ## report ----
  occ_gbif_taxonomy_report <- bdc::bdc_create_report(
    data = occ_gbif_taxonomy,
    database_id = "database_id",
    workflow_step = "taxonomy",
    save_report = FALSE)
  occ_gbif_taxonomy_report
  
  ## unresolved names ----
  occ_gbif_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
    data = occ_gbif_taxonomy,
    col_name = "notes",
    taxonomic_status = "accepted",
    opposite = TRUE)
  occ_gbif_taxonomy_unresolved_names
  
  ## filter ----
  occ_gbif_taxonomy_output <- bdc::bdc_filter_out_names(
    data = occ_gbif_taxonomy,
    taxonomic_status = "accepted",
    opposite = FALSE)
  occ_gbif_taxonomy_output
  
  ## subspecies and variation ----
  occ_gbif_taxonomy_output <- occ_gbif_taxonomy_output %>% 
    dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
    dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                         word(scientificName, 2),
                                         sep = " "))
  occ_gbif_taxonomy_output
  
  ## export ----
  arrow::write_parquet(occ_gbif_taxonomy_output, 
                       paste0("01_data/01_occurrences/06_filter_taxonomy/occ_gbif_part",
                              ifelse(g < 10, paste0("0", g), g), "_pf_time_taxonomy_output.parquet"))
  arrow::write_parquet(occ_gbif_taxonomy_report$x$data, 
                       paste0("01_data/01_occurrences/06_filter_taxonomy/occ_gbif_part",
                              ifelse(g < 10, paste0("0", g), g), "_pf_time_taxonomy_report.parquet"))
  arrow::write_parquet(occ_gbif_taxonomy_unresolved_names, 
                       paste0("01_data/01_occurrences/06_filter_taxonomy/occ_gbif_part",
                              ifelse(g < 10, paste0("0", g), g), "_pf_time_taxonomy_unresolved_names.parquet"))
  
}

# idigbio ------------------------------------------------------

# import data
occ_idigbio <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_idigbio_pf_time_output.parquet")
occ_idigbio

## 6.1 clean and parse species names ----
occ_idigbio_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_idigbio$scientificName, 
  save_outputs = FALSE)
occ_idigbio_parse_names

occ_idigbio_parse_names <- occ_idigbio_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_idigbio_parse_names

occ_idigbio <- dplyr::bind_cols(occ_idigbio, 
                                occ_idigbio_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_idigbio

# separate
occ_idigbio_list <- list()
occ_idigbio_list[[01]] <- dplyr::slice(occ_idigbio, 1:500000)   
occ_idigbio_list[[02]] <- dplyr::slice(occ_idigbio, 500001:1000000)   
occ_idigbio_list[[03]] <- dplyr::slice(occ_idigbio, 1000001:1500000)   
occ_idigbio_list[[04]] <- dplyr::slice(occ_idigbio, 1500001:2000000)   
occ_idigbio_list[[05]] <- dplyr::slice(occ_idigbio, 2000001:2500000)   
occ_idigbio_list[[06]] <- dplyr::slice(occ_idigbio, 2500001:3000000)   
occ_idigbio_list[[07]] <- dplyr::slice(occ_idigbio, 3000001:3500000)
occ_idigbio_list[[08]] <- dplyr::slice(occ_idigbio, 3500001:4000000)   
occ_idigbio_list[[09]] <- dplyr::slice(occ_idigbio, 4000001:4500000)   
occ_idigbio_list[[10]] <- dplyr::slice(occ_idigbio, 4500001:5000000)   
occ_idigbio_list[[11]] <- dplyr::slice(occ_idigbio, 5000001:5500000)   
occ_idigbio_list[[12]] <- dplyr::slice(occ_idigbio, 5500001:6000000)   
occ_idigbio_list[[13]] <- dplyr::slice(occ_idigbio, 6000001:6690372)
occ_idigbio_list

for(i in 1:13){
  
  ## 6.2 names harmonization ----
  occ_idigbio_query_names <- bdc::bdc_query_names_taxadb(
    sci_name            = occ_idigbio_list[[i]]$names_clean,
    replace_synonyms    = TRUE, # replace synonyms by accepted names?
    suggest_names       = TRUE, # try to found a candidate name for misspelled names?
    suggestion_distance = 0.9, # distance between the searched and suggested names
    db                  = "gbif", # taxonomic database
    rank_name           = "Plantae", # a taxonomic rank
    rank                = "kingdom", # name of the taxonomic rank
    parallel            = TRUE, # should parallel processing be used?
    ncores              = 23, # number of cores to be used in the parallelization process
    export_accepted     = FALSE # save names linked to multiple accepted names
  )
  occ_idigbio_query_names
  
  occ_idigbio_taxonomy <- occ_idigbio_list[[i]] %>%
    dplyr::rename(verbatim_scientificName = scientificName) %>%
    dplyr::select(-names_clean) %>%
    dplyr::bind_cols(., occ_idigbio_query_names)
  occ_idigbio_taxonomy
  
  ## report ----
  occ_idigbio_taxonomy_report <- bdc::bdc_create_report(
    data = occ_idigbio_taxonomy,
    database_id = "database_id",
    workflow_step = "taxonomy",
    save_report = FALSE)
  occ_idigbio_taxonomy_report
  
  ## unresolved names ----
  occ_idigbio_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
    data = occ_idigbio_taxonomy,
    col_name = "notes",
    taxonomic_status = "accepted",
    opposite = TRUE)
  occ_idigbio_taxonomy_unresolved_names
  
  ## filter ----
  occ_idigbio_taxonomy_output <- bdc::bdc_filter_out_names(
    data = occ_idigbio_taxonomy,
    taxonomic_status = "accepted",
    opposite = FALSE)
  occ_idigbio_taxonomy_output
  
  ## subspecies and variation ----
  occ_idigbio_taxonomy_output <- occ_idigbio_taxonomy_output %>% 
    dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
    dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                         word(scientificName, 2),
                                         sep = " "))
  occ_idigbio_taxonomy_output
  
  ## export ----
  arrow::write_parquet(occ_idigbio_taxonomy_output, 
                       paste0("01_data/01_occurrences/06_filter_taxonomy/occ_idigbio_part", 
                              ifelse(i < 10, paste0("0", i), i), "_pf_time_taxonomy_output.parquet"))
  arrow::write_parquet(occ_idigbio_taxonomy_report$x$data, 
                       paste0("01_data/01_occurrences/06_filter_taxonomy/occ_idigbio_part", 
                              ifelse(i < 10, paste0("0", i), i), "_pf_time_taxonomy_report.parquet"))
  arrow::write_parquet(occ_idigbio_taxonomy_unresolved_names, 
                       paste0("01_data/01_occurrences/06_filter_taxonomy/occ_idigbio_part",
                              ifelse(i < 10, paste0("0", i), i), "_pf_time_taxonomy_unresolved_names.parquet"))
  
  
}

# neotroptree ------------------------------------------------------

# import data
occ_neotroptree <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_neotroptree_pf_time_output.parquet")
occ_neotroptree

## 6.1 clean and parse species names ----
occ_neotroptree_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_neotroptree$scientificName, 
  save_outputs = FALSE)
occ_neotroptree_parse_names

occ_neotroptree_parse_names <- occ_neotroptree_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_neotroptree_parse_names

occ_neotroptree <- dplyr::bind_cols(occ_neotroptree, 
                                    occ_neotroptree_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_neotroptree

## 6.2 names harmonization ----
occ_neotroptree_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = occ_neotroptree$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = 23, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
occ_neotroptree_query_names

occ_neotroptree_taxonomy <- occ_neotroptree %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., occ_neotroptree_query_names)
occ_neotroptree_taxonomy

## report ----
occ_neotroptree_taxonomy_report <- bdc::bdc_create_report(
  data = occ_neotroptree_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
occ_neotroptree_taxonomy_report

## unresolved names ----
occ_neotroptree_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = occ_neotroptree_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
occ_neotroptree_taxonomy_unresolved_names

## filter ----
occ_neotroptree_taxonomy_output <- bdc::bdc_filter_out_names(
  data = occ_neotroptree_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
occ_neotroptree_taxonomy_output

## subspecies and variation ----
occ_neotroptree_taxonomy_output <- occ_neotroptree_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " "))
occ_neotroptree_taxonomy_output

## export ----
arrow::write_parquet(occ_neotroptree_taxonomy_output, "01_data/01_occurrences/06_filter_taxonomy/occ_neotroptree_pf_time_taxonomy_output.parquet")
arrow::write_parquet(occ_neotroptree_taxonomy_report$x$data, "01_data/01_occurrences/06_filter_taxonomy/occ_neotroptree_pf_time_taxonomy_report.parquet")
arrow::write_parquet(occ_neotroptree_taxonomy_unresolved_names, "01_data/01_occurrences/06_filter_taxonomy/occ_neotroptree_pf_time_taxonomy_unresolved_names.parquet")

# sibbr ------------------------------------------------------

# import data
occ_sibbr <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_sibbr_pf_time_output.parquet")
occ_sibbr

## 6.1 clean and parse species names ----
occ_sibbr_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_sibbr$scientificName, 
  save_outputs = FALSE)
occ_sibbr_parse_names

occ_sibbr_parse_names <- occ_sibbr_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_sibbr_parse_names

occ_sibbr <- dplyr::bind_cols(occ_sibbr, 
                              occ_sibbr_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_sibbr

## 6.2 names harmonization ----
occ_sibbr_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = occ_sibbr$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = 23, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
occ_sibbr_query_names

occ_sibbr_taxonomy <- occ_sibbr %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., occ_sibbr_query_names)
occ_sibbr_taxonomy

## report ----
occ_sibbr_taxonomy_report <- bdc::bdc_create_report(
  data = occ_sibbr_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
occ_sibbr_taxonomy_report

## unresolved names ----
occ_sibbr_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = occ_sibbr_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
occ_sibbr_taxonomy_unresolved_names

## filter ----
occ_sibbr_taxonomy_output <- bdc::bdc_filter_out_names(
  data = occ_sibbr_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
occ_sibbr_taxonomy_output

## subspecies and variation ----
occ_sibbr_taxonomy_output <- occ_sibbr_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " "))
occ_sibbr_taxonomy_output

## export ----
arrow::write_parquet(occ_sibbr_taxonomy_output, "01_data/01_occurrences/06_filter_taxonomy/occ_sibbr_pf_time_taxonomy_output.parquet")
arrow::write_parquet(occ_sibbr_taxonomy_report$x$data, "01_data/01_occurrences/06_filter_taxonomy/occ_sibbr_pf_time_taxonomy_report.parquet")
arrow::write_parquet(occ_sibbr_taxonomy_unresolved_names, "01_data/01_occurrences/06_filter_taxonomy/occ_sibbr_pf_time_taxonomy_unresolved_names.parquet")


# specieslink ------------------------------------------------------

# import data
occ_specieslink <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_specieslink_pf_time_output.parquet")
occ_specieslink

## 6.1 clean and parse species names ----
occ_specieslink_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_specieslink$scientificName, 
  save_outputs = FALSE)
occ_specieslink_parse_names

occ_specieslink_parse_names <- occ_specieslink_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_specieslink_parse_names

occ_specieslink <- dplyr::bind_cols(occ_specieslink, 
                                    occ_specieslink_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_specieslink

## 6.2 names harmonization ----
occ_specieslink_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = occ_specieslink$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = 23, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
occ_specieslink_query_names

occ_specieslink_taxonomy <- occ_specieslink %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., occ_specieslink_query_names)
occ_specieslink_taxonomy

## report ----
occ_specieslink_taxonomy_report <- bdc::bdc_create_report(
  data = occ_specieslink_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
occ_specieslink_taxonomy_report

## unresolved names ----
occ_specieslink_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = occ_specieslink_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
occ_specieslink_taxonomy_unresolved_names

## filter ----
occ_specieslink_taxonomy_output <- bdc::bdc_filter_out_names(
  data = occ_specieslink_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
occ_specieslink_taxonomy_output

## subspecies and variation ----
occ_specieslink_taxonomy_output <- occ_specieslink_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " "))
occ_specieslink_taxonomy_output

## export ----
arrow::write_parquet(occ_specieslink_taxonomy_output, "01_data/01_occurrences/06_filter_taxonomy/occ_specieslink_pf_time_taxonomy_output.parquet")
arrow::write_parquet(occ_specieslink_taxonomy_report$x$data, "01_data/01_occurrences/06_filter_taxonomy/occ_specieslink_pf_time_taxonomy_report.parquet")
arrow::write_parquet(occ_specieslink_taxonomy_unresolved_names, "01_data/01_occurrences/06_filter_taxonomy/occ_specieslink_pf_time_taxonomy_unresolved_names.parquet")

# jabot ------------------------------------------------------

# import data
occ_jabot <- arrow::read_parquet("01_data/01_occurrences/05_filter_time/occ_jabot_pf_time_output.parquet")
occ_jabot

## 6.1 clean and parse species names ----
occ_jabot_parse_names <- bdc::bdc_clean_names(
  sci_names = occ_jabot$scientificName, 
  save_outputs = FALSE)
occ_jabot_parse_names

occ_jabot_parse_names <- occ_jabot_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
occ_jabot_parse_names

occ_jabot <- dplyr::bind_cols(occ_jabot, 
                              occ_jabot_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
occ_jabot

## 6.2 names harmonization ----
occ_jabot_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = occ_jabot$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = 23, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
occ_jabot_query_names

occ_jabot_taxonomy <- occ_jabot %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., occ_jabot_query_names)
occ_jabot_taxonomy

## report ----
occ_jabot_taxonomy_report <- bdc::bdc_create_report(
  data = occ_jabot_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
occ_jabot_taxonomy_report

## unresolved names ----
occ_jabot_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = occ_jabot_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
occ_jabot_taxonomy_unresolved_names

## filter ----
occ_jabot_taxonomy_output <- bdc::bdc_filter_out_names(
  data = occ_jabot_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
occ_jabot_taxonomy_output

## subspecies and variation ----
occ_jabot_taxonomy_output <- occ_jabot_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " "))
occ_jabot_taxonomy_output

## export ----
arrow::write_parquet(occ_jabot_taxonomy_output, "01_data/01_occurrences/06_filter_taxonomy/occ_jabot_pf_time_taxonomy_output.parquet")
arrow::write_parquet(occ_jabot_taxonomy_report$x$data, "01_data/01_occurrences/06_filter_taxonomy/occ_jabot_pf_time_taxonomy_report.parquet")
arrow::write_parquet(occ_jabot_taxonomy_unresolved_names, "01_data/01_occurrences/06_filter_taxonomy/occ_jabot_pf_time_taxonomy_unresolved_names.parquet")

# end ---------------------------------------------------------------------
