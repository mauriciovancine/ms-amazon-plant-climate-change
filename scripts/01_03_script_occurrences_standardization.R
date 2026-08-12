#' ----
#' title: obj2 - occurrences - standardization
#' author: mauricio vancine
#' date: 2026-04-13
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(data.table)
library(arrow)
library(bdc)

# options
options(scipen = 1000)

# 1. stand databases ------------------------------------------------------

## metadata ----
metadata <- readr::read_csv(system.file("extdata/Config/DatabaseInfo.csv", package = "bdc"))
metadata

# atlantic epiphytes ------------------------------------------------------

## metadata  ----
metadata_occ_atlantic_epiphytes <- metadata %>% 
  dplyr::filter(datasetName == "AT_EPIPHYTES") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_occ_atlantic_epiphytes

## import ----
occ_atlantic_epiphytes <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_at_epiphytes_prep.parquet") %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(database_id = paste0("AT_EPIPHYTES_", 1:nrow(.)), .before = 1)
occ_atlantic_epiphytes

## stand ----
occ_atlantic_epiphytes_stand <- NULL
for(i in 1:ncol(metadata_occ_atlantic_epiphytes)){
  
  metadata_occ_atlantic_epiphytes_i <- metadata_occ_atlantic_epiphytes[, i]
  metadata_occ_atlantic_epiphytes_i_name <- colnames(metadata_occ_atlantic_epiphytes_i)
  
  if(!is.na(metadata_occ_atlantic_epiphytes_i)){
    
    occ_atlantic_epiphytes_i <- dplyr::select(occ_atlantic_epiphytes, as.character(metadata_occ_atlantic_epiphytes_i))  
    colnames(occ_atlantic_epiphytes_i) <- metadata_occ_atlantic_epiphytes_i_name
    
  }else{
    
    occ_atlantic_epiphytes_i <- tibble::tibble(col = NA)
    colnames(occ_atlantic_epiphytes_i) <- metadata_occ_atlantic_epiphytes_i_name
    
  }
  
  occ_atlantic_epiphytes_stand <- dplyr::bind_cols(occ_atlantic_epiphytes_stand, occ_atlantic_epiphytes_i)
  
}

## add id ----
occ_atlantic_epiphytes_stand <- dplyr::mutate(
  occ_atlantic_epiphytes_stand, 
  database_id = paste0("AT_EPIPHYTES_", 1:nrow(occ_atlantic_epiphytes_stand)), .before = 1)
occ_atlantic_epiphytes_stand

## export ----
arrow::write_parquet(occ_atlantic_epiphytes_stand, "01_data/01_occurrences/03_standardization/occ_atlantic_epiphytes_stand.parquet")

## remove ----
rm(occ_atlantic_epiphytes, occ_atlantic_epiphytes_stand)


# bien --------------------------------------------------------------------

## metadata ----
metadata_bien <- metadata %>% 
  dplyr::filter(datasetName == "BIEN") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_bien

## import ----
occ_bien <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_bien_prep.parquet") %>% 
  tibble::as_tibble()
occ_bien

## stand ----
occ_bien_stand <- NULL
for(i in 1:ncol(metadata_bien)){
  
  metadata_bien_i <- metadata_bien[, i]
  metadata_bien_i_name <- colnames(metadata_bien_i)
  
  if(!is.na(metadata_bien_i)){
    
    occ_bien_i <- dplyr::select(occ_bien, as.character(metadata_bien_i))  
    colnames(occ_bien_i) <- metadata_bien_i_name
    
  }else{
    
    occ_bien_i <- tibble::tibble(col = NA)
    colnames(occ_bien_i) <- metadata_bien_i_name
    
  }
  
  occ_bien_stand <- dplyr::bind_cols(occ_bien_stand, occ_bien_i)
  
}

## add id ----
occ_bien_stand <- dplyr::mutate(
  occ_bien_stand, 
  database_id = paste0("BIEN_", 1:nrow(occ_bien_stand)), .before = 1)
occ_bien_stand

## export ----
arrow::write_parquet(occ_bien_stand, "01_data/01_occurrences/03_standardization/occ_bien_stand.parquet")

## remove ----
rm(occ_bien, occ_bien_stand)


# dryflor -----------------------------------------------------------------

## metadata  ----
metadata_dryflor <- metadata %>% 
  dplyr::filter(datasetName == "DRYFLOR") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_dryflor

## import ----
occ_dryflor <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_dryflor_prep.parquet") %>% 
  tibble::as_tibble()
occ_dryflor

## stand ----
occ_dryflor_stand <- NULL
for(i in 1:ncol(metadata_dryflor)){
  
  metadata_dryflor_i <- metadata_dryflor[, i]
  metadata_dryflor_i_name <- colnames(metadata_dryflor_i)
  
  if(!is.na(metadata_dryflor_i)){
    
    occ_dryflor_i <- dplyr::select(occ_dryflor, as.character(metadata_dryflor_i))  
    colnames(occ_dryflor_i) <- metadata_dryflor_i_name
    
  }else{
    
    occ_dryflor_i <- tibble::tibble(col = NA)
    colnames(occ_dryflor_i) <- metadata_dryflor_i_name
    
  }
  
  occ_dryflor_stand <- dplyr::bind_cols(occ_dryflor_stand, occ_dryflor_i)
  
}

## add id ----
occ_dryflor_stand <- dplyr::mutate(
  occ_dryflor_stand, 
  database_id = paste0("DRYFLOR_", 1:nrow(occ_dryflor_stand)), .before = 1)
occ_dryflor_stand

## export ----
arrow::write_parquet(occ_dryflor_stand, "01_data/01_occurrences/03_standardization/occ_dryflor_stand.parquet")

## remove ----
rm(occ_dryflor, occ_dryflor_stand)


# gbif --------------------------------------------------------------------

## metadata ----
metadata_gbif <- metadata %>% 
  dplyr::filter(datasetName == "GBIF") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_gbif

for(g in 0:65){
  
  print(g)
  
  ## import ----
  occ_gbif <- arrow::read_parquet(paste0(
    "01_data/01_occurrences/02_prepared/occ_gbif_prep_part", 
    ifelse(g < 10, paste0("0", g), g), ".parquet")) %>% 
    tibble::as_tibble()
  occ_gbif
  
  ## stand ----
  occ_gbif_stand <- NULL
  for(i in 1:ncol(metadata_gbif)){
    
    metadata_gbif_i <- metadata_gbif[, i]
    metadata_gbif_i_name <- colnames(metadata_gbif_i)
    
    if(!is.na(metadata_gbif_i)){
      
      occ_gbif_i <- dplyr::select(occ_gbif, as.character(metadata_gbif_i))  
      colnames(occ_gbif_i) <- metadata_gbif_i_name
      
    }else{
      
      occ_gbif_i <- tibble::tibble(col = NA)
      colnames(occ_gbif_i) <- metadata_gbif_i_name
      
    }
    
    occ_gbif_stand <- dplyr::bind_cols(occ_gbif_stand, occ_gbif_i)
    
  }
  
  ## add id ----
  if(g != 65){
    gbif_id <- 1:nrow(occ_gbif_stand) + (g * 999999)  
  }else{
    gbif_id <- "64251001"
  }
  
  occ_gbif_stand <- dplyr::mutate(
    occ_gbif_stand, 
    database_id = paste0("GBIF_", gbif_id), .before = 1)
  occ_gbif_stand
  
  ## export ----
  arrow::write_parquet(occ_gbif_stand, 
                       paste0("01_data/01_occurrences/03_standardization/occ_gbif_part", 
                              ifelse(g < 10, paste0("0", g), g), "_stand.parquet"))
  
  ## remove ----
  rm(occ_gbif, occ_gbif_stand)
  
}

# adjust
# occ_gbif64 <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_gbif_part64_stand.parquet")
# occ_gbif64
# 
# occ_gbif65 <- arrow::read_parquet("01_data/01_occurrences/03_standardization/occ_gbif_part65_stand.parquet")
# occ_gbif65
# 
# occ_gbif64 <- rbind(occ_gbif64, occ_gbif65)
# occ_gbif64

# arrow::write_parquet(occ_gbif64, 
#                      paste0("01_data/01_occurrences/03_standardization/occ_gbif_part64_stand.parquet"))

# idigbio -----------------------------------------------------------------

## metada  ----
metadata_idigbio <- metadata %>% 
  dplyr::filter(datasetName == "IDIGBIO") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_idigbio

## import  ----
occ_idigbio <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_idgebio_prep.parquet") %>% 
  tibble::as_tibble()
occ_idigbio

## stard ----
occ_idigbio_stand <- NULL
for(i in 1:ncol(metadata_idigbio)){
  
  metadata_idigbio_i <- metadata_idigbio[, i]
  metadata_idigbio_i_name <- colnames(metadata_idigbio_i)
  
  if(!is.na(metadata_idigbio_i)){
    
    occ_idigbio_i <- dplyr::select(occ_idigbio, as.character(metadata_idigbio_i))  
    colnames(occ_idigbio_i) <- metadata_idigbio_i_name
    
  }else{
    
    occ_idigbio_i <- tibble::tibble(col = NA)
    colnames(occ_idigbio_i) <- metadata_idigbio_i_name
    
  }
  
  occ_idigbio_stand <- dplyr::bind_cols(occ_idigbio_stand, occ_idigbio_i)
  
}

## add id ----
occ_idigbio_stand <- dplyr::mutate(
  occ_idigbio_stand, 
  database_id = paste0("IDIGBIO_", 1:nrow(occ_idigbio_stand)), .before = 1)
occ_idigbio_stand

## export ----
arrow::write_parquet(occ_idigbio_stand, "01_data/01_occurrences/03_standardization/occ_idigbio_stand.parquet")

## remove ----
rm(occ_idigbio, occ_idigbio_stand)

# jabot -------------------------------------------------------------

## metadata ----
metadata_jabot <- tibble::tibble(
  scientificName = "scientificName",               
  decimalLatitude = "decimalLatitude",              
  decimalLongitude = "decimalLongitude",             
  occurrenceID = "occurrenceID",                 
  basisOfRecord = "basisOfRecord",                
  verbatimEventDate = "eventDate",            
  country = "country",                      
  stateProvince = "stateProvince",                
  county = "municipality",                       
  locality = "locality",                     
  identifiedBy = "identifiedBy",                 
  coordinateUncertaintyInMeters = NA,
  coordinatePrecision = NA,          
  recordedBy = "recordedBy" )
metadata_jabot

## import ----
occ_jabot <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_jabot_prep.parquet") %>% 
  tibble::as_tibble()
occ_jabot

## stand ----
occ_jabot_stand <- NULL
for(i in 1:ncol(metadata_jabot)){
  
  metadata_jabot_i <- metadata_jabot[, i]
  metadata_jabot_i_name <- colnames(metadata_jabot_i)
  
  if(!is.na(metadata_jabot_i)){
    
    occ_jabot_i <- dplyr::select(occ_jabot, as.character(metadata_jabot_i))  
    colnames(occ_jabot_i) <- metadata_jabot_i_name
    
  }else{
    
    occ_jabot_i <- tibble::tibble(col = NA)
    colnames(occ_jabot_i) <- metadata_jabot_i_name
    
  }
  
  occ_jabot_stand <- dplyr::bind_cols(occ_jabot_stand, occ_jabot_i)
  
}

## add id ----
occ_jabot_stand <- dplyr::mutate(
  occ_jabot_stand, 
  database_id = paste0("jabot_", 1:nrow(occ_jabot_stand)), .before = 1) %>% 
  dplyr::mutate(decimalLongitude = as.numeric(decimalLongitude),
                decimalLatitude = as.numeric(decimalLatitude))
occ_jabot_stand

## export ----
arrow::write_parquet(occ_jabot_stand, "01_data/01_occurrences/03_standardization/occ_jabot_stand.parquet")

## remove ----
rm(occ_jabot, occ_jabot_stand)

# neotroptree -------------------------------------------------------------

## metada ----
metadata_neotroptree <- metadata %>% 
  dplyr::filter(datasetName == "NEOTROPTREE") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_neotroptree

## import ----
occ_neotroptree <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_neotroptree_prep.parquet") %>% 
  tibble::as_tibble()
occ_neotroptree

## stard ----
occ_neotroptree_stand <- NULL
for(i in 1:ncol(metadata_neotroptree)){
  
  metadata_neotroptree_i <- metadata_neotroptree[, i]
  metadata_neotroptree_i_name <- colnames(metadata_neotroptree_i)
  
  if(!is.na(metadata_neotroptree_i)){
    
    occ_neotroptree_i <- dplyr::select(occ_neotroptree, as.character(metadata_neotroptree_i))  
    colnames(occ_neotroptree_i) <- metadata_neotroptree_i_name
    
  }else{
    
    occ_neotroptree_i <- tibble::tibble(col = NA)
    colnames(occ_neotroptree_i) <- metadata_neotroptree_i_name
    
  }
  
  occ_neotroptree_stand <- dplyr::bind_cols(occ_neotroptree_stand, occ_neotroptree_i)
  
}

## add id ----
occ_neotroptree_stand <- dplyr::mutate(
  occ_neotroptree_stand, 
  database_id = paste0("NEOTROPTREE_", 1:nrow(occ_neotroptree_stand)), .before = 1)
occ_neotroptree_stand

## export ----
arrow::write_parquet(occ_neotroptree_stand, "01_data/01_occurrences/03_standardization/occ_neotroptree_stand.parquet")

## remove ----
rm(occ_neotroptree, occ_neotroptree_stand)


# sibbr -------------------------------------------------------------------

## metadata  ----
metadata_sibbr <- metadata %>% 
  dplyr::filter(datasetName == "SIBBR") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_sibbr

## import ----
occ_sibbr <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_sibbr_prep.parquet") %>% 
  tibble::as_tibble()
occ_sibbr

## stand ----
occ_sibbr_stand <- NULL
for(i in 1:ncol(metadata_sibbr)){
  
  metadata_sibbr_i <- metadata_sibbr[, i]
  metadata_sibbr_i_name <- colnames(metadata_sibbr_i)
  
  if(!is.na(metadata_sibbr_i)){
    
    occ_sibbr_i <- dplyr::select(occ_sibbr, as.character(metadata_sibbr_i))  
    colnames(occ_sibbr_i) <- metadata_sibbr_i_name
    
  }else{
    
    occ_sibbr_i <- tibble::tibble(col = NA)
    colnames(occ_sibbr_i) <- metadata_sibbr_i_name
    
  }
  
  occ_sibbr_stand <- dplyr::bind_cols(occ_sibbr_stand, occ_sibbr_i)
  
}

## add id ----
occ_sibbr_stand <- dplyr::mutate(
  occ_sibbr_stand, 
  database_id = paste0("SIBBR_", 1:nrow(occ_sibbr_stand)), .before = 1)
occ_sibbr_stand

## export ----
arrow::write_parquet(occ_sibbr_stand, "01_data/01_occurrences/03_standardization/occ_sibbr_stand.parquet")

## remove ----
rm(occ_sibbr, occ_sibbr_stand)


# specieslink -------------------------------------------------------------

## metadata ----
metadata_specieslink <- metadata %>% 
  dplyr::filter(datasetName == "SPECIESLINK") %>% 
  dplyr::select(3:16) %>% 
  tibble::as_tibble()
metadata_specieslink

## import ----
occ_specieslink <- arrow::read_parquet("01_data/01_occurrences/02_prepared/occ_specieslink_prep.parquet") %>% 
  tibble::as_tibble()
occ_specieslink

## stand ----
occ_specieslink_stand <- NULL
for(i in 1:ncol(metadata_specieslink)){
  
  metadata_specieslink_i <- metadata_specieslink[, i]
  metadata_specieslink_i_name <- colnames(metadata_specieslink_i)
  
  if(!is.na(metadata_specieslink_i)){
    
    occ_specieslink_i <- dplyr::select(occ_specieslink, as.character(metadata_specieslink_i))  
    colnames(occ_specieslink_i) <- metadata_specieslink_i_name
    
  }else{
    
    occ_specieslink_i <- tibble::tibble(col = NA)
    colnames(occ_specieslink_i) <- metadata_specieslink_i_name
    
  }
  
  occ_specieslink_stand <- dplyr::bind_cols(occ_specieslink_stand, occ_specieslink_i)
  
}

## add id ----
occ_specieslink_stand <- dplyr::mutate(
  occ_specieslink_stand, 
  database_id = paste0("SPECIESLINK_", 1:nrow(occ_specieslink_stand)), .before = 1)
occ_specieslink_stand

## export ----
arrow::write_parquet(occ_specieslink_stand, "01_data/01_occurrences/03_standardization/occ_specieslink_stand.parquet")

## remove ----
rm(occ_specieslink, occ_specieslink_stand)

# end ---------------------------------------------------------------------