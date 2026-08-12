#' ----
#' title: obj2 - occurrences - prepare
#' author: mauricio vancine
#' date: 2026-04-13
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(vroom)
library(data.table)
library(arrow)
library(sf)
library(bdc)

# options 
options(scipen = 1e3)

# metadata ---------------------------------------------------------------

## bdc configuration data ----
bdc_metadata <- readr::read_csv(system.file("extdata/Config/DatabaseInfo.csv", package = "bdc"))
bdc_metadata

# at_epiphytes ------------------------------------------------------------

## metadata ----
bdc_metadata_at_epiphytes <- bdc_metadata %>% 
  dplyr::filter(datasetName == "AT_EPIPHYTES") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_at_epiphytes

## import ----
at_epiphytes_prep_occ <- data.table::fread("01_data/01_occurrences/01_raw/01_at_epiphytes/DataS1_Occurrence.txt") %>% 
  dplyr::mutate(YEAR_FINISH = as.numeric(YEAR_FINISH))
at_epiphytes_prep_occ

at_epiphytes_prep_abd <- data.table::fread("01_data/01_occurrences/01_raw/01_at_epiphytes/DataS1_Abundance.txt") 
at_epiphytes_prep_abd

## combine and select ----
at_epiphytes_prep <- at_epiphytes_prep_occ %>% 
  dplyr::bind_rows(at_epiphytes_prep_abd) %>% 
  dplyr::select(any_of(bdc_metadata_at_epiphytes))
at_epiphytes_prep

## confer ----
all(names(at_epiphytes_prep) == bdc_metadata_at_epiphytes)

## export ----
arrow::write_parquet(at_epiphytes_prep, "01_data/01_occurrences/02_prepared/occ_at_epiphytes_prep.parquet")

# bien --------------------------------------------------------------------

## metadata ----
bdc_metadata_bien <- bdc_metadata %>% 
  dplyr::filter(datasetName == "BIEN") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_bien

## import ----
occ_bien_prep_files <- dir("01_data/01_occurrences/01_raw/02_bien/", full.names = TRUE)
occ_bien_prep_files

occ_bien_prep <- NULL
for(i in occ_bien_prep_files){
  
  occ_bien_prep_i <- vroom::vroom(i)
  occ_bien_prep <- rbind(occ_bien_prep, occ_bien_prep_i)
  
}
occ_bien_prep

## select ----
occ_bien_prep <- dplyr::select(occ_bien_prep, any_of(bdc_metadata_bien))
occ_bien_prep

## confer ----
all(names(occ_bien_prep) == bdc_metadata_bien)

## export ----
arrow::write_parquet(occ_bien_prep, "01_data/01_occurrences/02_prepared/occ_bien_prep.parquet")

# dryflor -----------------------------------------------------------------

## metadata ----
bdc_metadata_dryflor <- bdc_metadata %>% 
  dplyr::filter(datasetName == "DRYFLOR") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_dryflor

## import ----
occ_dryflor_prep <- readr::read_csv("01_data/01_occurrences/01_raw/03_dryflor/dryflor_Science_rec_gr/dryflor_Science_rec_gr.csv") %>% 
  dplyr::select(sp_, Lat, Long, Cou, SiteCode) %>% 
  dplyr::rename(`Accepted name` = sp_,
                Latitude = Lat, 
                Longitude = Long, 
                Country = Cou, 
                `locality/site` = SiteCode) %>% 
  dplyr::mutate(`Accepted name` = stringr::str_replace_all(`Accepted name`, "_", " ")) %>% 
  dplyr::mutate(
    Country = dplyr::case_when(
      Country == "Arg" ~ "Argentina",
      Country == "Bah" ~ "Bahamas",
      Country == "Bol" ~ "Bolivia",
      Country == "Bra" ~ "Brazil",
      Country == "Col" ~ "Colombia",
      Country == "Cos" ~ "Costa Rica",
      Country == "Cub" ~ "Cuba",
      Country == "Dom" ~ "Dominican Republic",
      Country == "Ecu" ~ "Ecuador",
      Country == "Jam" ~ "Jamaica",
      Country == "Mex" ~ "Mexico",
      Country == "Nic" ~ "Nicaragua",
      Country == "Par" ~ "Paraguay",
      Country == "Per" ~ "Peru",
      Country == "Pue" ~ "Puerto Rico",
      Country == "Sai" ~ "Saint Lucia",
      Country == "Tri" ~ "Trinidad and Tobago",
      Country == "Ven" ~ "Venezuela",
      Country == "Vir" ~ "Virgin Islands",
      TRUE ~ Country))
occ_dryflor_prep

## confer ----
all(names(occ_dryflor_prep) == bdc_metadata_dryflor)

## export ----
arrow::write_parquet(occ_dryflor_prep, "01_data/01_occurrences/02_prepared/occ_dryflor_prep.parquet")

# gbif -----------------------------------------------------------------

## metadata ----
bdc_metadata_gbif <- bdc_metadata %>% 
  dplyr::filter(datasetName == "GBIF") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_gbif

## prepare ----
occ_gbif_prep_colnames <- data.table::fread(input = "01_data/01_occurrences/01_raw/04_gbif/occurrence.txt", nrow = 1) %>% 
  colnames()
occ_gbif_prep_colnames

occ_gbif_prep_colnames_n <- which(occ_gbif_prep_colnames %in% bdc_metadata_gbif)
occ_gbif_prep_colnames_n

occ_gbif_prep_colnames_n_desc <- occ_gbif_prep_colnames[occ_gbif_prep_colnames_n]
occ_gbif_prep_colnames_n_desc

## import and export ----
for(i in 0:65){
  
  print(i)
  
  if(i == 65){
    occ_gbif_prep <- data.table::fread(
      "01_data/01_occurrences/01_raw/04_gbif/occurrence.txt",
      sep = "\t", 
      quote="", 
      nrow = 64251064, 
      skip = 64251063,
      select = occ_gbif_prep_colnames_n,
      col.names = occ_gbif_prep_colnames_n_desc,
      showProgress = TRUE,
      nThread = parallel::detectCores() - 2) %>% 
      dplyr::select(any_of(bdc_metadata_gbif))
  }
  
  occ_gbif_prep <- data.table::fread(
    "01_data/01_occurrences/01_raw/04_gbif/occurrence.txt",
    sep = "\t", 
    quote="", 
    nrow = 1e6 - 1, 
    skip = i * 1e6,
    select = occ_gbif_prep_colnames_n,
    col.names = occ_gbif_prep_colnames_n_desc,
    showProgress = TRUE,
    nThread = parallel::detectCores() - 2) %>% 
    dplyr::select(any_of(bdc_metadata_gbif))
  
  arrow::write_parquet(
    occ_gbif_prep, 
    paste0("01_data/01_occurrences/02_prepared/occ_gbif_prep_part", 
           ifelse(i < 10, paste0("0", i), i), 
           ".parquet")) 
  
}

# idgebio -----------------------------------------------------------------

## metadata ----
bdc_metadata_idgebio <- bdc_metadata %>% 
  dplyr::filter(datasetName == "IDIGBIO") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_idgebio

## import ----
occ_idgebio_prep <- vroom::vroom("01_data/01_occurrences/01_raw/05_idgebio/occurrence_prep.csv")
occ_idgebio_prep

## select ----
occ_idgebio_prep <- dplyr::select(occ_idgebio_prep, any_of(bdc_metadata_idgebio))
occ_idgebio_prep

## confer ----
all(names(occ_idgebio_prep) == bdc_metadata_idgebio)

## export ----
arrow::write_parquet(occ_idgebio_prep, "01_data/01_occurrences/02_prepared/occ_idgebio_prep.parquet")

# jabot -------------------------------------------------------------------

## metadata ----
bdc_metadata_jabot <- c(
  "scientificName",               
  "decimalLatitude",              
  "decimalLongitude",             
  "occurrenceID",                 
  "basisOfRecord",                
  "eventDate",            
  "country",                      
  "stateProvince",                
  "municipality",                       
  "locality",                     
  "identifiedBy",                 
  "recordedBy")
bdc_metadata_jabot

## import ----
jabot_files <- dir(path = "01_data/01_occurrences/01_raw/09_jabot/", 
                   pattern = "occurrence.txt", full.names = TRUE, recursive = TRUE)
jabot_files

occ_jabot <- NULL
for(i in jabot_files){
  
  occ_jabot_i <- data.table::fread(i) %>%
    tibble::as_tibble() %>% 
    dplyr::select(any_of(bdc_metadata_jabot)) %>% 
    dplyr::mutate(across(everything(), as.character)) 
  occ_jabot <- rbind(occ_jabot, occ_jabot_i)
  
}
occ_jabot

## select ----
occ_jabot_prep <- dplyr::select(occ_jabot, any_of(bdc_metadata_jabot))
occ_jabot_prep

## confer ----
all(names(occ_jabot_prep) == bdc_metadata_jabot)

## export ----
arrow::write_parquet(occ_jabot_prep, "01_data/01_occurrences/02_prepared/occ_jabot_prep.parquet")

# neotroptree -----------------------------------------------------------------

## metadata ----
bdc_metadata_neotroptree <- bdc_metadata %>% 
  dplyr::filter(datasetName == "NEOTROPTREE") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_neotroptree

## import ----
occ_neotroptree_prep_spp <- vroom::vroom("01_data/01_occurrences/01_raw/06_neotroptree/v02/data_species_tree.csv") %>% 
  tidyr::pivot_longer(cols = -id, names_to = "names", values_to = "species_name") %>% 
  dplyr::select(-names) %>% 
  dplyr::rename(original_site_code = id) %>% 
  tidyr::drop_na()
occ_neotroptree_prep_spp

occ_neotroptree_prep_sf <- sf::st_read("01_data/01_occurrences/01_raw/06_neotroptree/v02/ShapeFile/pontos_neotroptree.shp") %>% 
  dplyr::mutate(long = sf::st_coordinates(.)[, 1],
                lat = sf::st_coordinates(.)[, 2]) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::rename(original_site_code = code) %>% 
  dplyr::select(bdc_metadata_neotroptree[-1])
occ_neotroptree_prep_sf

## join and select ----
occ_neotroptree_prep <- occ_neotroptree_prep_spp %>% 
  dplyr::left_join(occ_neotroptree_prep_sf) %>% 
  dplyr::select(any_of(bdc_metadata_neotroptree))
occ_neotroptree_prep

## confer ----
all(names(occ_neotroptree_prep) == bdc_metadata_neotroptree)

## export ----
arrow::write_parquet(occ_neotroptree_prep, "01_data/01_occurrences/02_prepared/occ_neotroptree_prep.parquet")

# sibbr -------------------------------------------------------------------

## metadata ----
bdc_metadata_sibbr <- bdc_metadata %>% 
  dplyr::filter(datasetName == "SIBBR") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_sibbr

## import ----
occ_sibbr_prep_part1 <- vroom::vroom("01_data/01_occurrences/01_raw/07_sibbr/03_sibbr_part1.csv")%>% 
  dplyr::rename(
    `Scientific Name` = `Scientific Name - original`,                 
    `Latitude` = `Latitude - original`,                         
    `Longitude` = `Longitude - original`,                       
    `Record ID` = `recordID`,                      
    `Basis Of Record` = `Basis Of Record - original`,                  
    `Year` = year,                             
    `Country - parsed` = `country...33`,                 
    `State - parsed` = stateProvince,                   
    `Locality` = locality,                         
    `identified _ by` = `identified _ by`,                 
    `Coordinate Uncertainty in Metres` = `coordinateUncertaintyInMetres`)
occ_sibbr_prep_part1

occ_sibbr_prep_part2 <- vroom::vroom("01_data/01_occurrences/01_raw/07_sibbr/03_sibbr_part2.csv")%>% 
  dplyr::rename(
    `Scientific Name` = `Scientific Name - original`,                 
    `Latitude` = `Latitude - original`,                         
    `Longitude` = `Longitude - original`,                       
    `Record ID` = `recordID`,                      
    `Basis Of Record` = `Basis Of Record - original`,                  
    `Year` = year,                             
    `Country - parsed` = `country...33`,                 
    `State - parsed` = stateProvince,                   
    `Locality` = locality,                         
    `identified _ by` = `identified _ by`,                 
    `Coordinate Uncertainty in Metres` = `coordinateUncertaintyInMetres`)
occ_sibbr_prep_part2

occ_sibbr_prep_part3 <- vroom::vroom("01_data/01_occurrences/01_raw/07_sibbr/03_sibbr_part3.csv")%>% 
  dplyr::rename(
    `Scientific Name` = `Scientific Name - original`,                 
    `Latitude` = `Latitude - original`,                         
    `Longitude` = `Longitude - original`,                       
    `Record ID` = `recordID`,                      
    `Basis Of Record` = `Basis Of Record - original`,                  
    `Year` = year,                             
    `Country - parsed` = `country...33`,                 
    `State - parsed` = stateProvince,                   
    `Locality` = locality,                         
    `identified _ by` = `identified _ by`,                 
    `Coordinate Uncertainty in Metres` = `coordinateUncertaintyInMetres`)
occ_sibbr_prep_part3

occ_sibbr_prep_part4 <- vroom::vroom("01_data/01_occurrences/01_raw/07_sibbr/03_sibbr_part4.csv")%>% 
  dplyr::rename(
    `Scientific Name` = `Scientific Name - original`,                 
    `Latitude` = `Latitude - original`,                         
    `Longitude` = `Longitude - original`,                       
    `Record ID` = `recordID`,                      
    `Basis Of Record` = `Basis Of Record - original`,                  
    `Year` = year,                             
    `Country - parsed` = `country...33`,                 
    `State - parsed` = stateProvince,                   
    `Locality` = locality,                         
    `identified _ by` = `identified _ by`,                 
    `Coordinate Uncertainty in Metres` = `coordinateUncertaintyInMetres`)
occ_sibbr_prep_part4

## select ----
occ_sibbr_prep_part1 <- dplyr::select(occ_sibbr_prep_part1, any_of(bdc_metadata_sibbr)) %>% 
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude))
occ_sibbr_prep_part1

occ_sibbr_prep_part2 <- dplyr::select(occ_sibbr_prep_part2, any_of(bdc_metadata_sibbr)) %>% 
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude))
occ_sibbr_prep_part2

occ_sibbr_prep_part3 <- dplyr::select(occ_sibbr_prep_part3, any_of(bdc_metadata_sibbr)) %>% 
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude))
occ_sibbr_prep_part3

occ_sibbr_prep_part4 <- dplyr::select(occ_sibbr_prep_part4, any_of(bdc_metadata_sibbr)) %>% 
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude))
occ_sibbr_prep_part4

## bind ----
occ_sibbr_prep <- dplyr::bind_rows(
  occ_sibbr_prep_part1, 
  occ_sibbr_prep_part2,
  occ_sibbr_prep_part3, 
  occ_sibbr_prep_part4)
occ_sibbr_prep

## confer ----
all(names(occ_sibbr_prep) == bdc_metadata_sibbr)

## export ----
arrow::write_parquet(occ_sibbr_prep, "01_data/01_occurrences/02_prepared/occ_sibbr_prep.parquet")

# specieslink -------------------------------------------------------------

## metadata ----
bdc_metadata_specieslink <- bdc_metadata %>% 
  dplyr::filter(datasetName == "SPECIESLINK") %>% 
  dplyr::select(3:ncol(.)) %>% 
  as.character() %>% 
  na.omit %>% 
  as.character()
bdc_metadata_specieslink

## import ----
occ_specieslink_prep <- vroom::vroom("01_data/01_occurrences/01_raw/08_specieslink/speciesLink-20251104072549-0015613.txt")
occ_specieslink_prep

## select ----
occ_specieslink_prep <- dplyr::select(occ_specieslink_prep, any_of(bdc_metadata_specieslink))
occ_specieslink_prep

## confer ----
all(names(occ_specieslink_prep) == bdc_metadata_specieslink)

## export ----
arrow::write_parquet(occ_specieslink_prep, "01_data/01_occurrences/02_prepared/occ_specieslink_prep.parquet")

# end ---------------------------------------------------------------------