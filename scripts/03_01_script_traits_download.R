#' ---
#' title: obj2 - traits - download
#' author: mauricio vancine
#' date: 2026-05-18
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(data.table)
library(arrow)
library(BIEN)
library(GIFT)

# options
options(scipen = 3000)


# try ---------------------------------------------------------------------

# TRY - Categorical Traits Dataset ----
# https://www.try-db.org/TryWeb/Data.php#3

# The global spectrum of plant form and function dataset ----
# https://www.try-db.org/TryWeb/Data.php#81

# bien --------------------------------------------------------------------

## list ----
trait_bien_list <- BIEN::BIEN_trait_list() %>% 
  tidyr::drop_na() %>% 
  dplyr::pull()
trait_bien_list

## downloaded ----
trait_bien_meta_downloaded <- dir(path = "01_data/03_traits/01_raw/01_bien") %>% 
  stringr::str_replace_all("trait_bien_", "") %>% 
  stringr::str_replace_all(".parquet", "") %>% 
  stringr::str_replace_all("_", " ")
trait_bien_meta_downloaded

## to download ----
trait_bien_meta_download <- setdiff(trait_bien_list, trait_bien_meta_downloaded)
trait_bien_meta_download

## download ----
for(i in trait_bien_meta_download){
  
  print(i)
  trait_bien_i <- BIEN::BIEN_trait_trait(trait = i, all.taxonomy = TRUE, source.citation = TRUE)
  arrow::write_parquet(trait_bien_i, 
                       paste0("01_data/03_traits/01_bien/01_raw/trait_bien_", 
                              gsub(" ", "_", tolower(i)), ".parquet"))
  
}

# gift --------------------------------------------------------------------

## traits metadata ----
trait_gift_meta <- GIFT::GIFT_traits_meta() %>% 
  tibble::as_tibble()
trait_gift_meta

## raw ----

### downloaded ----
trait_gift_meta_downloaded_raw <- dir(path = "01_data/03_traits/01_raw/02_gift_raw") %>% 
  stringr::str_replace_all("trait_gift_raw_", "") %>% 
  stringr::str_replace_all(".parquet", "")
trait_gift_meta_downloaded_raw

### to download ----
trait_gift_meta_download_raw <- trait_gift_meta$Lvl3
trait_gift_meta_download_raw <- setdiff(trait_gift_meta_download_raw, trait_gift_meta_downloaded_raw)
trait_gift_meta_download_raw

trait_gift_meta %>% 
  dplyr::filter(Lvl3 %in% trait_gift_meta_download_raw) %>% 
  dplyr::pull(Trait1)

### download ----
for(i in trait_gift_meta_download_raw){
  
  print(i)
  trait_gift_i <- GIFT::GIFT_traits_raw(trait_IDs = i)
  arrow::write_parquet(trait_gift_i, 
                       paste0("01_data/03_traits/01_raw/02_gift_raw/trait_gift_raw_", 
                              gsub(" ", "_", tolower(i)), ".parquet"))
  
}

## species ----

### downloaded ----
trait_gift_meta_downloaded_species <- dir(path = "01_data/03_traits/01_raw/02_gift_species") %>% 
  stringr::str_replace_all("trait_gift_species_", "") %>% 
  stringr::str_replace_all(".parquet", "")
trait_gift_meta_downloaded_species

### to download ----
trait_gift_meta_download_species <- unique(trait_gift_meta$Lvl3)
trait_gift_meta_download_species <- setdiff(trait_gift_meta_download_species, trait_gift_meta_downloaded_species)
trait_gift_meta_download_species

### download ----
for(i in trait_gift_meta_download_species){
  
  print(i)
  trait_gift_i <- GIFT::GIFT_traits(trait_IDs = i)
  arrow::write_parquet(trait_gift_i, 
                       paste0("01_data/03_traits/01_raw/02_gift_species/trait_gift_species_", 
                              gsub(" ", "_", tolower(i)), ".parquet"))
  
}

# domingos 2017 -----------------------------------------------------------

## download ----
download.file(url = "https://www.pnas.org/doi/suppl/10.1073/pnas.1706756114/suppl_file/pnas.1706756114.sd01.xlsx", 
              destfile = "01_data/03_traits/01_raw/04_domingos_etal_2017/pnas.1706756114.sd01.xlsx",
              mode = "wb")

# seed trait -----------------------------------------------------------

## download ----
download.file(url = "https://nph.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fnph.71268&file=nph71268-sup-0001-dataset-S1.csv", 
              destfile = "01_data/03_traits/01_raw/05_tropical_seed_trait_database/nph71268-sup-0001-dataset-S1.csv", 
              mode = "wb")

# end ---------------------------------------------------------------------
