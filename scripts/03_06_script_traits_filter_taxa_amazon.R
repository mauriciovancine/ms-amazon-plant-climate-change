#' ---
#' title: obj2 - traits - filter taxa amazon
#' author: mauricio vancine
#' date: 2026-06-18
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)
library(janitor)

# options
options(scipen = 1000)

# import data  ------------------------------------------------------------

# species list
sp_list_am <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/species_list_amazon_species_lists_oppc_model.csv") %>% 
  dplyr::arrange(scientificName) %>% 
  dplyr::pull(scientificName)
sp_list_am

length(sp_list_am)

# traits
trait <- arrow::read_parquet("01_data/03_traits/05_standardization_taxa/trait_std_taxonomy_output.parquet") %>% 
  dplyr::arrange(scientificName)
trait

# filter
trait_am <- trait %>% 
  dplyr::filter(scientificName %in% sp_list_am)
trait_am

# explore
trait_am %>% 
  dplyr::distinct(scientificName) 
  
trait_am %>% 
  dplyr::group_by(scientificName) %>% 
  dplyr::count(traitName) %>%
  dplyr::group_by(traitName) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(per = n/nrow(sp_list_am)*100)

# summarize ---------------------------------------------------------------

# continuous ----
trait_am_suma_cont <- trait_am %>% 
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::group_by(scientificName, traitName) %>%
  dplyr::mutate(traitValue = as.numeric(traitValue)) %>% 
  dplyr::summarize(traitValue_mn = mean(traitValue),
                   traitValue_md = median(traitValue),
                   traitValue_sd = sd(traitValue),
                   traitValue_cv = sd(traitValue)/mean(traitValue),
                   .groups = "drop")
trait_am_suma_cont  

# categorical ----
trait_am_suma_cat_growth_form <- trait_am %>% 
  dplyr::filter(traitName == "growth_form") %>%
  dplyr::select(scientificName, traitValue) %>% 
  tidyr::separate(col = traitValue, into = c("trait1", "trait2", "trait3"), sep = "_")
trait_am_suma_cat_growth_form  

# bind ----
trait_am_suma <- trait_am_suma_cont %>% 
  dplyr::select(1, 2, 4) %>% 
  dplyr::rename(traitValue = 3) %>%
  dplyr::mutate(traitValue = as.character(round(traitValue, 3))) %>% 
  dplyr::bind_rows(trait_am_suma_cat) %>% 
  dplyr::arrange(scientificName)
trait_am_suma

# export ----
readr::write_csv(trait_am_suma, "01_data/03_traits/06_traits_amazon_species_list/trait_am_sumarized.csv")

# end ---------------------------------------------------------------------
