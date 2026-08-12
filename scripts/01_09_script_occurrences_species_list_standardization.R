#' ----
#' title: obj2 - occurrences - species lists amazon standardization
#' author: mauricio vancine
#' date: 2026-06-22
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(bdc)
library(rgnparser)

# import data -------------------------------------------------------------

## amazon species lists ----
sp_list_domingos_etal_2017 <- readxl::read_xlsx("01_data/01_occurrences/09_species_list_amazon/00_raw/domingos_etal_2017/pnas.1706756114.sd01.xlsx") %>% 
  dplyr::rename(verbatimScientificName = species) %>% 
  dplyr::select(verbatimScientificName) %>% 
  dplyr::mutate(verbatimScientificName = case_when(
    verbatimScientificName == "Coussapoa arachnoidae" ~ "Coussapoa arachnoidea", 
    TRUE ~ verbatimScientificName))
sp_list_domingos_etal_2017

sp_list_ter_steege_etal_2013 <- readxl::read_xlsx("01_data/01_occurrences/09_species_list_amazon/00_raw/ter_steege_etal_2013/tersteege_appendix.xlsx") %>% 
  dplyr::rename(verbatimScientificName = 2) %>% 
  dplyr::mutate(verbatimScientificName = stringr::str_replace_all(verbatimScientificName, "_", " ")) %>% 
  dplyr::select(verbatimScientificName) %>% 
  tidyr::drop_na()
sp_list_ter_steege_etal_2013

sp_list_ter_steege_etal_2015 <- readxl::read_xlsx("01_data/01_occurrences/09_species_list_amazon/00_raw/ter_steege_etal_2015/1500936_appendixess1_to_s5.xlsx", sheet = 2) %>% 
  dplyr::rename(verbatimScientificName = 1) %>% 
  dplyr::select(verbatimScientificName)
sp_list_ter_steege_etal_2015

sp_list_ter_steege_etal_2016 <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/00_raw/ter_steege_etal_2016/41598_2016_BFsrep29549_MOESM37_ESM - Appendix S1 all species.csv") %>% 
  dplyr::rename(verbatimScientificName = 1) %>%
  dplyr::slice(1:11676) %>% 
  dplyr::select(verbatimScientificName)
sp_list_ter_steege_etal_2016

sp_list_ter_steege_etal_2019a <- readxl::read_xlsx("01_data/01_occurrences/09_species_list_amazon/00_raw/ter_steege_etal_2019a/41598_2019_40101_MOESM2_ESM.xlsx") %>% 
  dplyr::rename(verbatimScientificName = 1) %>% 
  dplyr::select(verbatimScientificName)
sp_list_ter_steege_etal_2019a

sp_list_ter_steege_etal_2019b <- readxl::read_xlsx("01_data/01_occurrences/09_species_list_amazon/00_raw/ter_steege_etal_2019b/Rarity of monodominance in hyperdiverse Amazonian forests V9 appendices (2).xlsx") %>% 
  dplyr::rename(verbatimScientificName = 1) %>% 
  dplyr::select(verbatimScientificName)
sp_list_ter_steege_etal_2019b

sp_list_ter_steege_etal_2020 <- readxl::read_xlsx("01_data/01_occurrences/09_species_list_amazon/00_raw/ter_steege_etal_2020/41598_2020_66686_MOESM2_ESM.xlsx") %>% 
  dplyr::rename(verbatimScientificName = 1) %>% 
  dplyr::select(verbatimScientificName) %>% 
  dplyr::slice(1:4962)
sp_list_ter_steege_etal_2020

## clean and parse species names ----
sp_list_domingos_etal_2017_parse_names <- bdc::bdc_clean_names(
  sci_names = sp_list_domingos_etal_2017$verbatimScientificName, 
  save_outputs = FALSE)
sp_list_domingos_etal_2017_parse_names

sp_list_domingos_etal_2017_parse_names <- sp_list_domingos_etal_2017_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
sp_list_domingos_etal_2017_parse_names

sp_list_domingos_etal_2017 <- sp_list_domingos_etal_2017 %>% 
  dplyr::bind_cols(sp_list_domingos_etal_2017_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
sp_list_domingos_etal_2017


sp_list_ter_steege_etal_2013_parse_names <- bdc::bdc_clean_names(
  sci_names = sp_list_ter_steege_etal_2013$verbatimScientificName, 
  save_outputs = FALSE)
sp_list_ter_steege_etal_2013_parse_names

sp_list_ter_steege_etal_2013_parse_names <- sp_list_ter_steege_etal_2013_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
sp_list_ter_steege_etal_2013_parse_names

sp_list_ter_steege_etal_2013 <- sp_list_ter_steege_etal_2013 %>% 
  dplyr::bind_cols(sp_list_ter_steege_etal_2013_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
sp_list_ter_steege_etal_2013

sp_list_ter_steege_etal_2015_parse_names <- bdc::bdc_clean_names(
  sci_names = sp_list_ter_steege_etal_2015$verbatimScientificName, 
  save_outputs = FALSE)
sp_list_ter_steege_etal_2015_parse_names

sp_list_ter_steege_etal_2015_parse_names <- sp_list_ter_steege_etal_2015_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
sp_list_ter_steege_etal_2015_parse_names

sp_list_ter_steege_etal_2015 <- sp_list_ter_steege_etal_2015 %>% 
  dplyr::bind_cols(sp_list_ter_steege_etal_2015_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
sp_list_ter_steege_etal_2015

sp_list_ter_steege_etal_2016_parse_names <- bdc::bdc_clean_names(
  sci_names = sp_list_ter_steege_etal_2016$verbatimScientificName, 
  save_outputs = FALSE)
sp_list_ter_steege_etal_2016_parse_names

sp_list_ter_steege_etal_2016_parse_names <- sp_list_ter_steege_etal_2016_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
sp_list_ter_steege_etal_2016_parse_names

sp_list_ter_steege_etal_2016 <- sp_list_ter_steege_etal_2016 %>% 
  dplyr::bind_cols(sp_list_ter_steege_etal_2016_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
sp_list_ter_steege_etal_2016


sp_list_ter_steege_etal_2019a_parse_names <- bdc::bdc_clean_names(
  sci_names = sp_list_ter_steege_etal_2019a$verbatimScientificName, 
  save_outputs = FALSE)
sp_list_ter_steege_etal_2019a_parse_names

sp_list_ter_steege_etal_2019a_parse_names <- sp_list_ter_steege_etal_2019a_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
sp_list_ter_steege_etal_2019a_parse_names

sp_list_ter_steege_etal_2019a <- sp_list_ter_steege_etal_2019a %>% 
  dplyr::bind_cols(sp_list_ter_steege_etal_2019a_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
sp_list_ter_steege_etal_2019a


sp_list_ter_steege_etal_2019b_parse_names <- bdc::bdc_clean_names(
  sci_names = sp_list_ter_steege_etal_2019b$verbatimScientificName, 
  save_outputs = FALSE)
sp_list_ter_steege_etal_2019b_parse_names

sp_list_ter_steege_etal_2019b_parse_names <- sp_list_ter_steege_etal_2019b_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
sp_list_ter_steege_etal_2019b_parse_names

sp_list_ter_steege_etal_2019b <- sp_list_ter_steege_etal_2019b %>% 
  dplyr::bind_cols(sp_list_ter_steege_etal_2019b_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
sp_list_ter_steege_etal_2019b


sp_list_ter_steege_etal_2020_parse_names <- bdc::bdc_clean_names(
  sci_names = sp_list_ter_steege_etal_2020$verbatimScientificName, 
  save_outputs = FALSE)
sp_list_ter_steege_etal_2020_parse_names

sp_list_ter_steege_etal_2020_parse_names <- sp_list_ter_steege_etal_2020_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
sp_list_ter_steege_etal_2020_parse_names

sp_list_ter_steege_etal_2020 <- sp_list_ter_steege_etal_2020 %>% 
  dplyr::bind_cols(sp_list_ter_steege_etal_2020_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2)
sp_list_ter_steege_etal_2020

## names harmonization ----
sp_list_domingos_etal_2017_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = sp_list_domingos_etal_2017$names_clean,
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
sp_list_domingos_etal_2017_query_names

sp_list_ter_steege_etal_2013_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = sp_list_ter_steege_etal_2013$names_clean,
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
sp_list_ter_steege_etal_2013_query_names

sp_list_ter_steege_etal_2015_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = sp_list_ter_steege_etal_2015$names_clean,
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
sp_list_ter_steege_etal_2015_query_names

sp_list_ter_steege_etal_2016_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = sp_list_ter_steege_etal_2016$names_clean,
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
sp_list_ter_steege_etal_2016_query_names

sp_list_ter_steege_etal_2019a_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = sp_list_ter_steege_etal_2019a$names_clean,
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
sp_list_ter_steege_etal_2019a_query_names

sp_list_ter_steege_etal_2019b_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = sp_list_ter_steege_etal_2019b$names_clean,
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
sp_list_ter_steege_etal_2019b_query_names

sp_list_ter_steege_etal_2020_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = sp_list_ter_steege_etal_2020$names_clean,
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
sp_list_ter_steege_etal_2020_query_names

## export ----
readr::write_csv(sp_list_domingos_etal_2017_query_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_domingos_etal_2017_query_names")
readr::write_csv(sp_list_ter_steege_etal_2013_query_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2013_query_names.csv")
readr::write_csv(sp_list_ter_steege_etal_2015_query_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2015_query_names.csv")
readr::write_csv(sp_list_ter_steege_etal_2016_query_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2016_query_names.csv")
readr::write_csv(sp_list_ter_steege_etal_2019a_query_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019a_query_names.csv")
readr::write_csv(sp_list_ter_steege_etal_2019b_query_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019b_query_names.csv")
readr::write_csv(sp_list_ter_steege_etal_2020_query_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2020_query_names.csv")

## import ----
sp_list_domingos_etal_2017_query_names <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_domingos_etal_2017_query_names")
sp_list_ter_steege_etal_2013_query_names <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2013_query_names.csv")
sp_list_ter_steege_etal_2015_query_names <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2015_query_names.csv")
sp_list_ter_steege_etal_2016_query_names <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2016_query_names.csv")
sp_list_ter_steege_etal_2019a_query_names <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019a_query_names.csv")
sp_list_ter_steege_etal_2019b_query_names <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019b_query_names.csv")
sp_list_ter_steege_etal_2020_query_names <- readr::read_csv("01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2020_query_names.csv")

## combine ----
sp_list_domingos_etal_2017_taxonomy <- sp_list_domingos_etal_2017 %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., sp_list_domingos_etal_2017_query_names)
sp_list_domingos_etal_2017_taxonomy

sp_list_ter_steege_etal_2013_taxonomy <- sp_list_ter_steege_etal_2013 %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., sp_list_ter_steege_etal_2013_query_names)
sp_list_ter_steege_etal_2013_taxonomy

sp_list_ter_steege_etal_2015_taxonomy <- sp_list_ter_steege_etal_2015 %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., sp_list_ter_steege_etal_2015_query_names)
sp_list_ter_steege_etal_2015_taxonomy

sp_list_ter_steege_etal_2016_taxonomy <- sp_list_ter_steege_etal_2016 %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., sp_list_ter_steege_etal_2016_query_names)
sp_list_ter_steege_etal_2016_taxonomy

sp_list_ter_steege_etal_2019a_taxonomy <- sp_list_ter_steege_etal_2019a %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., sp_list_ter_steege_etal_2019a_query_names)
sp_list_ter_steege_etal_2019a_taxonomy

sp_list_ter_steege_etal_2019b_taxonomy <- sp_list_ter_steege_etal_2019b %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., sp_list_ter_steege_etal_2019b_query_names)
sp_list_ter_steege_etal_2019b_taxonomy

sp_list_ter_steege_etal_2020_taxonomy <- sp_list_ter_steege_etal_2020 %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., sp_list_ter_steege_etal_2020_query_names)
sp_list_ter_steege_etal_2020_taxonomy

## report ----
sp_list_domingos_etal_2017_taxonomy_report <- bdc::bdc_create_report(
  data = sp_list_domingos_etal_2017_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
sp_list_domingos_etal_2017_taxonomy_report

sp_list_ter_steege_etal_2013_taxonomy_report <- bdc::bdc_create_report(
  data = sp_list_ter_steege_etal_2013_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
sp_list_ter_steege_etal_2013_taxonomy_report

sp_list_ter_steege_etal_2015_taxonomy_report <- bdc::bdc_create_report(
  data = sp_list_ter_steege_etal_2015_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
sp_list_ter_steege_etal_2015_taxonomy_report

sp_list_ter_steege_etal_2016_taxonomy_report <- bdc::bdc_create_report(
  data = sp_list_ter_steege_etal_2016_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
sp_list_ter_steege_etal_2016_taxonomy_report

sp_list_ter_steege_etal_2019a_taxonomy_report <- bdc::bdc_create_report(
  data = sp_list_ter_steege_etal_2019a_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
sp_list_ter_steege_etal_2019a_taxonomy_report

sp_list_ter_steege_etal_2019b_taxonomy_report <- bdc::bdc_create_report(
  data = sp_list_ter_steege_etal_2019b_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
sp_list_ter_steege_etal_2019b_taxonomy_report

sp_list_ter_steege_etal_2020_taxonomy_report <- bdc::bdc_create_report(
  data = sp_list_ter_steege_etal_2020_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
sp_list_ter_steege_etal_2020_taxonomy_report

## unresolved names ----
sp_list_domingos_etal_2017_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = sp_list_domingos_etal_2017_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
sp_list_domingos_etal_2017_taxonomy_unresolved_names

sp_list_ter_steege_etal_2013_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2013_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
sp_list_ter_steege_etal_2013_taxonomy_unresolved_names

sp_list_ter_steege_etal_2015_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2015_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
sp_list_ter_steege_etal_2015_taxonomy_unresolved_names

sp_list_ter_steege_etal_2016_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2016_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
sp_list_ter_steege_etal_2016_taxonomy_unresolved_names

sp_list_ter_steege_etal_2019a_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2019a_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
sp_list_ter_steege_etal_2019a_taxonomy_unresolved_names

sp_list_ter_steege_etal_2019b_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2019b_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
sp_list_ter_steege_etal_2019b_taxonomy_unresolved_names

sp_list_ter_steege_etal_2020_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2020_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
sp_list_ter_steege_etal_2020_taxonomy_unresolved_names

## filter ----
sp_list_domingos_etal_2017_taxonomy_output <- bdc::bdc_filter_out_names(
  data = sp_list_domingos_etal_2017_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
sp_list_domingos_etal_2017_taxonomy_output

sp_list_ter_steege_etal_2013_taxonomy_output <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2013_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
sp_list_ter_steege_etal_2013_taxonomy_output

sp_list_ter_steege_etal_2015_taxonomy_output <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2015_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
sp_list_ter_steege_etal_2015_taxonomy_output

sp_list_ter_steege_etal_2016_taxonomy_output <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2016_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
sp_list_ter_steege_etal_2016_taxonomy_output

sp_list_ter_steege_etal_2019a_taxonomy_output <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2019a_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
sp_list_ter_steege_etal_2019a_taxonomy_output

sp_list_ter_steege_etal_2019b_taxonomy_output <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2019b_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
sp_list_ter_steege_etal_2019b_taxonomy_output

sp_list_ter_steege_etal_2020_taxonomy_output <- bdc::bdc_filter_out_names(
  data = sp_list_ter_steege_etal_2020_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
sp_list_ter_steege_etal_2020_taxonomy_output

## subspecies and variation ----
sp_list_domingos_etal_2017_taxonomy_output <- sp_list_domingos_etal_2017_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, .keep_all = TRUE)
sp_list_domingos_etal_2017_taxonomy_output

sp_list_ter_steege_etal_2013_taxonomy_output <- sp_list_ter_steege_etal_2013_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, .keep_all = TRUE)
sp_list_ter_steege_etal_2013_taxonomy_output

sp_list_ter_steege_etal_2015_taxonomy_output <- sp_list_ter_steege_etal_2015_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, .keep_all = TRUE)
sp_list_ter_steege_etal_2015_taxonomy_output

sp_list_ter_steege_etal_2016_taxonomy_output <- sp_list_ter_steege_etal_2016_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, .keep_all = TRUE)
sp_list_ter_steege_etal_2016_taxonomy_output

sp_list_ter_steege_etal_2019a_taxonomy_output <- sp_list_ter_steege_etal_2019a_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, .keep_all = TRUE)
sp_list_ter_steege_etal_2019a_taxonomy_output

sp_list_ter_steege_etal_2019b_taxonomy_output <- sp_list_ter_steege_etal_2019b_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, .keep_all = TRUE)
sp_list_ter_steege_etal_2019b_taxonomy_output

sp_list_ter_steege_etal_2020_taxonomy_output <- sp_list_ter_steege_etal_2020_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, .keep_all = TRUE)
sp_list_ter_steege_etal_2020_taxonomy_output

## export ----
readr::write_csv(sp_list_domingos_etal_2017_taxonomy_output, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_domingos_etal_2017_taxonomy_output.csv")
readr::write_csv(sp_list_domingos_etal_2017_taxonomy_report$x$data, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_domingos_etal_2017_taxonomy_report.csv")
readr::write_csv(sp_list_domingos_etal_2017_taxonomy_unresolved_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_domingos_etal_2017_taxonomy_unresolved_names.csv")

readr::write_csv(sp_list_ter_steege_etal_2013_taxonomy_output, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2013_taxonomy_output.csv")
readr::write_csv(sp_list_ter_steege_etal_2013_taxonomy_report$x$data, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2013_taxonomy_report.csv")
readr::write_csv(sp_list_ter_steege_etal_2013_taxonomy_unresolved_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2013_taxonomy_unresolved_names.csv")

readr::write_csv(sp_list_ter_steege_etal_2015_taxonomy_output, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2015_taxonomy_output.csv")
readr::write_csv(sp_list_ter_steege_etal_2015_taxonomy_report$x$data, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2015_taxonomy_report.csv")
readr::write_csv(sp_list_ter_steege_etal_2015_taxonomy_unresolved_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2015_taxonomy_unresolved_names.csv")

readr::write_csv(sp_list_ter_steege_etal_2016_taxonomy_output, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2016_taxonomy_output.csv")
readr::write_csv(sp_list_ter_steege_etal_2016_taxonomy_report$x$data, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2016_taxonomy_report.csv")
readr::write_csv(sp_list_ter_steege_etal_2016_taxonomy_unresolved_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2016_taxonomy_unresolved_names.csv")

readr::write_csv(sp_list_ter_steege_etal_2019a_taxonomy_output, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019a_taxonomy_output.csv")
readr::write_csv(sp_list_ter_steege_etal_2019a_taxonomy_report$x$data, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019a_taxonomy_report.csv")
readr::write_csv(sp_list_ter_steege_etal_2019a_taxonomy_unresolved_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019a_taxonomy_unresolved_names.csv")

readr::write_csv(sp_list_ter_steege_etal_2019b_taxonomy_output, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019b_taxonomy_output.csv")
readr::write_csv(sp_list_ter_steege_etal_2019b_taxonomy_report$x$data, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019b_taxonomy_report.csv")
readr::write_csv(sp_list_ter_steege_etal_2019b_taxonomy_unresolved_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2019b_taxonomy_unresolved_names.csv")

readr::write_csv(sp_list_ter_steege_etal_2020_taxonomy_output, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2020_taxonomy_output.csv")
readr::write_csv(sp_list_ter_steege_etal_2020_taxonomy_report$x$data, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2020_taxonomy_report.csv")
readr::write_csv(sp_list_ter_steege_etal_2020_taxonomy_unresolved_names, "01_data/01_occurrences/09_species_list_amazon/01_stand_taxonomy/sp_list_ter_steege_etal_2020_taxonomy_unresolved_names.csv")

# end ---------------------------------------------------------------------