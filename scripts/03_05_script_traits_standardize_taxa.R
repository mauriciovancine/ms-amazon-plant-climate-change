#' ---
#' title: obj2 - traits - standardization taxa
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

# taxa stand ------------------------------------------------------

# import data
trait_std <- arrow::read_parquet("01_data/03_traits/04_standardization_traits/traits_standardize.parquet") 
trait_std

## clean and parse species names ----
trait_std_parse_names <- bdc::bdc_clean_names(
  sci_names = trait_std$verbatimScientificName, 
  save_outputs = FALSE)
trait_std_parse_names

trait_std_parse_names <- trait_std_parse_names %>% 
  dplyr::select(.uncer_terms, names_clean)
trait_std_parse_names

trait_std <- dplyr::bind_cols(trait_std, 
                              trait_std_parse_names) %>% 
  dplyr::mutate(names_clean_n = str_count(names_clean, "\\S+")) %>%
  dplyr::filter(names_clean_n >= 2) 
trait_std

## names harmonization ----
trait_std_query_names <- bdc::bdc_query_names_taxadb(
  sci_name            = trait_std$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = TRUE, # should parallel processing be used?
  ncores              = parallel::detectCores(logical = TRUE) - 2, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
trait_std_query_names

## export ----
arrow::write_parquet(trait_std_query_names, "01_data/03_traits/05_standardization_taxa/trait_std_query_names.parquet")
trait_std_query_names <- arrow::read_parquet("01_data/03_traits/05_standardization_taxa/trait_std_query_names.parquet")

## combine ----
trait_std_taxonomy <- trait_std %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., trait_std_query_names)
trait_std_taxonomy

## report ----
trait_std_taxonomy_report <- bdc::bdc_create_report(
  data = trait_std_taxonomy,
  database_id = "database_id",
  workflow_step = "taxonomy",
  save_report = FALSE)
trait_std_taxonomy_report

## unresolved names ----
trait_std_taxonomy_unresolved_names <- bdc::bdc_filter_out_names(
  data = trait_std_taxonomy,
  col_name = "notes",
  taxonomic_status = "accepted",
  opposite = TRUE)
trait_std_taxonomy_unresolved_names

## filter ----
trait_std_taxonomy_output <- bdc::bdc_filter_out_names(
  data = trait_std_taxonomy,
  taxonomic_status = "accepted",
  opposite = FALSE)
trait_std_taxonomy_output

## subspecies and variation ----
trait_std_taxonomy_output <- trait_std_taxonomy_output %>% 
  dplyr::mutate(scientificNameOriginal = scientificName, .before = scientificName) %>% 
  dplyr::mutate(scientificName = str_c(word(scientificName, 1),
                                       word(scientificName, 2),
                                       sep = " ")) %>% 
  dplyr::distinct(scientificName, traitName, traitValue, .keep_all = TRUE) %>% 
  dplyr::select(database_id,
                scientificName, traitName, traitValue, traitUnit, valueType, factorLevels, traitID, traitDescription,
                verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
                taxonID, measurementID, datasetID, datasetName, 
                author, bibliographicCitation, warnings,
                taxonRank, kingdom, phylum, class, order, family, genus)
trait_std_taxonomy_output

## export ----
arrow::write_parquet(trait_std_taxonomy_output, "01_data/03_traits/05_standardization_taxa/trait_std_taxonomy_output.parquet")
arrow::write_parquet(trait_std_taxonomy_report$x$data, "01_data/03_traits/05_standardization_taxa/trait_std_taxonomy_report.parquet")
arrow::write_parquet(trait_std_taxonomy_unresolved_names, "01_data/03_traits/05_standardization_taxa/trait_std_taxonomy_unresolved_names.parquet")

# end ---------------------------------------------------------------------