#' ----
#' title: obj2 - traits - filter traits
#' author: mauricio vancine
#' date: 2026-06-02
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)

# import data -------------------------------------------------------------

## try ----
trait_try <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_try_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_try

trait_try_cat <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_try_cat_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_try_cat

trait_try_form <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_try_form_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_try_form

## bien ----
trait_bien <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_bien_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_bien$verbatimTraitName

## gift ----
trait_gift <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_gift_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_gift

## neotroptree ----
trait_neotroptree <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_neotroptree_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_neotroptree

## domingos ----
trait_domingos <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_domingos_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_domingos

## seed_trait ----
trait_seed_trait <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_seed_trait_ets_prep.parquet") %>% 
  dplyr::select(database_id, 
                scientificName, verbatimScientificName, 
                traitName, traitValue, traitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings) %>% 
  dplyr::rename(stdScientificName = scientificName,
                verbatimTraitName = traitName, 
                verbatimTraitValue = traitValue, 
                verbatimTraitUnit = traitUnit) %>% 
  dplyr::select(database_id, 
                verbatimScientificName, stdScientificName, 
                verbatimTraitName, verbatimTraitValue, verbatimTraitUnit, 
                measurementID, datasetID, datasetName, author, 
                bibliographicCitation, warnings)
trait_seed_trait

# filter ------------------------------------------------------------------

## traits ----
trait_map_try <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "sla", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded", "numerical",
  "ldmc", "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)", "numerical",
  "leaf_n", "Leaf nitrogen (N) content per leaf dry mass", "numerical",
  "leaf_p", "Leaf phosphorus (P) content per leaf dry mass", "numerical",
  "height", "Plant height vegetative", "numerical",
  "seed_mass", "Seed dry mass", "numerical",
  "ssd", "Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)", "numerical",
  "leaf_thickness", "Leaf thickness", "numerical",
  
  "dispersal", "Dispersal syndrome", "categorical",
  "resprout", "Plant resprouting capacity", "categorical",
  "growth_form", "Plant growth form", "categorical",
  "mycorrhiza", "Mycorrhiza type", "categorical",
  "n_fix", "Plant nitrogen(N) fixation capacity", "categorical")
trait_map_try

trait_map_try_cat <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "growth_form", "PlantGrowthForm", "categorical",
  "woodiness", "Woodiness", "categorical")
trait_map_try_cat

trait_map_try_form <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "sla", "LMA (g/m2)", "numerical",
  "ldmc", "LDMC (g/g)", "numerical",
  "leaf_n", "Nmass (mg/g)", "numerical",
  "height", "Plant height (m)", "numerical",
  "seed_mass", "Diaspore mass (mg)", "numerical",
  "ssd", "SSD observed (mg/mm3)", "numerical",

  "growth_form", "Growth Form", "categorical",
  "woodiness", "Woodiness", "categorical")
trait_map_try_form

trait_map_bien <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "sla", "leaf area per leaf dry mass", "numerical",
  "ldmc", "leaf dry mass per leaf fresh mass", "numerical",
  "leaf_n", "leaf nitrogen content per leaf dry mass", "numerical",
  "leaf_p", "leaf phosphorus content per leaf dry mass", "numerical",
  "height", "whole plant height", "numerical",
  "dbh", "diameter at breast height (1.3 m)", "numerical",
  "seed_mass", "seed mass", "numerical",
  "ssd", "stem wood density", "numerical",
  
  "dispersal", "whole plant dispersal syndrome", "categorical",
  "growth_form", "whole plant growth form", "categorical",
  "leaf_lifespan", "leaf life span", "categorical")
trait_map_bien

trait_map_gift <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "sla", "SLA_mean", "numerical",
  "height", "Plant_height_mean", "numerical",
  "dbh", "DBH_max", "numerical",
  "seed_mass", "Seed_mass_mean", "numerical",
  "ssd", "SSD_mean", "numerical",
  "leaf_thickness", "Leaf_thickness_mean", "numerical",
  
  "dispersal", "Dispersal_syndrome_1", "categorical",
  "growth_form", "Growth_form_1", "categorical",
  "mycorrhiza", "Mycorrhiza_1", "categorical",
  "n_fix", "Nitrogen_fix_1", "categorical",)
trait_map_gift

trait_map_neotroptree <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "height", "Potential height", "numerical",
  "growth_form", "Growth habits", "categorical",)
trait_map_neotroptree

trait_map_domingos <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "growth_form", "life.form", "categorical",)
trait_map_domingos

trait_map_seed_trait <- tibble::tribble(
  ~traitName, ~verbatimTraitName, ~valueType,
  
  "seed_mass", "individualSeedMass", "numerical",
  "dispersal", "dispersalSyndrome", "categorical",
  "growth_form", "growthForm", "categorical")
trait_map_seed_trait

## try ----
trait_try_filtered <- trait_try %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_try$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_try) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_try_filtered

trait_try_cat_filtered <- trait_try_cat %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_try_cat$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_try_cat) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_try_cat_filtered

trait_try_form_filtered <- trait_try_form %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_try_form$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_try_form) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_try_form_filtered

## bien ----
trait_bien_filtered <- trait_bien %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_bien$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_bien) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_bien_filtered

## gift ----
trait_gift_filtered <- trait_gift %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_gift$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_gift) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_gift_filtered

## neotroptree ----
trait_neotroptree_filtered <- trait_neotroptree %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_neotroptree$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_neotroptree) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_neotroptree_filtered

## domingos ----
trait_domingos_filtered <- trait_domingos %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_domingos$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_domingos) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_domingos_filtered

## seed_trait ----
trait_seed_trait_filtered <- trait_seed_trait %>% 
  tidyr::drop_na(verbatimScientificName, verbatimTraitName, verbatimTraitValue) %>% 
  dplyr::distinct(verbatimScientificName, verbatimTraitName, verbatimTraitValue, .keep_all = TRUE) %>% 
  dplyr::filter(verbatimTraitName %in% trait_map_seed_trait$verbatimTraitName) %>% 
  dplyr::left_join(trait_map_seed_trait) %>% 
  dplyr::relocate(traitName, .after = stdScientificName) %>% 
  dplyr::relocate(valueType, .after = verbatimTraitUnit)
trait_seed_trait_filtered

# export ------------------------------------------------------------------

## export ----
arrow::write_parquet(trait_try_filtered, "01_data/03_traits/03_filtered/trait_try_filtered.parquet")
arrow::write_parquet(trait_try_cat_filtered, "01_data/03_traits/03_filtered/trait_try_cat_filtered.parquet")
arrow::write_parquet(trait_try_form_filtered, "01_data/03_traits/03_filtered/trait_try_form_filtered.parquet")
arrow::write_parquet(trait_bien_filtered, "01_data/03_traits/03_filtered/trait_bien_filtered.parquet")
arrow::write_parquet(trait_gift_filtered, "01_data/03_traits/03_filtered/trait_gift_filtered.parquet")
arrow::write_parquet(trait_neotroptree_filtered, "01_data/03_traits/03_filtered/trait_neotroptree_filtered.parquet")
arrow::write_parquet(trait_domingos_filtered, "01_data/03_traits/03_filtered/trait_domingos_filtered.parquet")
arrow::write_parquet(trait_seed_trait_filtered, "01_data/03_traits/03_filtered/trait_seed_trait_filtered.parquet")

# end ---------------------------------------------------------------------