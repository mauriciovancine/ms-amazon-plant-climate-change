#' ----
#' title: obj2 - traits - prepare
#' author: mauricio vancine
#' date: 2026-06-02
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(arrow)

# metadata
# Extended MeasurementOrFact (eMoF): https://ets.tdwg.org/terms/#measurementorfact 

# try -------------------------------------------------------------------

## columns try ----
# LastName -	Surname of data contributor
# FirstName -	First name of data contributor
# DatasetID -	Unique identifier of contributed dataset
# Dataset -	Name of contributed dataset
# SpeciesName -	Original name of species
# AccSpeciesID -	Unique identifier of consolidated species name
# AccSpeciesName -	Consolidated species name
# ObservationID -	Unique identifier for each observation in TRY
# ObsDataID -	Unique identifier for each row in the TRY data table, either trait record or ancillary data
# TraitID -	Unique identifier for traits (only if the record is a trait)
# TraitName -	Name of trait (only if the record is a trait)
# DataID -	Unique identifier for each DataName (either sub-trait or ancillary data)
# DataName -	Name of sub-trait or ancillary data
# OriglName -	Original name of sub-trait or ancillary data
# OrigValueStr -	Original value of trait or ancillary data
# OrigUnitStr -	Original unit of trait or ancillary data
# ValueKindName -	Value kind (single measurement, mean, median, etc.)
# OrigUncertaintyStr -	Original uncertainty
# UncertaintyName -	Kind of uncertainty (standard deviation, standard error, etc.)
# Replicates -	Number of replicates
# StdValue -	Standardized trait value: available for frequent continuous traits
# UnitName -	Standard unit: available for frequent continuous traits
# RelUncertaintyPercent -	Relative uncertainty in %
# OrigObsDataID -	Unique identifier for duplicate trait records
# ErrorRisk -	Indication for outlier trait values: distance to mean in standard deviations
# Reference -	Reference to be cited if trait record is used in analysis
# Comment -	Explanation for the OriglName in the contributed dataset
# V28 -	Empty, an artifact due to different interpretation of column separator by MySQL and R

## columns categorical ----
# AccSpeciesID - Unique identifier of the species name as accepted in the context of the TRY database
# AccSpeciesName - Binary name of species as accepted in the context of the TRY database
# IPNI/TROPICOS - Accepted species name verified against IPNI/Tropicos Checklist as accessed 2008 from the SALVIAS website (http://www.salvias.net/pages/index.html)
# Genus - The genus of the accepted species
# SpeciesEpithet - The species epithet of the accepted species
# Family - The family of the accepted species
# PhylogeneticGroup - The phylogenetic group of the accepted species

# PlantGrowthForm - Plant growth form (fern, lichen, moss, herb, graminoid, shrub, tree)
# Succulent - Succulent (succulent, stem succulent, leaf succulent)
# Climber - Climber (climber)
# Parasitic - Parasitic (parasitic. hemiparasitic)
# Aquatic - Aquatic (aquatic, mangrove, submerged, water plant)
# Epiphyte - Epiphyte (epiphyte, hemiepiphyte)
# Crop - Crop (crop)
# Palmoid - Palmoid (palmoid)
# LeafType - Leaf type (broadleaved, needleleaved)
# LeafPhenology - Leaf phenology (deciduous, evergreen)
# PhotosyntheticPathway - Photosynthetic pathway (C3, C4, CAM)
# Woodiness - Woodiness (woody, non woody)
# WoodinessDetail - Woodiness detail (woody at base, woody rootstock)
# LeafCompoundness - Leaf compoundness (simple, compound)
# NumberOfLeaflets - Number of leaflets

# PlantGrowthFormSource - Source of data
# SucculentSource - Source of data
# ClimbingSource - Source of data
# ParasiticSource - Source of data
# AquaticSource - Source of data
# EpiphyticSource - Source of data
# CropSource - Source of data
# PalmoidSource - Source of data
# PlantGrowthFormAttributesSource - Source of data
# LeafTypeSource - Source of data
# LeafPhenSource - Source of data
# PhotoSource - Source of data
# WoodinessSource - Source of data
# LeafCompoundnessSource - Source of data

## columns life form ----
# TRY 3.0 AccSpecies ID - Unique identifier for the species in the TRY Database version 3.0
# Species name standardized against TPL - TPL-consolidated name
# Taxonomic level - Genus, species, variety or subspecies
# Status according to TPL - Status according to TPL; unrecognized are those species not in TPL but seem to be valid according to other reputable sources, such as well-known national floras. All the ones in this list were included in Díaz et al. 2016 and therefore should be included by those that want to replicate the analysis. The general approach was to include all those entities that were reasonably well defined and not duplicated in the dataset, even if their name was not fully accepted or was not defined (e.g. in the case of entities defined only at the level of genus, but which we knew was not a duplicate).
# Genus - According to TPL
# Family - According to TPL
# Phylogenetic Group within Angiosperms - Angiosperm (Magnoliid, Monocotyledon, other Eudicotyledon) , non-Angiosperm
# Phylogenetic Group General - Angiosperm, Gymnosperm, Pteridophyte
# Adaptation to aquatic or terrestrial habitats - aquatic, aquatic/semiaquatic, semiaquatic, terrestrial
# Woodiness - woody, semi-woody, non-woody
# Growth Form - bamboo gaminoid, climber, fern, herbaceous gaminoid, herbaceous non-graminoid, herbaceous non-graminoid/shrub, other, succulent, shrub, shrub/tree, tree
# Succulence - leaf and stem succulent, leaf rosette and stem succulent, leaf rosette succulent, leaf rosette succulent (tall), leaf succulent, stem succulent, stem succulent (short), stem succulent (tall), succulent
# Nutrition type (parasitism) - hemiparasitic, holoparasitic, independent, parasitic
# Nutrition type (carnivory) - carnivorous, detritivorous
# Leaf type - broadleaved, needleleaved, photosynthetic stem, scale-shaped, scale-shaped/needleleaved
# Leaf Area (mm2) - Area of the smallest lamina unit (leaf or leaflet)
# Leaf Area (n.o.) - Number of trait measurements from the TRY database before exclusion of non-representative data
# Nmass (mg/g) - Leaf nitrogen content per leaf dry mass
# Nmass (n.o.) - Number of trait measurements from the TRY database before exclusion of non-representative data
# LMA (g/m2) - Leaf dry mass per leaf area
# LMA (n.o.) - Number of trait measurements from the TRY database before exclusion of non-representative data
# Plant Height (m) - Vegetative plant height
# Plant Height (n.o.) - Number of trait measurements from the TRY database before exclusion of non-representative data
# Diaspore Mass (mg) - Seed or spore dry mass, respectively
# Diaspore Mass (n.o.) - Number of trait measurements from the TRY database before exclusion of non-representative data
# SSD_obs (mg/mm3) - Stem specific density: stem dry mass per stem fresh volume, measured
# SSD (n.o.) - Number of trait measurements from the TRY database before exclusion of non-representative data
# LDMC (g/g) - Leaf dry matter content: leaf dry mass per leaf rehydrated fresh mass
# LDMC (n.o.) - Number of trait measurements from the TRY database before exclusion of non-representative data
# SSD_filled (mg/mm3) - Stem density for herbaceous plants calculated from LDMC (see data descriptor)
# SSD combined (mg/mm3) - Stem density inlcuding directily observed and estimated values for stem specific density
# Number of traits with values - Number of species mean trait values provided per species, from 1 to 6

## import ----
trait_try01 <- arrow::read_parquet("01_data/03_traits/01_raw/00_try/try01.parquet") %>% 
  dplyr::mutate(across(everything(), as.character))
trait_try01

trait_try02 <- arrow::read_parquet("01_data/03_traits/01_raw/00_try/try02.parquet") %>% 
  dplyr::mutate(across(everything(), as.character))
trait_try02

trait_try03 <- arrow::read_parquet("01_data/03_traits/01_raw/00_try/try03.parquet") %>% 
  dplyr::mutate(across(everything(), as.character))
trait_try03

trait_try04 <- arrow::read_parquet("01_data/03_traits/01_raw/00_try/try04.parquet") %>% 
  dplyr::mutate(across(everything(), as.character))
trait_try04

trait_try05 <- arrow::read_parquet("01_data/03_traits/01_raw/00_try/try05.parquet") %>% 
  dplyr::mutate(across(everything(), as.character))
trait_try05

trait_try <- dplyr::bind_rows(
  trait_try01, trait_try02, trait_try03,
  trait_try04, trait_try05)
trait_try


trait_try_cat <- readxl::read_excel("01_data/03_traits/01_raw/00_try/00_raw/Try202651944642TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.xlsx") %>% 
  dplyr::mutate(across(everything(), as.character))
trait_try_cat

trait_try_cat_value <- trait_try_cat %>%
  dplyr::select(AccSpeciesID, AccSpeciesName, PlantGrowthForm:NumberOfLeaflets) %>% 
  tidyr::pivot_longer(
    cols = -c(AccSpeciesID, AccSpeciesName),
    names_to = "trait",
    values_to = "value") %>% 
  dplyr::distinct()
trait_try_cat_value

trait_try_cat_source <- trait_try_cat %>%
  dplyr::select(AccSpeciesID, AccSpeciesName, PlantGrowthFormSource:LeafCompoundnessSource) %>% 
  tidyr::pivot_longer(
    cols = -c(AccSpeciesID, AccSpeciesName),
    names_to = "trait",
    values_to = "source") %>% 
  dplyr::mutate(trait = sub("Source", "", trait)) %>% 
  dplyr::distinct()
trait_try_cat_source

trait_try_cat <- trait_try_cat_value %>% 
  dplyr::left_join(trait_try_cat_source)
trait_try_cat


trait_try_form <- readxl::read_excel("01_data/03_traits/01_raw/00_try/00_raw/Try20266844323480_Dataset/Dataset/Species_mean_traits.xlsx") %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  dplyr::select(1, 2, 9:16, 18, 20, 22, 24, 26, 28, 30, 31) %>% 
  tidyr::pivot_longer(
    cols = -c(1, 2),
    names_to = "trait",
    values_to = "value") %>% 
  dplyr::mutate(unit = case_when(
    trait == "Diaspore mass (mg)" ~ "mg",
    trait == "LDMC (g/g)" ~ "g/g",
    trait == "LMA (g/m2)" ~ "g/m2",
    trait == "Leaf area (mm2)" ~ "mm2",
    trait == "Nmass (mg/g)" ~ "mg/g",
    trait == "Plant height (m)" ~ "m",
    trait == "SSD combined (mg/mm3)" ~ "mg/mm3",
    trait == "SSD imputed (mg/mm3)" ~ "mg/mm3",
    trait == "SSD observed (mg/mm3)" ~ "mg/mm3"))
trait_try_form

## prepare ----
trait_try_ets <- trait_try %>%
  dplyr::mutate(
    traitID = TraitID,
    scientificName = AccSpeciesName,
    traitName = TraitName,
    traitValue = as.character(StdValue),
    traitUnit  = UnitName,
    verbatimScientificName = SpeciesName,
    verbatimTraitName = OriglName,
    verbatimTraitValue = OrigValueStr,
    verbatimTraitUnit = OrigUnitStr,
    taxonID = AccSpeciesID,
    measurementID = ObsDataID,
    occurrenceID = ObservationID,
    warnings = ErrorRisk,
    datasetID = DatasetID,
    datasetName = Dataset,
    author = stringr::str_trim(paste(FirstName, LastName)),
    bibliographicCitation = Reference,
    decimalLongitude = NA,
    decimalLatitude = NA, 
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(database_id = paste0("TRY_", 1:nrow(.)), .before = 1)
trait_try_ets

trait_try_cat_ets <- trait_try_cat %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = AccSpeciesName,
    traitName = trait,
    traitValue = as.character(value),
    traitUnit  = NA,
    verbatimScientificName = AccSpeciesName,
    verbatimTraitName = trait,
    verbatimTraitValue = value,
    verbatimTraitUnit = NA,
    taxonID = AccSpeciesID,
    measurementID = NA,
    occurrenceID = NA,
    warnings = NA,
    datasetID = NA,
    datasetName = NA,
    author = NA,
    bibliographicCitation = source,
    decimalLongitude = NA,
    decimalLatitude = NA, 
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(database_id = paste0("TRY_CATEGORICAL_", 1:nrow(.)), .before = 1)
trait_try_cat_ets

trait_try_form_ets <- trait_try_form %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = `Species name standardized against TPL`,
    traitName = trait,
    traitValue = as.character(value),
    traitUnit  = unit,
    verbatimScientificName = `Species name standardized against TPL`,
    verbatimTraitName = trait,
    verbatimTraitValue = value,
    verbatimTraitUnit = unit,
    taxonID = `TRY 30 AccSpecies ID`,
    measurementID = NA,
    occurrenceID = NA,
    warnings = NA,
    datasetID = NA,
    datasetName = NA,
    author = NA,
    bibliographicCitation = NA,
    decimalLongitude = NA,
    decimalLatitude = NA, 
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(database_id = paste0("TRY_LIFEFORM_", 1:nrow(.)), .before = 1)
trait_try_form_ets


## export ----
arrow::write_parquet(trait_try_ets, "01_data/03_traits/02_prepared/trait_try_prep.parquet")
arrow::write_parquet(trait_try_cat_ets, "01_data/03_traits/02_prepared/trait_try_cat_prep.parquet")
arrow::write_parquet(trait_try_form_ets, "01_data/03_traits/02_prepared/trait_try_form_prep.parquet")

# bien --------------------------------------------------------------------

## import ----
trait_bien_files <- dir(path = "01_data/03_traits/01_raw/01_bien", full.names = TRUE)
trait_bien_files

trait_bien <- purrr::map_dfr(.x = trait_bien_files, .f = arrow::read_parquet)
trait_bien

## prepare ----
trait_bien_ets <- trait_bien %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = scrubbed_species_binomial,
    traitName = trait_name,
    traitValue = trait_value,
    traitUnit = unit,
    verbatimScientificName = verbatim_scientific_name,
    verbatimTraitName = trait_name,
    verbatimTraitValue = trait_value,
    verbatimTraitUnit = unit,
    taxonID = scrubbed_taxon_canonical,
    measurementID = id,
    occurrenceID = id,
    warnings = tnrs_warning,
    datasetID = id,
    datasetName = NA,
    author = project_pi,
    bibliographicCitation = source_citation,
    decimalLongitude = longitude,
    decimalLatitude = latitude,
    elevation = elevation_m) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(database_id = paste0("BIEN_", 1:nrow(.)), .before = 1)
trait_bien_ets

## export ----
arrow::write_parquet(trait_bien_ets, "01_data/03_traits/02_prepared/trait_bien_prep.parquet")

# gift --------------------------------------------------------------------

## columns ----
# trait_derived_ID - Identification number of the trait record in the database
# ref_ID - Identification number of the reference
# orig_ID - Identification number of the species, as it came in the source
# trait_ID - Identification number of the trait
# trait_value - Value of the trait (coded as character, even for continuous trait)
# derived - Is the trait value derived from another information (e.g. phanerophytes are woody)
# bias_deriv - Is the derivation potentially introducing a bias
# bias_ref - Is the resource potentially introducing a bias
# name_ID - Identification number of the species before being resolved
# cf_genus - Whether the genus name is uncertain
# genus - Genus of the species
# cf_species - Whether the species' epithet is uncertain
# aff_species - Species' epithet uncertain
# species_epithet - Epithet of the species
# subtaxon - Sub-taxon name
# author - Author who described the species
# matched - Was the species name matched in the taxonomic backbone
# epithetscore - Matching score for the epithet
# overallscore - Overall matching score
# resolved - Was the species name resolved in the taxonomic backbone
# service - Taxonomic backbone used for taxonomic harmonization
# work_ID - Identification number of the taxonomically harmonized species
# genus_ID - Identification number of the taxonomically harmonized genus
# work_genus - Genus name (after taxonomic harmonization)
# work_species - Species name (after taxonomic harmonization)
# work_author - Name of the author who described the species
# geo_entity _ref - Name of the region of the reference
# ref_long - Full reference to cite

## metadata ----
trait_gift_meta <- GIFT::GIFT_traits_meta() %>% 
  tibble::as_tibble()
trait_gift_meta

## import ----
trait_gift_files_raw <- dir(path = "01_data/03_traits/01_raw/02_gift_raw/", full.names = TRUE)
trait_gift_files_raw

trait_gift <- purrr::map_dfr(.x = trait_gift_files_raw, .f = arrow::read_parquet)
trait_gift

trait_gift_meta_join_raw <- trait_gift_meta %>% 
  dplyr::select(Lvl3, Trait2, Units)
trait_gift_meta_join_raw

trait_gift_join_raw <- trait_gift %>% 
  dplyr::left_join(trait_gift_meta_join_raw, by = c("trait_ID" = "Lvl3"))
trait_gift_join_raw

## prepare ----
trait_gift_ets <- trait_gift_join_raw %>%
  dplyr::mutate(
    traitID = trait_ID,
    scientificName = work_species,
    traitName = as.character(Trait2),
    traitValue = trait_value,
    traitUnit = Units,
    verbatimScientificName = str_trim(paste(genus, species_epithet, author)),
    verbatimTraitName = as.character(Trait2),
    verbatimTraitValue = trait_value,
    verbatimTraitUnit = NA,
    taxonID = work_ID,
    measurementID = trait_derived_ID,
    occurrenceID = orig_ID,
    warnings = paste(
      ifelse(derived, "derived", NA),
      ifelse(bias_deriv, "bias_derivation", NA),
      ifelse(bias_ref, "bias_reference", NA),
      ifelse(!matched, "not_matched", NA),
      ifelse(!resolved, "not_resolved", NA),
      sep = ";"),
    datasetID = ref_ID,
    datasetName = NA,
    author = NA,
    bibliographicCitation = ref_long,
    decimalLongitude = NA,
    decimalLatitude = NA,
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(database_id = paste0("GIFT_", 1:nrow(.)), .before = 1)
trait_gift_ets

## export ----
arrow::write_parquet(trait_gift_ets, "01_data/03_traits/02_prepared/trait_gift_prep.parquet")

# neotroptree -----------------------------------------------------------

## import ----
trait_neotroptree <- readxl::read_excel("01_data/03_traits/01_raw/03_neotroptree/v01/Species table.xlsx") %>% 
  dplyr::select(Species, `Growth habits`, `Potential height`, SppID, `Main source`)
trait_neotroptree

trait_neotroptree_pivot <- trait_neotroptree %>%
  dplyr::mutate(`Potential height` = as.character(as.numeric(`Potential height`) * 100)) %>% 
  tidyr::pivot_longer(
    cols = -c(SppID, Species, `Main source`),
    names_to = "trait",
    values_to = "value")
trait_neotroptree_pivot

## prepare ---- 
trait_neotroptree_ets <- trait_neotroptree_pivot %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = Species,
    traitName = as.character(trait),
    traitValue = value,
    traitUnit = NA,
    verbatimScientificName = Species,
    verbatimTraitName = trait,
    verbatimTraitValue = value,
    verbatimTraitUnit = NA,
    taxonID = NA,
    measurementID = NA,
    occurrenceID = NA,
    warnings = NA,
    datasetID = NA,
    datasetName = NA,
    author = NA,
    bibliographicCitation = `Main source`,
    decimalLongitude = NA,
    decimalLatitude = NA,
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(traitUnit = if_else(traitName == "Potential height", "cm", traitUnit),
                verbatimTraitUnit = if_else(traitName == "Potential height", "cm", verbatimTraitUnit)) %>% 
  dplyr::mutate(database_id = paste0("NEOTROPTREE_", 1:nrow(.)), .before = 1)
trait_neotroptree_ets

## export ----
arrow::write_parquet(trait_neotroptree_ets, "01_data/03_traits/02_prepared/trait_neotroptree_prep.parquet")

# domingos 2017 -----------------------------------------------------------

## import ----
trait_domingos <- readxl::read_excel("01_data/03_traits/01_raw/04_domingos_etal_2017/pnas.1706756114.sd01.xlsx") %>% 
  dplyr::select(species, fullname, life.form) %>%
  dplyr::rename(value = life.form) %>% 
  dplyr::mutate(trait = "life.form")
trait_domingos

## prepare ---- 
trait_domingos_ets <- trait_domingos %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = species,
    traitName = as.character(trait),
    traitValue = value,
    traitUnit = NA,
    verbatimScientificName = fullname,
    verbatimTraitName = trait,
    verbatimTraitValue = value,
    verbatimTraitUnit = NA,
    taxonID = NA,
    measurementID = NA,
    occurrenceID = NA,
    warnings = NA,
    datasetID = NA,
    datasetName = NA,
    author = NA,
    bibliographicCitation = NA,
    decimalLongitude = NA,
    decimalLatitude = NA,
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(database_id = paste0("DOMINGOS2017_", 1:nrow(.)), .before = 1)
trait_domingos_ets

## export ----
arrow::write_parquet(trait_domingos_ets, "01_data/03_traits/02_prepared/trait_domingos_prep.parquet")

# seed trait -----------------------------------------------------------

## import ----
trait_seed_trait <- readr::read_csv("01_data/03_traits/01_raw/05_tropical_seed_trait_database/nph71268-sup-0001-dataset-S1.csv") %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  dplyr::select(-c(distribution, jw_distance.x, spelling_suggestion.y, jw_distance.y, spelling_suggestion.x)) %>% 
  tidyr::pivot_longer(cols = -c(datasetID:landUse, notes), 
                      names_to = "trait",
                      values_to = "value")
trait_seed_trait

trait_seed_trait_units <- tibble::tibble(
  trait = unique(trait_seed_trait$trait),
  unit = c("Herb,shrub,tree,vine",
           "Unassisted,Anemochory,Hidrochory,Epizoohory,Mirmechochory,Hoarding,Endozoochory,Endozoochory-birds,Endozoochory-bats, Endozoochory-primates",
           "Bent,Linear-underdeveloped,Linear-developed,Folded,Spatulate,Investing,Rudimentary,Dwarf",
           "ratio",
           "Black,Blue,Brown,Dark brown,Dark purple,Green,Light brown,Light red,Maroon,Orange,Pink,Purple,Red,Reddish brown,White,Yellow",
           "mm",
           "Fleshy,Dry",
           "mg",
           "mg",
           "n",
           "Dry,Wet,Dry and Wet",
           "µm",
           "Beige,Black,Brown,Green,Grey,Orange,Pale,Pink,Purple,Red,White,Yellow",
           "mm",
           "mm",
           "mm",
           "n",
           "Yes,No",
           "mpa",
           "Yes,No",
           "Desiccation-tolerant,Recalcitrant",
           "mg.g-1",
           "mg.g-1",
           "mg.g-1",
           "mg.g-1",
           "Glucosinolates,Alkaloids,Terpenoids,Saponins,Phenolics,Cyanogenic,Glycosides",
           "Yes,No",
           "%",
           "mg.g-1",
           "mg.g-1",
           "m.s-1",
           "ºC",
           "ºC",
           "%",
           "Non-dormant,Morphological Physiological,Physical,Morphophysiological,Combinational",
           "%",
           "%",
           "days",
           "%",
           "%",
           "days",
           "%",
           "%",
           "ºC",
           "Transient,Persistent",
           "%"))
trait_seed_trait_units

trait_seed_trait <- trait_seed_trait %>% 
  dplyr::left_join(trait_seed_trait_units)
trait_seed_trait %>% names()

## prepare ---- 
trait_seed_trait_ets <- trait_seed_trait %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = scientificName,
    traitName = as.character(trait),
    traitValue = value,
    traitUnit = unit,
    verbatimScientificName = species_reported,
    verbatimTraitName = trait,
    verbatimTraitValue = value,
    verbatimTraitUnit = unit,
    taxonID = NA,
    measurementID = NA,
    occurrenceID = occurrenceID,
    warnings = notes,
    datasetID = datasetID,
    datasetName = NA,
    author = NA,
    bibliographicCitation = NA,
    decimalLongitude = decimalLongitude,
    decimalLatitude = decimalLatitude,
    elevation = verbatimElevation) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(database_id = paste0("SEEDTRAIT_", 1:nrow(.)), .before = 1)
trait_seed_trait_ets

trait_seed_trait_ets %>% count(traitName)

## export ----
arrow::write_parquet(trait_seed_trait_ets, "01_data/03_traits/02_prepared/trait_seed_trait_ets_prep.parquet")

# draft -------------------------------------------------------------------

# palmtraits -----------------------------------------------------------

## import ----
trait_palmtraits_species <- data.table::fread("01_data/03_traits/01_raw/06_palmtraits/PalmTraits_1.0.txt") %>% 
  tibble::as_tibble()
trait_palmtraits_species

trait_palmtraits_refs_species <- data.table::fread("01_data/03_traits/01_raw/06_palmtraits/ReferenceToSpecies_PalmTraits_1.0.txt") %>% 
  tibble::as_tibble()
trait_palmtraits_refs_species

trait_palmtraits_species_pivot_join <- trait_palmtraits_species %>% 
  dplyr::mutate(across(where(is.numeric), as.character)) %>% 
  tidyr::pivot_longer(cols = -c(SpecName:PalmSubfamily),
                      names_to = "trait",
                      values_to = "value") %>% 
  dplyr::left_join(trait_palmtraits_refs_species)
trait_palmtraits_species_pivot_join

## prepare ---- 
trait_palmtraits_ets <- trait_palmtraits_species_pivot_join %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = SpecName,
    traitName = as.character(trait),
    traitValue = value,
    traitUnit = NA,
    verbatimScientificName = SpecName,
    verbatimTraitName = trait,
    verbatimTraitValue = value,
    verbatimTraitUnit = NA,
    taxonID = NA,
    measurementID = NA,
    occurrenceID = NA,
    warnings = NA,
    datasetID = NA,
    datasetName = NA,
    author = NA,
    bibliographicCitation = References,
    decimalLongitude = NA,
    decimalLatitude = NA,
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(database_id = paste0("PALMTRAITS_", 1:nrow(.)), .before = 1)
trait_palmtraits_ets

## export ----
arrow::write_parquet(trait_palmtraits_ets, "01_data/03_traits/02_prepared/trait_palmtraits_prep.parquet")

# melastomatraits -----------------------------------------------------------

## import ----
trait_melastomatraits_species <- data.table::fread("01_data/03_traits/01_raw/07_melastomatraits/records.txt") %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(ID = as.character(ID))
trait_melastomatraits_species

trait_melastomatraits_refs <- data.table::fread("01_data/03_traits/01_raw/07_melastomatraits/references.txt") %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(ID = as.character(ID))
trait_melastomatraits_refs

trait_melastomatraits_units <- readr::read_csv("01_data/03_traits/01_raw/07_melastomatraits/units.csv")
trait_melastomatraits_units

trait_palmtraits_species_pivot_join <- trait_melastomatraits_species %>% 
  dplyr::mutate(across(where(is.numeric), as.character)) %>% 
  tidyr::pivot_longer(cols = -c(ID:Accepted_Full),
                      names_to = "trait",
                      values_to = "value") %>% 
  dplyr::left_join(trait_melastomatraits_refs) %>% 
  dplyr::left_join(trait_melastomatraits_units)
trait_palmtraits_species_pivot_join

## prepare ---- 
trait_melastomatraits_ets <- trait_palmtraits_species_pivot_join %>%
  dplyr::mutate(
    traitID = NA,
    scientificName = Accepted_Name,
    traitName = as.character(trait),
    traitValue = value,
    traitUnit = unit,
    verbatimScientificName = Reported_Full,
    verbatimTraitName = trait,
    verbatimTraitValue = value,
    verbatimTraitUnit = unit,
    taxonID = NA,
    measurementID = NA,
    occurrenceID = NA,
    warnings = NA,
    datasetID = NA,
    datasetName = NA,
    author = NA,
    bibliographicCitation = Title,
    decimalLongitude = Longitude,
    decimalLatitude = Latitude,
    elevation = NA) %>%
  dplyr::select(
    traitID, scientificName, traitName, traitValue, traitUnit,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    taxonID, measurementID, occurrenceID, warnings,
    datasetID, datasetName, author, bibliographicCitation,
    decimalLongitude, decimalLatitude, elevation) %>% 
  dplyr::mutate(across(everything(), as.character)) %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>% 
  dplyr::mutate(database_id = paste0("MELOTOMATRAITS_", 1:nrow(.)), .before = 1)
trait_melastomatraits_ets

## export ----
arrow::write_parquet(trait_melastomatraits_ets, "01_data/03_traits/02_prepared/trait_melastomatraits_prep.parquet")

# end ---------------------------------------------------------------------
