#' ----
#' title: obj2 - traits - standardization trait
#' author: mauricio vancine
#' date: 2026-05-18
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(arrow)

# metadata
# Ecological Trait-data Standard (ETS): https://terminologies.gfbio.org/terms/ets/pages/index.html
# Plant Trait Ontology (TO): https://www.ebi.ac.uk/ols4/ontologies/to
# Plant Trait Ontology (PTO): https://obofoundry.org/ontology/to.html
# Thesaurus of Plant Characteristics (TOP): https://biodivportal.gfbio.org/ontologies/TOP
# A Terminological Resource for Plant Functional Diversity: https://top-thesaurus.cefe.cnrs.fr/home

# prepare thesaurus -----------------------------------------------------

## download ----
download.file(url = "https://data.biodivportal.gfbio.org/ontologies/TOP/download?apikey=580f5b2f-64d0-4b1a-9c1a-41562297a654&download_format=csv", 
              destfile = "01_data/03_traits/04_standardization_traits/thesauri/top-basic.csv", mode = "wb")

download.file(url = "https://data.bioontology.org/ontologies/PTO/download?apikey=8b5b7825-538d-40e0-9e9e-5ab9274a9aeb&download_format=csv", 
              destfile = "01_data/03_traits/04_standardization_traits/thesauri/to-basic.csv", mode = "wb")

## import ----
thesaurus_top <- readr::read_csv("01_data/03_traits/04_standardization_traits/thesauri/top-basic.csv")
thesaurus_top

thesaurus_to <- readr::read_csv("01_data/03_traits/04_standardization_traits/thesauri/to-basic.csv")
thesaurus_to

trait_try <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_try_prep.parquet") %>% 
  dplyr::select(traitName)
trait_try

trait_try_cat <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_try_cat_prep.parquet") %>% 
  dplyr::select(traitName)
trait_try_cat

trait_try_form <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_try_form_prep.parquet") %>% 
  dplyr::select(traitName)
trait_try_form

trait_bien <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_bien_prep.parquet") %>% 
  dplyr::select(traitName)
trait_bien

trait_gift <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_gift_prep.parquet") %>% 
  dplyr::select(traitName)
trait_gift

trait_neotroptree <- arrow::read_parquet("01_data/03_traits/02_prepared/trait_neotroptree_prep.parquet") %>% 
  dplyr::select(traitName)
trait_neotroptree

## traits ----
top_traits <- thesaurus_top %>% 
  dplyr::mutate(`Preferred Label` = stringr::str_to_lower(`Preferred Label`),
                `Synonyms` = stringr::str_to_lower(`Synonyms`)) %>% 
  dplyr::arrange(`Preferred Label`) %>% 
  dplyr::select(`Preferred Label`, `Synonyms`)
top_traits

to_traits <- thesaurus_to %>% 
  dplyr::mutate(`Preferred Label` = stringr::str_to_lower(`Preferred Label`),
                `Synonyms` = stringr::str_to_lower(`Synonyms`)) %>% 
  dplyr::arrange(`Preferred Label`) %>% 
  dplyr::select(`Preferred Label`, `Synonyms`)
to_traits

trait_try_traits <- trait_try %>%
  tidyr::drop_na(traitName) %>% 
  dplyr::distinct(traitName) %>% 
  dplyr::mutate(traitName = stringr::str_sort(stringr::str_to_lower(traitName))) %>% 
  dplyr::rename(verbatinTraitName = traitName)
trait_try_traits

trait_try_cat_traits <- trait_try_cat %>%
  tidyr::drop_na(traitName) %>% 
  dplyr::distinct(traitName) %>% 
  dplyr::mutate(traitName = stringr::str_sort(stringr::str_to_lower(traitName))) %>% 
  dplyr::rename(verbatinTraitName = traitName)
trait_try_cat_traits

trait_try_form_traits <- trait_try_form %>%
  tidyr::drop_na(traitName) %>% 
  dplyr::distinct(traitName) %>% 
  dplyr::mutate(traitName = stringr::str_sort(stringr::str_to_lower(traitName))) %>% 
  dplyr::rename(verbatinTraitName = traitName)
trait_try_form_traits

trait_bien_traits <- trait_bien %>%
  tidyr::drop_na(traitName) %>% 
  dplyr::distinct(traitName) %>% 
  dplyr::mutate(traitName = stringr::str_sort(stringr::str_to_lower(traitName))) %>% 
  dplyr::rename(verbatinTraitName = traitName)
trait_bien_traits

trait_gift_traits <- trait_gift %>%
  tidyr::drop_na(traitName) %>% 
  dplyr::distinct(traitName) %>% 
  dplyr::mutate(traitName = stringr::str_sort(stringr::str_to_lower(traitName))) %>% 
  dplyr::rename(verbatinTraitName = traitName)
trait_gift_traits

trait_neotroptree_traits <- trait_neotroptree %>%
  tidyr::drop_na(traitName) %>% 
  dplyr::distinct(traitName) %>% 
  dplyr::mutate(traitName = stringr::str_sort(stringr::str_to_lower(traitName))) %>% 
  dplyr::rename(verbatinTraitName = traitName)
trait_neotroptree_traits

## export ----
readr::write_csv(top_traits, "01_data/03_traits/04_standardization_traits/traits/top_traits.csv")
readr::write_csv(to_traits, "01_data/03_traits/04_standardization_traits/traits/to_traits.csv")
readr::write_csv(trait_try_traits, "01_data/03_traits/04_standardization_traits/traits/try_traits.csv")
readr::write_csv(trait_try_cat_traits, "01_data/03_traits/04_standardization_traits/traits/try_cat_traits.csv")
readr::write_csv(trait_try_form_traits, "01_data/03_traits/04_standardization_traits/traits/try_form_traits.csv")
readr::write_csv(trait_bien_traits, "01_data/03_traits/04_standardization_traits/traits/bien_traits.csv")
readr::write_csv(trait_gift_traits, "01_data/03_traits/04_standardization_traits/traits/gift_traits.csv")
readr::write_csv(trait_neotroptree_traits, "01_data/03_traits/04_standardization_traits/traits/neotroptree_traits.csv")

# traits standardization -------------------------------------------------

## import trait filtered ----
trait_try <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_try_filtered.parquet")
trait_try

trait_try_cat <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_try_cat_filtered.parquet")
trait_try_cat

trait_try_form <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_try_form_filtered.parquet")
trait_try_form

trait_bien <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_bien_filtered.parquet")
trait_bien

trait_gift <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_gift_filtered.parquet")
trait_gift

trait_neotroptree <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_neotroptree_filtered.parquet")
trait_neotroptree

trait_domingos <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_domingos_filtered.parquet")
trait_domingos

trait_seed_trait <- arrow::read_parquet("01_data/03_traits/03_filtered/trait_seed_trait_filtered.parquet")
trait_seed_trait

## trait names and units ----
trait_try %>% 
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::count(traitName, verbatimTraitUnit)

trait_try_form %>% 
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::count(traitName, verbatimTraitUnit)

trait_bien %>% 
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::count(traitName, verbatimTraitUnit)

trait_gift %>% 
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::count(traitName, verbatimTraitUnit)

trait_neotroptree %>% 
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::count(traitName, verbatimTraitUnit)

trait_seed_trait %>% 
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::count(traitName, verbatimTraitUnit)

## trait names and levels ----
trait_try %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

trait_try_cat %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

trait_try_form %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

trait_bien %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

trait_gift %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

trait_neotroptree %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

trait_domingos %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

trait_seed_trait %>% 
  dplyr::filter(valueType == "categorical") %>% 
  dplyr::count(traitName)

## fix units ----
trait_try_fixed_units <- trait_try %>%
  dplyr::filter(valueType == "numerical" & !is.na(verbatimTraitUnit)) %>%
  dplyr::mutate(
    traitValue = dplyr::case_when(
      traitName == "seed_mass" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "ssd" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "leaf_thickness" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "leaf_n" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "leaf_p" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "sla" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "ldmc" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "height" ~ as.character(as.numeric(verbatimTraitValue)),
      TRUE ~ traitName),
    traitUnit = dplyr::case_when(
      traitName == "seed_mass" ~ "mg",
      traitName == "ssd" ~ "g.cm-3",
      traitName == "leaf_thickness" ~ "mm",
      traitName == "leaf_n" ~ "mg.g-1",
      traitName == "leaf_p" ~ "mg.g-1",
      traitName == "sla" ~ "m2.kg-1",
      traitName == "ldmc" ~ "g.g-1",
      traitName == "height" ~ "m",
      TRUE ~ traitName),
    .after = traitName)
trait_try_fixed_units

trait_try_form_fixed_units <- trait_try_form %>%
  dplyr::filter(valueType == "numerical") %>%
  dplyr::mutate(
    traitValue = dplyr::case_when(
      traitName == "leaf_n" ~ as.character(1000/as.numeric(verbatimTraitValue)),
      traitName == "sla" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "height" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "seed_mass" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "ssd" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "ldmc" ~ as.character(as.numeric(verbatimTraitValue)),
      TRUE ~ traitName),
    traitUnit = dplyr::case_when(
      traitName == "leaf_n" ~ "mg.g-1",
      traitName == "sla" ~ "m2.kg-1",
      traitName == "height" ~ "m",
      traitName == "seed_mass" ~ "mg",
      traitName == "ssd" ~ "g.cm-3",
      traitName == "ldmc" ~ "g.g-1",
      TRUE ~ traitName),
    .after = traitName)
trait_try_form_fixed_units

trait_bien_fixed_units <- trait_bien %>%
  dplyr::filter(valueType == "numerical") %>%
  dplyr::mutate(
    traitValue = dplyr::case_when(
      traitName == "sla" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "ldmc" ~ as.character(as.numeric(verbatimTraitValue)/1000),
      traitName == "leaf_n" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "leaf_p" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "seed_mass" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "ssd" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "height" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "dbh" ~ as.character(as.numeric(verbatimTraitValue)),
      TRUE ~ traitName),
    traitUnit = dplyr::case_when(
      traitName == "sla" ~ "m2.kg-1",
      traitName == "ldmc" ~ "g.g-1",
      traitName == "leaf_n" ~ "mg.g-1",
      traitName == "leaf_p" ~ "mg.g-1",
      traitName == "seed_mass" ~ "mg",
      traitName == "ssd" ~ "g.cm-3",
      traitName == "height" ~ "m",
      traitName == "dbh" ~ "cm",
      TRUE ~ traitName),
    .after = traitName)
trait_bien_fixed_units

trait_gift_fixed_units <- trait_gift %>%
  dplyr::filter(valueType == "numerical") %>%
  dplyr::mutate(
    traitValue = dplyr::case_when(
      traitName == "height" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "seed_mass" ~ as.character(as.numeric(verbatimTraitValue) * 1000),
      traitName == "sla" ~ as.character(as.numeric(verbatimTraitValue) * 0.1),
      traitName == "ssd" ~ as.character(as.numeric(verbatimTraitValue)/1000),
      traitName == "leaf_thickness" ~ as.character(as.numeric(verbatimTraitValue)),
      traitName == "dbh" ~ as.character(as.numeric(verbatimTraitValue)),
      TRUE ~ traitName),
    traitUnit = dplyr::case_when(
      traitName == "height" ~ "m",
      traitName == "seed_mass" ~ "mg",
      traitName == "sla" ~ "m2.kg-1",
      traitName == "ssd" ~ "g.cm-3",
      traitName == "leaf_thickness" ~ "mm",
      traitName == "dbh" ~ "cm",
      TRUE ~ traitName),
    .after = traitName)
trait_gift_fixed_units

trait_neotroptree_fixed_units <- trait_neotroptree %>%
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::mutate(
    traitValue = as.character(as.numeric(verbatimTraitValue)/100),
    traitUnit = "m", 
    .after = traitName)
trait_neotroptree_fixed_units

trait_seed_trait_fixed_units <- trait_seed_trait %>%
  dplyr::filter(valueType == "numerical") %>% 
  dplyr::mutate(
    traitValue = as.character(as.numeric(verbatimTraitValue)),
    traitUnit = "mg", 
    .after = traitName)
trait_seed_trait_fixed_units

## fix levels ----

### growth_form ----
trait_try_cat %>% 
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_try_cat_fixed_levels_growth_form <- trait_try_cat %>%
  dplyr::filter(traitName == "growth_form" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "tree" ~ "tree",
    verbatimTraitValue == "shrub" ~ "shrub",
    verbatimTraitValue == "herb" ~ "herb",
    verbatimTraitValue == "graminoid" ~ "graminoid",
    verbatimTraitValue == "fern" ~ "fern",
    verbatimTraitValue == "moss" ~ "moss",
    verbatimTraitValue == "lichen" ~ "lichen",
    verbatimTraitValue == "shrub/tree" ~ "tree_shrub",
    verbatimTraitValue == "herb/shrub" ~ "shrub_herb",
    verbatimTraitValue == "herb/shrub/tree" ~ "tree_shrub_herb"),
    .after = traitName)
trait_try_cat_fixed_levels_growth_form %>% count(traitValue)

trait_try_form %>% 
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_try_form_fixed_levels_growth_form <- trait_try_form %>%
  dplyr::filter(traitName == "growth_form" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "tree" ~ "tree",
    verbatimTraitValue == "shrub" ~ "shrub",
    verbatimTraitValue == "herbaceous non-graminoid" ~ "herb",
    verbatimTraitValue == "herbaceous graminoid" ~ "graminoid",
    verbatimTraitValue == "bamboo graminoid" ~ "graminoid",
    verbatimTraitValue == "fern" ~ "fern",
    verbatimTraitValue == "climber" ~ "climber",
    verbatimTraitValue == "succulent" ~ "succulent",
    verbatimTraitValue == "shrub/tree" ~ "tree_shrub",
    verbatimTraitValue == "herbaceous non-graminoid/shrub" ~ "shrub_herb",
    verbatimTraitValue == "other" ~ "other",),
    .after = traitName)
trait_try_form_fixed_levels_growth_form %>% count(traitValue)

trait_bien_fixed_levels_growth_form <- trait_bien %>%
  dplyr::filter(traitName == "growth_form" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(
    traitValue = case_when(
      str_detect(verbatimTraitValue, regex("fern|lycopod|lycopsid", ignore_case = TRUE)) ~ "fern",
      str_detect(verbatimTraitValue, regex("moss|hepatic|liverwort|acrocarp|pleurocarp", ignore_case = TRUE)) ~ "moss",
      str_detect(verbatimTraitValue, regex("grass|graminoid|sedge|cyper|bamboo|reed", ignore_case = TRUE)) ~ "graminoid",
      str_detect(verbatimTraitValue, regex("climb|liana|vine|scandent|scrambler|twiner|creeper|trailing", ignore_case = TRUE)) ~ "climber",
      str_detect(verbatimTraitValue, regex("epiphyte|hanging|pendent", ignore_case = TRUE)) ~ "epiphyte",
      str_detect(verbatimTraitValue, regex("succulent|cactus|aloe", ignore_case = TRUE)) ~ "succulent",
      str_detect(verbatimTraitValue, regex("tree|palm|mallee|arborescent|canopy", ignore_case = TRUE)) ~ "tree",
      str_detect(verbatimTraitValue, regex("shrub|subshrub|suffrutex|bush", ignore_case = TRUE)) ~ "shrub",
      str_detect(verbatimTraitValue, regex("herb|forb|wildflower|therophyte", ignore_case = TRUE)) ~ "herb"),
    .after = traitName) %>% 
  dplyr::filter(!is.na(traitValue))
trait_bien_fixed_levels_growth_form %>% count(traitValue)

trait_gift %>% 
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_gift_fixed_levels_growth_form <- trait_gift %>%
  dplyr::filter(traitName == "growth_form" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "tree" ~ "tree",
    verbatimTraitValue == "shrub" ~ "shrub",
    verbatimTraitValue == "herb" ~ "herb",
    verbatimTraitValue == "graminoid" ~ "graminoid",
    verbatimTraitValue == "fern" ~ "fern",
    verbatimTraitValue == "moss" ~ "moss",
    verbatimTraitValue == "lichen" ~ "lichen",
    verbatimTraitValue == "shrub/tree" ~ "tree_shrub",
    verbatimTraitValue == "herb/shrub" ~ "shrub_herb",
    verbatimTraitValue == "herb/tree" ~ "tree_herb",
    verbatimTraitValue == "herb/shrub/tree" ~ "tree_shrub_herb",
    verbatimTraitValue == "tree/other" ~ "other",
    verbatimTraitValue == "other/shrub" ~ "other",
    verbatimTraitValue == "herb/other" ~ "other",
    verbatimTraitValue == "herb/other/shrub" ~ "other",
    verbatimTraitValue == "other/shrub/tree" ~ "other",
    verbatimTraitValue == "other/tree" ~ "other",
    verbatimTraitValue == "herb/other" ~ "other",
    verbatimTraitValue == "other" ~ "other"),
    .after = traitName)
trait_gift_fixed_levels_growth_form %>% count(traitValue)

trait_neotroptree %>% 
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_neotroptree_fixed_levels_growth_form <- trait_neotroptree %>%
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "Tree" ~ "tree",
    verbatimTraitValue == "Succulent tree" ~ "tree",
    verbatimTraitValue == "Lianescent tree" ~ "tree",
    verbatimTraitValue == "Lianescent treelet" ~ "tree",
    verbatimTraitValue == "Solitary palm" ~ "tree",
    verbatimTraitValue == "Cespitose palm" ~ "tree",
    verbatimTraitValue == "Hemiepiphytic tree" ~ "tree",
    verbatimTraitValue == "Succulent treeleet" ~ "tree",
    verbatimTraitValue == "Hemiepiphytic treelet" ~ "tree",
    verbatimTraitValue == "Succulent treelet" ~ "tree",
    verbatimTraitValue == "Arborescent fern" ~ "fern",
    verbatimTraitValue == "Lianescent tree or shrub" ~ "tree_shrub",
    verbatimTraitValue == "Palmoid tree or shrub" ~ "tree_shrub",
    verbatimTraitValue == "Shrub or treelet" ~ "tree_shrub",
    verbatimTraitValue == "Tree or shrub" ~ "tree_shrub",
    verbatimTraitValue == "Woody bamboo" ~ "graminoid"),
    .after = traitName)
trait_neotroptree_fixed_levels_growth_form %>% count(traitValue)

trait_domingos %>% 
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE) %>% 
  pull(verbatimTraitValue)

trait_domingos_fixed_levels_growth_form <- trait_domingos %>%
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "tree" ~ "tree",
    verbatimTraitValue == "herb" ~ "herb", 
    verbatimTraitValue == "shrub" ~ "shrub",
    verbatimTraitValue == "shrub, tree" ~ "tree_shrub",
    verbatimTraitValue == "liana" ~ "climber",
    verbatimTraitValue == "climber" ~ "climber",
    verbatimTraitValue == "vine" ~ "climber",
    verbatimTraitValue == "subshrub" ~ "shrub",
    verbatimTraitValue == "shrub, liana" ~ "shrub_climber",
    verbatimTraitValue == "shrub, small tree" ~ "tree_shrub",
    verbatimTraitValue == "palm tree" ~ "tree",
    verbatimTraitValue == "herb, subshrub" ~ "shrub_herb",
    verbatimTraitValue == "shrub, subshrub" ~ "shrub",
    verbatimTraitValue == "epiphyte" ~ "epiphyte",
    verbatimTraitValue == "shrub, tree, liana" ~ "tree_shrub_climber",
    verbatimTraitValue == "herb, shrub" ~ "shrub_herb",
    verbatimTraitValue == "small tree" ~ "tree",
    verbatimTraitValue == "herb, vine" ~ "herb_climber",
    verbatimTraitValue == "herb, shrub, subshrub" ~ "shrub_herb",
    verbatimTraitValue == "palm" ~ "tree",
    verbatimTraitValue == "tree, shrub" ~ "tree_shrub",
    verbatimTraitValue == "vine or liana" ~ "climber",
    verbatimTraitValue == "herb (bamboo)" ~ "graminoid",
    verbatimTraitValue == "tree, liana" ~ "tree_climber",
    verbatimTraitValue == "hemiepiphyte" ~ "epiphyte",
    verbatimTraitValue == "liana, shrub" ~ "shrub_climber",
    verbatimTraitValue == "climbing palm" ~ "climber",
    verbatimTraitValue == "liana, tree" ~ "tree_climber",
    verbatimTraitValue == "shrub, tree, subshrub" ~ "tree_shrub",
    verbatimTraitValue == "subshrub, liana" ~ "shrub_climber",
    verbatimTraitValue == "Shrub" ~ "shrub",
    verbatimTraitValue == "herb, subshrub, shrub" ~ "shrub_herb",
    verbatimTraitValue == "scandent shrub" ~ "shrub",
    verbatimTraitValue == "climber, shrub" ~ "shrub_climber",
    verbatimTraitValue == "liana/herbaceous vines" ~ "climber",
    verbatimTraitValue == "Subshrub" ~ "shrub",
    verbatimTraitValue == "liana, shrub, tree" ~ "tree_shrub_climber",
    verbatimTraitValue == "shrub, climber" ~ "shrub_climber",
    verbatimTraitValue == "shrub, liana, tree" ~ "tree_shrub_climber",
    verbatimTraitValue == "shrub, subshrub, liana" ~ "shrub_climber",
    verbatimTraitValue == "shrub, treelet" ~ "tree_shrub",
    verbatimTraitValue == "small shrub" ~ "shrub",
    verbatimTraitValue == "subshrub, shrub" ~ "shrub",
    verbatimTraitValue == "subshrub, vine" ~ "shrub_climber",
    verbatimTraitValue == "Herb" ~ "herb",
    verbatimTraitValue == "herb, shrub, climber" ~ "shrub_herb_climber",
    verbatimTraitValue == "shrub, scandent shrub" ~ "herb",
    verbatimTraitValue == "shrub, vine" ~ "shrub_climber",
    verbatimTraitValue == "vine/liana" ~ "climber",
    verbatimTraitValue == "Liana or woody epiphyte" ~ "epiphyte_climber",
    verbatimTraitValue == "herb, climber" ~ "herb_climber",
    verbatimTraitValue == "herb, shrub, tree" ~ "tree_shrub_herb",
    verbatimTraitValue == "herb, subshrub, liana" ~ "shrub_herb_climber",
    verbatimTraitValue == "herb, vine, epiphyte" ~ "shrub_herb_climber",
    verbatimTraitValue == "liana, rarely trees" ~ "tree_climber",
    verbatimTraitValue == "liana, scandent shrub" ~ "shrub_climber",
    verbatimTraitValue == "liana/herbaceous vine" ~ "climber",
    verbatimTraitValue == "shrub or treelet" ~ "tree_shrub",
    verbatimTraitValue == "shrub, small tree, liana" ~ "tree_shrub_climber",
    verbatimTraitValue == "shurb" ~ "shrub",
    verbatimTraitValue == "subshrub, shrub, small tree" ~ "shrub_tree",
    verbatimTraitValue == "subshrub, shrub, tree" ~ "shrub_tree",
    verbatimTraitValue == "tree, vine" ~ "tree_climber",
    verbatimTraitValue == "vine, shrub" ~ "shrub_climber"),
    .after = traitName)
trait_domingos_fixed_levels_growth_form %>% count(traitValue)

trait_seed_trait %>% 
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_seed_trait_fixed_levels_growth_form <- trait_seed_trait %>%
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "tree" ~ "tree",
    verbatimTraitValue == "shrub" ~ "shrub",
    verbatimTraitValue == "herb" ~ "herb",
    verbatimTraitValue == "liana" ~ "climber",
    verbatimTraitValue == "vine" ~ "climber",
    verbatimTraitValue == "root climber" ~ "climber",
    verbatimTraitValue == "climber" ~ "climber",
    verbatimTraitValue == "epiphyts" ~ "epiphyte",
    verbatimTraitValue == "hemiepiphytes" ~ "epiphyte",
    verbatimTraitValue == "shrub/tree" ~ "tree_shrub",
    verbatimTraitValue == "rosette" ~ "herb",
    verbatimTraitValue == "dracaenoid" ~ "shrub"),
    .after = traitName)
trait_seed_trait_fixed_levels_growth_form %>% count(traitValue)

### woodiness ----
trait_try_cat %>% 
  dplyr::filter(traitName == "woodiness") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_try_cat_fixed_levels_woodiness <- trait_try_cat %>%
  dplyr::filter(traitName == "woodiness" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "woody" ~ "woody",
    verbatimTraitValue == "non-woody" ~ "non_woody",
    verbatimTraitValue == "non-woody/woody" ~ "non_woody_woody"),
    .after = traitName)
trait_try_cat_fixed_levels_woodiness %>% count(traitValue)

trait_try_form %>% 
  dplyr::filter(traitName == "woodiness") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_try_form_fixed_levels_woodiness <- trait_try_form %>%
  dplyr::filter(traitName == "woodiness" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "woody" ~ "woody",
    verbatimTraitValue == "Woody" ~ "woody",
    verbatimTraitValue == "non-woody" ~ "non_woody",
    verbatimTraitValue == "semi-woody" ~ "semi_woody"),
    .after = traitName)
trait_try_form_fixed_levels_woodiness %>% count(traitValue)

### dispersal ----
trait_bien %>% 
  dplyr::filter(traitName == "dispersal") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_bien_fixed_levels_dispersal <- trait_bien %>%
  dplyr::filter(traitName == "dispersal" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = case_when(
    verbatimTraitValue == "biotic" ~ "biotic",
    verbatimTraitValue == "abiotic" ~ "abiotic"),
    .after = traitName)
trait_bien_fixed_levels_dispersal %>% count(traitValue)

trait_gift %>% 
  dplyr::filter(traitName == "dispersal") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_gift_fixed_levels_dispersal <- trait_gift %>%
  dplyr::filter(traitName == "dispersal" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = stringr::str_replace_all(verbatimTraitValue, "[/]", "_"), .after = traitName)
trait_gift_fixed_levels_dispersal %>% count(traitValue)

trait_seed_trait %>% 
  dplyr::filter(traitName == "dispersal") %>% 
  dplyr::count(verbatimTraitValue, sort = TRUE)

trait_seed_trait_fixed_levels_dispersal <- trait_seed_trait %>%
  dplyr::filter(traitName == "dispersal" & !is.na(verbatimTraitValue)) %>% 
  dplyr::mutate(traitValue = stringr::str_replace_all(verbatimTraitValue, "[-]", "_"), .after = traitName)
trait_seed_trait_fixed_levels_dispersal %>% count(traitValue)

## bind traits ----
traits <- dplyr::bind_rows(
  trait_try_fixed_units,
  trait_try_form_fixed_units,
  trait_bien_fixed_units,
  trait_gift_fixed_units,
  trait_neotroptree_fixed_units,
  trait_seed_trait_fixed_units,
  
  trait_try_cat_fixed_levels_growth_form,
  trait_try_form_fixed_levels_growth_form,
  trait_bien_fixed_levels_growth_form,
  trait_gift_fixed_levels_growth_form,
  trait_neotroptree_fixed_levels_growth_form,
  trait_domingos_fixed_levels_growth_form,
  trait_seed_trait_fixed_levels_growth_form,
  
  trait_try_cat_fixed_levels_woodiness,
  trait_try_form_fixed_levels_woodiness,
  
  trait_bien_fixed_levels_dispersal,
  trait_gift_fixed_levels_dispersal,
  trait_seed_trait_fixed_levels_dispersal)
traits

traits_unique <- traits %>% 
  dplyr::distinct(verbatimScientificName, traitName, traitValue, .keep_all = TRUE)
traits_unique

# join top and to ---------------------------------------------------------

## traits ----
traits_unique_trait <- traits_unique %>% 
  count(traitName, sort = TRUE)
traits_unique_trait

## import trait top and to ----
thesaurus_top <- readr::read_csv("01_data/03_traits/04_standardization_traits/thesauri/top-basic.csv")
thesaurus_top

thesaurus_to <- readr::read_csv("01_data/03_traits/04_standardization_traits/thesauri/to-basic.csv")
thesaurus_to

thesaurus_top_join <- thesaurus_top %>% 
  dplyr::select(`Class ID`, Definitions) %>% 
  dplyr::rename(traitID = 1, traitDescription = 2) %>% 
  dplyr::mutate(traitDescription = stringr::str_to_sentence(traitDescription))
thesaurus_top_join

thesaurus_to_join <- thesaurus_to %>% 
  dplyr::select(`Class ID`, Definitions) %>% 
  dplyr::rename(traitID = 1, traitDescription = 2) %>% 
  dplyr::mutate(traitDescription = stringr::str_to_sentence(traitDescription))
thesaurus_to_join

thesaurus_top_std <- readr::read_csv("01_data/03_traits/04_standardization_traits/thesauri/top-basic.csv") %>% 
  dplyr::select(1:2) %>% 
  dplyr::rename(traitID = 1, traitName_top = 2) %>% 
  dplyr::mutate(traitName_top = stringr::str_to_lower(traitName_top)) %>% 
  dplyr::left_join(thesaurus_top_join) %>% 
  dplyr::filter(traitName_top %in% c(
    "plant growth form", 
    "plant dispersal syndrome",
    "woodiness", 
    "plant height trait", 
    "leaf dry matter content", 
    "seed mass", 
    "plant leaf area", 
    "leaf nitrogen content per leaf dry mass",
    "leaf thickness", 
    "stem specific density", 
    "leaf phosphorus content per leaf dry mass")) %>% 
  dplyr::mutate(traitName = c(
    "dispersal",
    "growth_form",
    "ldmc",
    "leaf_p",
    "woodiness",
    "ssd",
    "sla",
    "height",
    "seed_mass",
    "leaf_n",
    "leaf_thickness")) %>% 
  dplyr::select(-traitName_top)
thesaurus_top_std

thesaurus_to_std <- readr::read_csv("01_data/03_traits/04_standardization_traits/thesauri/to-basic.csv") %>% 
  dplyr::select(1:2) %>% 
  dplyr::rename(traitID = 1, traitName_to = 2) %>% 
  dplyr::mutate(traitName_to = stringr::str_to_lower(traitName_to)) %>% 
  dplyr::left_join(thesaurus_to_join) %>% 
  dplyr::filter(traitName_to == "stem diameter") %>% 
  dplyr::mutate(traitName = "dbh") %>% 
  dplyr::select(-traitName_to)
thesaurus_to_std

thesaurus_std <- rbind(thesaurus_top_std, thesaurus_to_std)
thesaurus_std

## categorical levels ---- 
trait_cat_levels_growth_form <- traits_unique %>% 
  dplyr::filter(traitName == "growth_form") %>% 
  dplyr::count(traitValue) %>% 
  dplyr::pull(1) %>% 
  paste0(collapse = ",")
trait_cat_levels_growth_form

trait_cat_levels_woodiness <- traits_unique %>% 
  dplyr::filter(traitName == "woodiness") %>% 
  dplyr::count(traitValue) %>% 
  dplyr::pull(1) %>% 
  paste0(collapse = ",")
trait_cat_levels_woodiness

trait_cat_levels_dispersal <- traits_unique %>% 
  dplyr::filter(traitName == "dispersal") %>% 
  dplyr::count(traitValue) %>% 
  dplyr::pull(1) %>% 
  paste0(collapse = ",")
trait_cat_levels_dispersal

## standardize traits ----
traits_standardize <- traits_unique %>% 
  dplyr::mutate(factorLevels = case_when(
    traitName == "growth_form" ~ trait_cat_levels_growth_form,
    traitName == "woodiness" ~ trait_cat_levels_woodiness,
    traitName == "dispersal" ~ trait_cat_levels_dispersal,
    TRUE ~ NA)) %>% 
  dplyr::left_join(thesaurus_std) %>% 
  dplyr::select(
    database_id, 
    traitID, traitName, traitValue, traitUnit, valueType, factorLevels, traitDescription,
    verbatimScientificName, verbatimTraitName, verbatimTraitValue, verbatimTraitUnit,
    stdScientificName, measurementID, datasetID, datasetName, author, 
    bibliographicCitation, warnings)         
traits_standardize

traits_standardize_description <- traits_standardize %>% 
  dplyr::count(valueType, traitID, traitName, traitUnit, factorLevels, traitDescription) %>% 
  dplyr::select(-n)
traits_standardize_description

## export ----
readr::write_csv(traits_standardize_description, "01_data/03_traits/04_standardization_traits/traits_standardize_description.csv")
arrow::write_parquet(traits_standardize, "01_data/03_traits/04_standardization_traits/traits_standardize.parquet")

# end ---------------------------------------------------------------------
