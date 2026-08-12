check_require_cran <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package `", pkg, "` needed for this function to work! Please, install it with: `install.packages(\"", pkg, "\")`"), call. = FALSE)
  } else {
    require(pkg, character.only = TRUE)
  }
}

bdc_create_report <- function (data, database_id = "database_id", workflow_step, 
                               save_report = FALSE) 
{
  Description <- . <- NULL
  . <- V1 <- Test_name <- Description <- notes <- NULL
  number_of_records <- .uncer_terms <- `NA` <- NULL
  suppressWarnings({
    check_require_cran("readr")
    check_require_cran("DT")
  })
  match.arg(arg = workflow_step, choices = c("prefilter", 
                                             "taxonomy", "space", "time"))
  temp <- data %>% dplyr::select(tidyselect::starts_with("."))
  if (workflow_step %in% c("prefilter", "space", "time")) {
    if (all((colSums(temp, na.rm = TRUE) - nrow(temp)) == 
            0)) {
      stop("Report was not created.\nNo records flagged as 'FALSE' in test columns starting with '.'")
    }
    if (ncol(temp) == 0) {
      stop("Report was not created.\nAt least one column 'starting with '.' containing results of data-quality tests must be provided")
    }
  }
  if (workflow_step == "prefilter") {
    tests <- c(".scientificName_empty", ".coordinates_empty", 
               ".coordinates_outOfRange", ".basisOfRecords_notStandard", 
               ".coordinates_country_inconsistent")
    names_tab <- names(data)
    col_to_tests <- dplyr::intersect(tests, names_tab)
    has_file <- file.exists("Output/Check/01_coordinates_transposed.csv")
    has_column <- "coordinates_transposed" %in% names(data)
    if (has_file & has_column) {
      col_to_tests <- c(col_to_tests, "coordinates_transposed")
    }
  }
  if (workflow_step == "space") {
    tests <- c(".equ", ".zer", ".cap", ".cen", ".otl", ".gbf", 
               ".inst", ".dpl", ".rou", ".sea", ".urb")
    names_tab <- names(data)
    col_to_tests <- dplyr::intersect(tests, names_tab)
  }
  if (workflow_step == "time") {
    tests <- c(".eventDate_empty", ".year_outOfRange")
    names_tab <- names(data)
    col_to_tests <- dplyr::intersect(tests, names_tab)
  }
  suppressMessages({
    suppressWarnings({
      n_records <- data %>% dplyr::summarise(n = dplyr::n()) %>% 
        dplyr::pull(n)
      n_record_database <- data %>% dplyr::mutate(database_id = gsub("[[:digit:]]+", 
                                                                     "", database_id), database_id = gsub("_", "", 
                                                                                                          database_id)) %>% dplyr::group_by(database_id) %>% 
        dplyr::summarise(n_total = dplyr::n())
      format_df <- function(x) {
        x <- x %>% dplyr::add_row() %>% as.data.frame()
        x <- x %>% dplyr::select(Description, dplyr::everything())
        x[1 + nrow(x), 1] <- paste("(*) calculated in relation to total number of records, i.e.", 
                                   n_records, "records")
        names(x)[4] <- "perc_number_records(*)"
        x <- x %>% replace(is.na(.), "")
        data <- x %>% DT::datatable(., class = "stripe", 
                                    options = list(pageLength = 10, dom = "Brtip", 
                                                   scrollX = TRUE))
        return(list(x, data))
      }
      if ("prefilter" %in% workflow_step) {
        pf <- data %>% dplyr::select(all_of(col_to_tests)) %>% 
          bdc::bdc_summary_col() %>% dplyr::mutate_if(is.character, 
                                                      ~as.logical(as.character(.))) %>% dplyr::summarise_all(., 
                                                                                                             .funs = sum) %>% t() %>% tibble::as_tibble(rownames = "NA") %>% 
          dplyr::mutate(V1 = nrow(data) - V1) %>% dplyr::mutate(Perc_records_flagged = round((V1/nrow(data) * 
                                                                                                100), 2)) %>% dplyr::rename(Test_name = `NA`, 
                                                                                                                            Records_flagged = V1)
        pf <- pf %>% dplyr::mutate(Description = Test_name) %>% 
          dplyr::mutate(Description = if_else(Description == 
                                                ".scientificName_empty", "Records with empty scientific name", 
                                              Description), Description = dplyr::if_else(Description == 
                                                                                           ".coordinates_empty", "Records with empty coordinates", 
                                                                                         Description), Description = dplyr::if_else(Description == 
                                                                                                                                      ".coordinates_outOfRange", "Records with coordinates out-of-range", 
                                                                                                                                    Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                 ".basisOfRecords_notStandard", "Records from a doubtful source", 
                                                                                                                                                                               Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                            ".coordinates_country_inconsistent", "Records outside one or multiple reference countries", 
                                                                                                                                                                                                                          Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                       ".summary", "Summary of all tests", Description), 
                        Description = dplyr::if_else(Description == 
                                                       "coordinates_transposed", "Records with transposed geographic coordinates", 
                                                     Description))
        res <- format_df(pf)
        data <- res[[2]]
        if (save_report) {
          bdc_create_dir()
          readr::write_csv(res[[1]], here::here("Output/Report/01_Report_Prefilter.csv"))
        }
      }
      if ("taxonomy" %in% workflow_step) {
        names <- data %>% dplyr::group_by(notes) %>% 
          dplyr::summarise(number_of_records = dplyr::n()) %>% 
          dplyr::mutate(perc_number_records = round((number_of_records/n_records) * 
                                                      100, 2))
        names$notes <- stringr::str_squish(names$notes)
        if (".uncer_terms" %in% names(data)) {
          taxo_unc <- data %>% dplyr::select(.uncer_terms) %>% 
            dplyr::filter(.uncer_terms == FALSE) %>% 
            dplyr::group_by(.uncer_terms) %>% dplyr::summarise(number_of_records = dplyr::n()) %>% 
            dplyr::mutate(perc_number_records = round((number_of_records/n_records) * 
                                                        100, 2)) %>% dplyr::rename(notes = .uncer_terms) %>% 
            dplyr::mutate(notes = ifelse(notes == "FALSE", 
                                         "taxo_uncer", notes))
        }
        if (nrow(taxo_unc > 0)) {
          names <- dplyr::bind_rows(names, taxo_unc)
        }
        names$notes <- stringr::str_squish(names$notes)
        names <- names %>% dplyr::mutate(notes = dplyr::if_else(notes == 
                                                                  "", "valid names", notes)) %>% dplyr::mutate(Description = notes) %>% 
          dplyr::mutate(Description = dplyr::if_else(Description == 
                                                       "taxo_uncer", "check: doubtful taxonomic identification", 
                                                     Description), Description = dplyr::if_else(is.na(Description), 
                                                                                                "invalid: no interpretable name", Description), 
                        Description = dplyr::if_else(Description == 
                                                       "accepted", "valid: name accepted", Description), 
                        Description = dplyr::if_else(Description == 
                                                       "accepted | replaceSynonym", "valid: synonym replaced by an accepted name", 
                                                     Description), Description = dplyr::if_else(Description == 
                                                                                                  "accepted | wasMisspelled", "valid: accepted name that was misspelled", 
                                                                                                Description), Description = dplyr::if_else(Description == 
                                                                                                                                             "accepted | wasMisspelled | replaceSynonym", 
                                                                                                                                           "valid: accepted name assigned as misspelled synonym", 
                                                                                                                                           Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                        "heterotypic synonym", "check: ambiguous synonym linked to multiple accepted names", 
                                                                                                                                                                                      Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                   "homotypic synonym", "check: ambiguous synonym linked to multiple accepted names", 
                                                                                                                                                                                                                                 Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                              "proparte synonym", "check: ambiguous synonym linked to multiple accepted names", 
                                                                                                                                                                                                                                                                            Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                                                                         "homotypic synonym | wasMisspelled", "check: ambiguous synonym that was misspelled", 
                                                                                                                                                                                                                                                                                                                       Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                                                                                                                    "heterotypic synonym | wasMisspelled", 
                                                                                                                                                                                                                                                                                                                                                                  "check: ambiguous synonym that was misspelled", 
                                                                                                                                                                                                                                                                                                                                                                  Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                                                                                                                                                               "proparte synonym | wasMisspelled", "check: ambiguous synonym that was misspelled", 
                                                                                                                                                                                                                                                                                                                                                                                                             Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "notFound", "invalid: name not found in the taxonomic database or a non\n                -interpretable name", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "multipleAccepted", "invalid: name linked to multiple accepted names", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "synonym | noAcceptedName", "check: synonym with no accepted names", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Description), Description = dplyr::if_else(Description == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "accepted | replacedSynonym", "check: synonym with no accepted names", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Description))
        res <- format_df(names)
        data <- res[[2]]
        if (save_report) {
          bdc_create_dir()
          readr::write_csv(res[[1]], here::here("Output/Report/02_Report_taxonomy.csv"))
        }
      }
      if ("space" %in% workflow_step) {
        space <- data %>% dplyr::select(col_to_tests) %>% 
          dplyr::mutate_if(is.character, ~as.logical(as.character(.))) %>% 
          dplyr::summarise_all(., .funs = sum) %>% t() %>% 
          tibble::as_tibble(rownames = "NA") %>% dplyr::mutate(V1 = nrow(data) - 
                                                                 V1) %>% dplyr::mutate(Perc_records_flagged = round((V1/n_records * 
                                                                                                                       100), 2)) %>% dplyr::rename(Test_name = `NA`, 
                                                                                                                                                   Records_flagged = V1)
        space <- space %>% dplyr::mutate(Description = Test_name) %>% 
          dplyr::mutate(Description = dplyr::if_else(
            Description == ".val", "Coordinates valid", Description), 
            Description = dplyr::if_else(Description == ".equ", "Identical coordinates", Description), 
            Description = dplyr::if_else(Description == ".zer", "Plain zeros", Description), 
            Description = dplyr::if_else(Description == ".cap", "Records around country capital centroid", Description), 
            Description = dplyr::if_else(Description == ".cen", "Records around country or province centroids", Description), 
            Description = dplyr::if_else(Description == ".otl", "Geographical outliers", Description), 
            Description = dplyr::if_else(Description == ".gbf", "Records around the GBIF headquarters", Description), 
            Description = dplyr::if_else(Description == ".inst", "Records around biodiversity institutions", Description), 
            Description = dplyr::if_else(Description == ".dpl", "Duplicated coordinates per species", Description),
            Description = dplyr::if_else(Description == ".rou", "Rounded (probably imprecise) coordinates", Description), 
            Description = dplyr::if_else(Description == ".sea", "Records within ocean", Description), 
            Description = dplyr::if_else(Description == ".urb", "Records within urban areas", Description), 
            Description = dplyr::if_else(Description == ".summary", "Summary of all tests", Description))
        res <- format_df(space)
        data <- res[[2]]
        if (save_report) {
          bdc_create_dir()
          readr::write_csv(res[[1]], here::here("Output/Report/03_Report_space.csv"))
        }
      }
      if ("time" %in% workflow_step) {
        date <- data %>% dplyr::select(all_of(col_to_tests)) %>% 
          bdc::bdc_summary_col() %>% dplyr::mutate_if(is.character, 
                                                      ~as.logical(as.character(.))) %>% dplyr::summarise_all(., 
                                                                                                             .funs = sum) %>% t() %>% tibble::as_tibble(rownames = "NA") %>% 
          dplyr::mutate(V1 = nrow(data) - V1) %>% dplyr::mutate(Perc_records_flagged = round((V1/n_records * 
                                                                                                100), 2)) %>% dplyr::rename(Test_name = `NA`, 
                                                                                                                            Records_flagged = V1)
        date <- date %>% dplyr::mutate(Description = Test_name) %>% 
          dplyr::mutate(Description = dplyr::if_else(Description == 
                                                       ".eventDate_empty", "Records with empty eventDate", 
                                                     Description), Description = dplyr::if_else(Description == 
                                                                                                  ".year_outOfRange", "Records with year out-of-range (i.e., year in the future or\n                collected before a threshold defined by the user).", 
                                                                                                Description), Description = dplyr::if_else(Description == 
                                                                                                                                             ".summary", "Summary of all tests", Description))
        res <- format_df(date)
        data <- res[[2]]
        if (save_report) {
          bdc_create_dir()
          readr::write_csv(res[[1]], here::here("Output/Report/04_Report_time.csv"))
        }
      }
    })
  })
  if (save_report) {
    message(paste("\nbdc_create_report:\nCheck the report summarizing the results of the", 
                  workflow_step, "in:\n", here::here("Output", "Report")))
  }
  return(data)
}
