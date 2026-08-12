#' ----
#' title: sdm - rare - modeling
#' author: mauricio vancine
#' date: 30/06/2026
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(flexsdm) # pak::pak("sjevelazco/flexsdm")

# options
options(scipen = 1000)
set.seed(42)

# import data -------------------------------------------------------------

## occurrences ----
occ <- readr::read_csv("01_data/01_occurrences/01_occurrences/01_filtered/occ_tanguro_oppc_adjusted_rare.csv") %>% 
    dplyr::select(scientificName, decimalLongitude, decimalLatitude) %>% 
    dplyr::rename(species = scientificName, x = decimalLongitude, y = decimalLatitude)
occ

# modeling ----------------------------------------------------------------

## pc config ----
parallel::detectCores(logical = FALSE)
parallel::detectCores(logical = TRUE)
sapply(ps::ps_system_memory(), function(x) round(x/1024^3, 2))

n_cores <- parallel::detectCores(logical = TRUE) - 2
n_cores

## parameters ----
methods <- c("glm", "gam", "gbm", "net", "svm", "max") 
sorensen_good_models <- .5

## modeling ----
for(i in sort(unique(occ$species))[2:7]){
    
    # information
    print(i)
    sp_name <- paste0(sub(" ", "_", tolower(i)))
    
    # directories
    sp_dir_data <- paste0("02_output/01_sdm_rare/", sp_name, "/01_pre_modeling/")
    sp_dir_model <- paste0("02_output/01_sdm_rare/", sp_name, "/02_modeling/")
    
    dir.create(path = sp_dir_model)
    
    # import data
    psa_data <- readr::read_csv(paste0(sp_dir_data, "02_03_01_var_data_", sp_name,"_occ_psa.csv"))
    bg_data <- readr::read_csv(paste0(sp_dir_data, "02_03_02_var_data_", sp_name,"_bg.csv"))
    
    var_names <- names(psa_data)
    var_names <- var_names[!grepl("x|y|pr_ab|\\.part", var_names)]
    
    # models fitted
    fitted_models <- as.list(methods)
    names(fitted_models) <- paste0("esm_", methods)
    
    #### glm ----
    if("glm" %in% methods){
        fitted_models$esm_glm <- tryCatch({
            flexsdm::esm_glm(
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                thr = "max_sorensen")
        }, error = function(e){
            warning("ESM GLM failed: ", e$message)
            NULL
        })
    }
    
    #### gam ----
    if("gam" %in% methods){
        fitted_models$esm_gam <- tryCatch({
            flexsdm::esm_gam(
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                thr = "max_sorensen")
        }, error = function(e){
            warning("ESM GAM failed: ", e$message)
            NULL
        })
    }
    
    #### gau ----
    if("gau" %in% methods){
        fitted_models$esm_gau <- tryCatch({
            flexsdm::esm_gau(
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                background = bg_data,
                thr = "max_sorensen")
        }, error = function(e){
            warning("ESM GAU failed: ", e$message)
            NULL
        })
    }
    
    #### gbm ----
    if("gbm" %in% methods){
        fitted_models$esm_gbm <- tryCatch({
            flexsdm::esm_gbm(            
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                thr = "max_sorensen")
        }, error = function(e){
            warning("ESM GBM failed: ", e$message)
            NULL
        })
    }
    
    #### net -----
    if("net" %in% methods){
        fitted_models$esm_net <- tryCatch({
            flexsdm::esm_net(           
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                thr = "max_sorensen")
        }, error = function(e){
            warning("ESM NET failed: ", e$message)
            NULL
        })
    }
    
    #### svm -----
    if("svm" %in% methods){
        fitted_models$esm_svm <- tryCatch({
            flexsdm::esm_svm(           
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                thr = "max_sorensen")
        }, error = function(e){
            warning("ESM SVM failed: ", e$message)
            NULL
        })
    }
    
    #### max -----
    if("max" %in% methods){
        fitted_models$esm_max <- tryCatch({
            flexsdm::esm_max(            
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                background = bg_data,
                thr = "max_sorensen")
        }, error = function(e){
            warning("ESM MAXENT failed: ", e$message)
            NULL
        })
    }
    
    # filter
    fitted_models <- fitted_models[!sapply(fitted_models, is.null)]
    
    # export models
    readr::write_rds(fitted_models, paste0(sp_dir_model, "03_01_mod_fitted_models", sp_name, ".rds"))
    
    # variable importance ----
    var_imp <- list()
    for(m in 1:length(fitted_models)){
        
        mod_name <- names(fitted_models)[m]
        model_obj <- fitted_models[[m]]
        
        # variable importance
        var_imp[[m]] <- flexsdm::sdm_varimp(
            model = model_obj,
            data = psa_data,
            response = "pr_ab",
            predictors = var_names,
            n_sim = 30, 
            n_cores = min(n_cores, 4),
            thr = "max_sorensen",
            clamp = TRUE,
            pred_type = "cloglog")
    }
    
    var_imp <- dplyr::bind_rows(var_imp) %>% 
        dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))
    readr::write_csv(var_imp, paste0(sp_dir_model, "03_03_mod_importance_", sp_name, "_data.csv"))
    
    # plot
    var_imp_models <- var_imp %>%
        tidyr::pivot_longer(
            cols = TPR:IMAE,
            names_to = "metric",
            values_to = "value") %>%
        dplyr::filter(metric %in% c("AUC", "TSS", "SORENSEN")) %>% 
        dplyr::mutate(model = paste(
            stringr::str_split(model, pattern = "_", simplify = TRUE)[, 1],
            stringr::str_split(model, pattern = "_", simplify = TRUE)[, 2], sep = "_")) %>% 
        dplyr::group_by(model, threshold, predictors, metric) %>% 
        dplyr::summarise(value = mean(value))
    
    p_vi <- ggplot(var_imp_models, aes(x = predictors, y = value, fill = model)) +
        scale_fill_brewer(palette = "Set1") +
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_grid(model ~ metric, scales = "free") +
        coord_flip() +
        labs(x = "Predictor variables", y = "Variable importance") +
        theme_bw()
    ggsave(filename = paste0(sp_dir_model, "03_03_mod_importance_", sp_name, ".png"),
           plot = p_vi, wi = 12, he = 14, dpi = 300, unit = "cm", scale = 1.5)
    
    # model performance ----
    fitted_models_performance <- flexsdm::sdm_summarize(fitted_models) %>% 
        dplyr::mutate(d_somers = 2 * (AUC_mean - .5)) %>% 
        dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))
    readr::write_csv(fitted_models_performance, paste0(sp_dir_model, "03_04_mod_performance_", sp_name, ".csv"))
    
    # model good fit ----
    good_models_performance <- fitted_models_performance$model[
        fitted_models_performance$SORENSEN_mean >= sorensen_good_models &
            fitted_models_performance$thr_value > 0 &
            fitted_models_performance$thr_value < 1]
    
    good_models <- fitted_models[good_models_performance]
    
    # export good models ----
    readr::write_rds(good_models, paste0(sp_dir_model, "03_05_mod_good_models_", sp_name, ".rds"))
    
    # clean memory ----
    gc(verbose = FALSE, reset = TRUE)
    
}

# end ---------------------------------------------------------------------
