#' ----
#' title: sdm - common - modeling
#' author: mauricio vancine
#' date: 05/07/2026
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(flexsdm) # pak::pak("sjevelazco/flexsdm")

# options
options(scipen = 1000)
set.seed(42)

# import data -------------------------------------------------------------

# occurrences ----
occ <- readr::read_csv("01_data/01_occurrences/01_occurrences/01_filtered/occ_tanguro_oppc_adjusted_common.csv") %>% 
    dplyr::select(scientificName, decimalLongitude, decimalLatitude) %>% 
    dplyr::rename(species = scientificName, x = decimalLongitude, y = decimalLatitude)
occ

## species list
sp_list <- sort(unique(occ$species))
sp_list

# modeling ----------------------------------------------------------------

# pc config ----
parallel::detectCores(logical = FALSE)
parallel::detectCores(logical = TRUE)
sapply(ps::ps_system_memory(), function(x) round(x/1024^3, 2))
n_cores <- parallel::detectCores(logical = TRUE) - 4

# parameters ----
methods <- c("glm", "gam", "gbm", "net", "raf", "svm", "max")
sorensen_good_models <- .5

# modeling ----
for(i in sp_list){
    
    # information ----
    print(i)
    sp_name <- paste0(sub(" ", "_", tolower(i)))
    
    # directories ----
    sp_dir_data <- paste0("02_output/01_sdm_common/", sp_name, "/01_pre_modeling/")
    sp_dir_model <- paste0("02_output/01_sdm_common/", sp_name, "/02_modeling/")
    
    dir.create(path = sp_dir_model)
    
    # import data ----
    psa_data <- readr::read_csv(paste0(sp_dir_data, "02_03_01_var_data_", sp_name,"_occ_psa.csv"))
    bg_data <- readr::read_csv(paste0(sp_dir_data, "02_03_02_var_data_", sp_name,"_bg.csv"))
    
    var_names <- names(psa_data)
    var_names <- var_names[!grepl("x|y|pr_ab|\\.part|vrm", var_names)]
    
    # fit models  ----
    fitted_models <- as.list(methods)
    names(fitted_models) <- methods
    
    ## glm ----
    if("glm" %in% methods){
        fitted_models$glm <- tryCatch({
            flexsdm::fit_glm(
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                thr = "max_sorensen",
                poly = 2,
                inter_order = 0)
        }, error = function(e){
            warning("GLM failed: ", e$message)
            NULL
        })
    }
    
    ## gam ----
    if("gam" %in% methods){
        fitted_models$gam <- tryCatch({
            
            n_t <- flexsdm:::n_training(data = psa_data, partition = ".part")
            candidate_k <- 10
            min_k <- 3
            
            while (any(n_t < flexsdm:::n_coefficients(
                data = psa_data,
                predictors = var_names,
                k = candidate_k))) {
                
                candidate_k <- candidate_k - 1
                
                if(candidate_k < min_k) {
                    warning("k minimo; using k = 3")
                    candidate_k <- min_k
                    break
                }
            }
            
            flexsdm::fit_gam(
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                thr = "max_sorensen",
                k = candidate_k)
            
        }, error = function(e){
            warning("GAM failed: ", e$message)
            NULL
        })
    }
    
    ## gau ----
    if("gau" %in% methods){
        fitted_models$gau <- tryCatch({
            flexsdm::fit_gau(
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                background = bg_data,
                thr = "max_sorensen")
        }, error = function(e){
            warning("GAU failed: ", e$message)
            NULL
        })
    }
    
    ## gbm ----
    if("gbm" %in% methods){
        fitted_models$gbm <- tryCatch({
            
            tune_gbm_hyper <- expand.grid(
                n.trees = c(500, 1000, 1500),
                shrinkage = c(0.01, 0.05, 0.1),
                n.minobsinnode = c(5, 7, 10))
            
            flexsdm::tune_gbm(            
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                grid = tune_gbm_hyper,
                thr = "max_sorensen",
                metric = "SORENSEN",
                n_cores = n_cores)
            
        }, error = function(e){
            warning("GBM failed: ", e$message)
            NULL
        })
    }
    
    ## net -----
    if("net" %in% methods){
        fitted_models$net <- tryCatch({
            
            tune_net_hyper <- expand.grid(
                size = c(2, 3, 4, 5),
                decay = c(0.0001, 0.001, 0.01, 0.05, 0.1))
            
            flexsdm::tune_net(           
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                grid = tune_net_hyper,
                thr = "max_sorensen",
                metric = "SORENSEN",
                n_cores = n_cores)
            
        }, error = function(e){
            warning("NET failed: ", e$message)
            NULL
        })
    }
    
    ## raf -----
    if("raf" %in% methods){
        fitted_models$raf <- tryCatch({
            
            p <- length(var_names)
            tune_raf_hyper <- expand.grid(
                mtry = unique(round(c(sqrt(p), p/3, p/2))),
                ntree = c(400, 600, 800))
            
            flexsdm::tune_raf(           
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                grid = tune_raf_hyper,
                thr = "max_sorensen",
                metric = "SORENSEN",
                n_cores = n_cores)
            
        }, error = function(e){
            warning("RAF failed: ", e$message)
            NULL
        })
    }
    
    ## svm -----
    if("svm" %in% methods){
        fitted_models$svm <- tryCatch({
            
            tune_svm_hyper <- expand.grid(
                C = c(1, 4, 16, 64),
                sigma = c(0.001, 0.01, 0.05, 0.1))
            
            flexsdm::tune_svm(           
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                grid = tune_svm_hyper,
                thr = "max_sorensen",
                metric = "SORENSEN",
                n_cores = n_cores)
            
        }, error = function(e){
            warning("SVM failed: ", e$message)
            NULL
        })
    }
    
    ## max -----
    if("max" %in% methods){
        fitted_models$max <- tryCatch({
            
            tune_max_hyper <- expand.grid(
                regmult = seq(0.5, 4, 0.5),
                classes = c("l", "lq", "lqh", "lqhp"))
            
            flexsdm::tune_max(            
                data = psa_data,
                response = "pr_ab",
                predictors = var_names,
                partition = ".part",
                background = bg_data,
                grid = tune_max_hyper,
                thr = "max_sorensen",
                metric = "SORENSEN",
                n_cores = n_cores)
            
        }, error = function(e){
            warning("MAXENT failed: ", e$message)
            NULL
        })
    }
    
    # filter
    fitted_models <- fitted_models[!sapply(fitted_models, is.null)]
    
    # export models
    readr::write_rds(fitted_models, paste0(sp_dir_model, "03_01_mod_fitted_models", sp_name, ".rds"))
    
    # variable response ----
    var_imp <- list()
    var_res <- NULL
    for(m in 1:length(fitted_models)){
        
        mod_name <- names(fitted_models)[m]
        model_obj <- fitted_models[[m]]
        
        # plot
        p_vr <- p_pdp(model = model_obj$model, training_data = psa_data) +
            ggtitle(paste(i, "-", mod_name)) +
            theme(plot.title = element_blank())
        
        if (!is.null(p_vr)) {
            ggsave(filename = paste0(sp_dir_model, "03_02_mod_response_", sp_name, "_", mod_name, ".png"),
                   plot = p_vr, wi = 8, he = 6, dpi = 300)
        }
        
        # data
        d_vr <- NULL
        for(v in var_names){
            
            d_vr_v <- flexsdm::data_pdp(model = model_obj$model, 
                                        training_data = psa_data, 
                                        predictors = v)[[1]] %>% 
                tidyr::pivot_longer(cols = 1, names_to = "var") %>% 
                dplyr::mutate(model = mod_name, .before = 1) %>% 
                dplyr::select(model, var, value, Suitability)
            
            d_vr <- rbind(d_vr, d_vr_v)
        }
        
        var_res <- rbind(var_res, d_vr)
        
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
    
    # export 
    var_res <- dplyr::mutate(var_res, across(where(is.numeric), \(x) round(x, 3)))
    readr::write_csv(var_res, paste0(sp_dir_model, "03_02_mod_response_", sp_name, "_data.csv"))
    
    # variable importance ----
    var_imp <- dplyr::bind_rows(var_imp) %>% 
        dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))
    readr::write_csv(var_imp, paste0(sp_dir_model, "03_03_mod_importance_", sp_name, "_data.csv"))
    
    # plot
    var_imp_models <- var_imp %>%
        tidyr::pivot_longer(
            cols = TPR:IMAE,
            names_to = "metric",
            values_to = "value") %>%
        filter(metric %in% c("AUC", "TSS", "SORENSEN"))
    
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
        dplyr::mutate(across(where(is.numeric), \(x) round(x, 3)))
    readr::write_csv(fitted_models_performance, paste0(sp_dir_model, "03_04_mod_performance_", sp_name, ".csv"))
    
    # good models ----
    good_models_performance <- fitted_models_performance$model[
        fitted_models_performance$SORENSEN_mean >= sorensen_good_models &
            fitted_models_performance$thr_value > 0 &
            fitted_models_performance$thr_value < 1]
    
    good_models <- fitted_models[good_models_performance]
    readr::write_rds(good_models, paste0(sp_dir_model, "03_05_mod_good_models_", sp_name, ".rds"))
    
    # ensemble ----
    ensemble_all_models <- flexsdm::fit_ensemble(
        models = fitted_models,
        ens_method = "meanw",
        thr = "max_sorensen",
        thr_model = "max_sorensen",
        metric = "SORENSEN")
    
    readr::write_rds(ensemble_all_models, paste0(sp_dir_model, "03_06_mod_ensemble_all_models_", sp_name, ".rds"))
    readr::write_csv(ensemble_all_models$performance, paste0(sp_dir_model, "03_06_mod_ensemble_all_models_performance_", sp_name, ".csv"))
    
    # ensemble good ----
    if(length(good_models) > 1){
        
        ensemble_good_models <- flexsdm::fit_ensemble(
            models = good_models,
            ens_method = "meanw",
            thr = "max_sorensen",
            thr_model = "max_sorensen",
            metric = "SORENSEN")
        
        readr::write_rds(ensemble_good_models, paste0(sp_dir_model, "03_07_mod_ensemble_good_models_", sp_name, ".rds"))
        readr::write_csv(ensemble_good_models$performance, paste0(sp_dir_model, "03_07_mod_ensemble_good_models_performance_", sp_name, ".csv"))
        
    } else if(length(good_models) == 1){
        
        readr::write_rds(good_models, paste0(sp_dir_model, "03_07_mod_ensemble_good_models_", sp_name, ".rds"))
        readr::write_csv(fitted_models_performance[fitted_models_performance$model == good_models_performance, ], paste0(sp_dir_model, "03_07_mod_ensemble_good_models_performance_", sp_name, ".csv"))        
        
    }else{
        
    }
    
    # clean memory ----
    gc(verbose = FALSE, reset = TRUE)
    
}

# end ---------------------------------------------------------------------
