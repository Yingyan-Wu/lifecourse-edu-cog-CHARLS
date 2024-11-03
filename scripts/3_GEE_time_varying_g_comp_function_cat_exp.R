# Time-varying g comp (GEE)
# Created by: Yingyan Wu
# Date: Aug.19.2024

# #---- Test ----
# dataset <- imp_pmm_stacked
# dataset_long <- imp_pmm_long
# i_boot = 1
# bootstrap = i_boot != 0

#---- function ----
# x0, x1 to be categorical (factored already)
joint_effect_g_comp <- function(dataset, dataset_long, bootstrap = TRUE){
  set.seed(i_boot)
  
  start_time <- Sys.time()
  if (bootstrap == T){
    boot_ids <- sample(unique(dataset$ID), length(unique(dataset$ID)), replace = T)
  } else {
    boot_ids <- unique(dataset$ID)
  }
  
  # for confounders
  data_boot <- tibble(
    ID = boot_ids,
    newid = 1:length(boot_ids)) %>%
    left_join(dataset %>% filter(imp == num_imp), by = "ID")
  
  n <- length(boot_ids)
  
  # for main model and MSM
  data_boot_long <- data_boot %>%
    select(ID, newid) %>%
    left_join(dataset_long %>% filter(imp == num_imp), by = "ID",
              relationship = "many-to-many")
  
  # For weighted models for exposures and confounders
  data_boot_svy <- svydesign(ids = ~newid, weights = ~get(svy_wt), 
                             data = data_boot)
  data_boot_long_svy <- svydesign(ids = ~newid, weights = ~get(svy_wt), 
                                  data = data_boot_long)
  
  #---- Step 1: Obtain f(x0), f(x1) from the observed distribution ----
  # Estimating marginal expectation of X0 and X1
  # ordered logistic regression intercept only model did not work here
  # multinomial logistic regression prediction does not work well (all in the categories with largest prop)
  chd_edu_prop <- svymean(as.formula(paste0("~", x0)), data_boot_svy)
  ctn_edu_prop <- svymean(as.formula(paste0("~", x1)), data_boot_svy)
  
  #---- Step 2: Fit a model for C1 given X0 and C0 (lambdas)----
  for (c1_var in c1_binary){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    warning_vec <- w_c1s[[paste0("w_", c1_var)]]
    tryCatch({
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = quasibinomial(link = "logit"),
                       control = list(maxit = 100))
      # If no warnings, append 0s
      w_c1s[[paste0("w_", c1_var)]] <<- c(warning_vec, 0)
    }, warning = function(w) {
      # If there's warning, append 1s
      w_c1s[[paste0("w_", c1_var)]] <<- c(warning_vec, 1)
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = quasibinomial(link = "logit"),
                       control = list(maxit = 100))
    })
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  for (c1_var in c1_continuous){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    warning_vec <- w_c1s[[paste0("w_", c1_var)]]
    tryCatch({
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = gaussian(link = "identity"))
      # If no warnings, append 0s
      w_c1s[[paste0("w_", c1_var)]] <<- c(warning_vec, 0)
    }, warning = function(w) {
      # If there's warning, append 1s
      w_c1s[[paste0("w_", c1_var)]] <<- c(warning_vec, 1)
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = gaussian(link = "identity"))
    })
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  #---- Step 3: Devlop IPCW ----
  # for each wave of outcome, create IPCW
  # in long dataset
  outcome_censor <- paste0(y, "_inw")
  pcnum_formula <- paste0(outcome_censor, " ~ fu_time")
  pcden_formula <- paste0(outcome_censor, " ~ fu_time + ",  x0, " * ", x1, " + ",
                          paste(c0, collapse = "+"), " + ",
                          paste(c1, collapse = "+"), " + ",
                          paste(ipcw_c, collapse = "+"))
  
  # Numerator
  tryCatch({
    pcnum_model <<- svyglm(as.formula(pcnum_formula),
                           design = data_boot_long_svy,
                           family = quasibinomial(link = "logit"),
                           control = list(maxit = 100))
    # If no warnings, append 0s
    w_pcnum <<- c(w_pcnum, 0)
  }, warning = function(w) {
    # If there's warning, append 1s
    w_pcnum <<- c(w_pcnum, 1)
    pcnum_model <<- svyglm(as.formula(pcnum_formula),
                           design = data_boot_long_svy,
                           family = quasibinomial(link = "logit"),
                           control = list(maxit = 100))
  })
  # Denominator
  tryCatch({
    pcden_model <<- svyglm(as.formula(pcden_formula),
                           design = data_boot_long_svy,
                           family = quasibinomial(link = "logit"),
                           control = list(maxit = 100))
    # If no warnings, append 0s
    w_pcden <<- c(w_pcden, 0)
  }, warning = function(w) {
    # If there's warning, append 1s
    w_pcden <<- c(w_pcden, 1)
    pcden_model <<- svyglm(as.formula(pcden_formula),
                           design = data_boot_long_svy,
                           family = quasibinomial(link = "logit"),
                           control = list(maxit = 100))
  })
  
  data_boot_long %<>%
    mutate(
      pred_pcnum = predict(pcnum_model, newdata = data_boot_long, type = "response"),
      pred_pcden = predict(pcden_model, newdata = data_boot_long, type = "response")) %>%
    group_by(newid) %>% 
    mutate(
      cum_pred_pcnum = cumprod(pred_pcnum), 
      cum_pred_pcden = cumprod(pred_pcden)
    ) %>% 
    ungroup()
  
  data_boot_long %<>%
    mutate(ipcw = 1/cum_pred_pcden,
           sipcw = cum_pred_pcnum/cum_pred_pcden,
           # truncate weights
           # remember to remove na.rm = TRUE after imputation
           ipcw_trunc = case_when(
             ipcw > quantile(ipcw, 0.99) ~ quantile(ipcw, 0.99),
             TRUE ~ ipcw), 
           sipcw_trunc = case_when(
             sipcw > quantile(sipcw, 0.99) ~ quantile(sipcw, 0.99),
             TRUE ~ sipcw))
  
  data_boot_long %<>%
    mutate(svy_ipcw = sipcw_trunc * get(svy_wt))
  
  data_boot_long %<>%
    filter(!is.na(!!sym(y)))
  
  #---- Step 3: Fit a model for Y given X0, X1, C0 and C1 (Q model, betas) ----
  # check fu_yr for each participant to see if they have the same fu_yr
  # 
  q_start <- Sys.time()
  tryCatch({
    qmodel_gee <<- geepack::geeglm(as.formula(paste0(y, " ~ ", x0, " * fu_time * ", 
                                                     x1, " + ", 
                                                     paste(c0, collapse = "+"), " + ", 
                                                     paste(c1, collapse = "+"))),
                                   data = data_boot_long,
                                   id = newid,
                                   waves = n_fu,
                                   weights = svy_ipcw,
                                   family = gaussian)
    w_qmodel <<- c(w_qmodel, 0)
    w_qmodel_msg <<- c(w_qmodel_msg, NA_character_)
  }, warning = function(w) {
    # If there's warning, append 1s
    w_qmodel <<- c(w_qmodel, 1)
    w_qmodel_msg <<- c(w_qmodel_msg, attr(warnings(), "name"))
    qmodel_gee <<- geepack::geeglm(as.formula(paste0(y, " ~ ", x0, " * fu_time * ", 
                                                     x1, " + ", 
                                                     paste(c0, collapse = "+"), " + ", 
                                                     paste(c1, collapse = "+"))),
                                   data = data_boot_long,
                                   id = newid,
                                   waves = n_fu,
                                   weights = svy_ipcw,
                                   family = gaussian)
  })
  q_end  <- Sys.time()
  # 
  # test <- geepack::geeglm(as.formula(paste0(y, " ~ ", x0, " * fu_time * ", 
  #                                           x1, " + n_fu +", 
  #                                           paste(c0, collapse = "+"), " + ", 
  #                                           paste(c1, collapse = "+"))),
  #                         data = data_boot_long,
  #                         id = newid,
  #                         waves = n_fu,
  #                         weights = svy_ipcw,
  #                         family = gaussian)
  # 
  # QIC(qmodel_gee)
  # QIC(test) # original model has a lower QIC than the model adding num follow up indicator (as practice effect)
  # summary(q_model_gee)
  
  #---- Step 4: Simulate post intervention DAG distribution----
  data_boot %<>%
    dplyr::mutate(
      # Intervention X (categorical)
      doX0 = sample(0:3, n, replace = TRUE, prob = as.vector(chd_edu_prop)),
      doX1 = sample(0:1, n, replace = TRUE, prob = as.vector(ctn_edu_prop))) %>%
    mutate(across(c(doX0, doX1), as.factor))
  
  # Simulate C1
  for (c1_var in c1){
    var_type <- "response"
    # var_type <- case_when(c1_var %in% c1_continuous ~ "response",
    # c1_var %in% c1_binary ~ "response",
    # c1_var %in% c1_multinomial ~ "class")
    if (c1_var %in% c1_binary){
      data_boot %<>%
        mutate(!!paste0(c1_var, "_doX0") := 
                 rbinom(n, size = 1, prob = 
                          predict(get(paste0("pc1_model_", c1_var)),
                                  newdata = data_boot %>% select(-all_of(x0)) %>%
                                    dplyr::rename(!!sym(x0) := "doX0"),
                                  type = var_type)) %>% as.factor)
    } else if (c1_var %in% c1_continuous){
      data_boot %<>%
        mutate(!!paste0(c1_var, "_doX0") :=
                 predict(get(paste0("pc1_model_", c1_var)),
                         newdata = data_boot %>% select(-all_of(x0)) %>%
                           dplyr::rename(!!sym(x0) := "doX0"),
                         type = var_type))
    }
  }
  # Round simulated variables "# of children at 30" to integers
  data_boot %<>% mutate(chldstate_30_doX0 = round(chldstate_30_doX0))
  
  
  # Create a balanced long dataset
  cloned_data_long <- data_boot %>%
    select(newid, imp) %>%
    expand_grid(n_fu = 1:5) %>%
    mutate(fu_time = case_when(n_fu == 1 ~ 0,
                               n_fu == 2 ~ 2,
                               n_fu == 3 ~ 2,
                               n_fu == 4 ~ 3,
                               n_fu == 5 ~ 2)) %>%
    left_join(data_boot %>% select(newid, all_of(c0), 
                                   contains(c("doX0", "doX1"))),
              by = "newid")
  
  # Predict potential outcomes
  cloned_data_long %<>%
    mutate(
      Y_doX0_doX1 = predict(
        qmodel_gee, 
        newdata = cloned_data_long %>% 
          dplyr::rename(!!sym(x0) := doX0, !!sym(x1) := doX1) %>%
          # rename c1 to observed c1 variable names
          rename_with(~ gsub("_doX0", "", .x)),
        type = "response"))
  # Y_doX0_doX1_round = round(Y_doX0_doX1, 2))
  
  #---- Step 5: MSM for potential outcomes ~ doX0*doX1 ----
  msm_start <- Sys.time()
  tryCatch({
    msm_gee <<- geepack::geeglm(Y_doX0_doX1 ~ fu_time + doX0 * doX1 * fu_time,
                                data = cloned_data_long,
                                id = newid,
                                waves = n_fu,
                                family = gaussian)
    w_msm <<- c(w_msm, 0)
    w_msm_msg <<- c(w_msm_msg, NA_character_)
  }, warning = function(w) {
    # If there's warning, append 1s
    w_msm <<- c(w_msm, 1)
    w_msm_msg <<- c(w_msm_msg, attr(warnings(), "name"))
    msm_gee <<- geepack::geeglm(Y_doX0_doX1 ~ fu_time + doX0 * doX1 * fu_time,
                                data = cloned_data_long,
                                id = newid,
                                waves = n_fu,
                                family = gaussian)
  })
  msm_end <- Sys.time()
  
  coef_tib <- msm_gee$coef %>% t() %>% as.data.frame()
  colnames(coef_tib) <- gsub("(doX)([0-9])([0-9])", "\\1\\2_\\3", colnames(coef_tib))
  end_time <- Sys.time()
  
  results_tib <- tibble(
    n_imp = num_imp,
    i_boot = i_boot,
    scenario_num = scenario_num,
    computing_time_sec = difftime(end_time, start_time, units = "secs"),
    q_model_time_sec = difftime(q_end, q_start, units = "secs"),
    msm_model_time_sec = difftime(msm_end, msm_start, units = "secs")) %>%
    cbind(coef_tib)
  
  return(results_tib)
}
