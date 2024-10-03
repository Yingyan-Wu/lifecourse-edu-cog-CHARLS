# Time-varying g comp -- cluster script
# Created by: Yingyan Wu
# Date: Aug.19.2024

#---- Argument ----
# Read in the arguments listed in the:
# R CMD BATCH --no-save --no-restore "--args scenario_num=$SGE_TASK_ID"  
## expression:
args = (commandArgs(TRUE))

# Check to see if arguments are passed and set default values if not.
# If so, parse the arguments. (I only have one argument here.)
if (length(args) == 0) {
  print("No arguments supplied.")
  ##supply default values
  scenario_num <- 1
} else {
  for (i in 1:length(args)) {
    eval(parse(text = args[[i]])) # parse each argument (only have 1 here)
  }
}

#---- Package loading + option specifications ----
library(tidyverse)
library(magrittr)
library(survey)
library(WeMix)
# No scientific notation
options(scipen = 999)
# Needed if plyr was loaded
# select <- dplyr::select

#---- Load the data ----
# Paths (local)
# imp_pmm_10 <- readRDS(here::here("data", "analysis_data", "imp_pmm_10_stacked.RDS"))
# imp_pmm_long <- readRDS( here::here("data", "analysis_data", "imp_pmm_10_long.RDS"))

# # Paths cluster
project_folder <- paste0("/u/home/y/yingyanw/edu_cog_CHARLS/")
imp_pmm_10 <- readRDS(paste0(project_folder, "data/imp_pmm_10_stacked.RDS"))
imp_pmm_long <- readRDS(paste0(project_folder, "data/imp_pmm_10_long.RDS")) %>%
  mutate(w_obs = 1)

#---- Test ----
# # Test data
# id_test <- head(unique(imp_pmm_10$ID), 100)
# imp_pmm_test <- imp_pmm_10 %>% filter(ID %in% id_test)
# imp_pmm_long_test <- imp_pmm_long %>% filter(ID %in% id_test) %>%
#   mutate(w_obs = 1)
# 
# dataset <- imp_pmm_test
# dataset_long <- imp_pmm_long_test
# x0 = "childhood_edu"
# x1 = "continuing_edu"
# y = "imrc"
# c0 = c0
# c1 = c1
# i_boot = 1
# bootstrap = i_boot != 0

#---- function ----
# x0, x1 to be categorical (factored already)
joint_effect_g_comp <- function(dataset, dataset_long, 
                                x0, x1, c0, c1, y, bootstrap = TRUE){
  
  set.seed(i_boot*1000)
  
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
              relationship = "many-to-many") %>%
    filter(!is.na(!!sym(y)))
  
  # For weighted models for exposures and confounders
  data_boot_svy <- svydesign(ids = ~newid, weights = ~r1wtrespb_rescaled, 
                             data = data_boot)
  
  #---- Step 1: Obtain f(x0), f(x1) from the observed distribution ----
  # Estimating marginal expectation of X0 and X1
  # ordered logistic regression intercept only model did not work here
  # multinomial logistic regression prediction does not work well (all in the categories with largest prop)
  chd_edu_prop <- svymean(~childhood_edu, data_boot_svy)
  ctn_edu_prop <- svymean(~continuing_edu, data_boot_svy)
  
  #---- Step 2: Fit a model for C1 given X0 and C0 (lambdas)----
  for (c1_var in c1_binary){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    warning_vec <- get(paste0("w_", c1_var))
    tryCatch({
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = quasibinomial(link = "logit"),
                       control = list(maxit = 100))
      # If no warnings, append 0s
      warning_vec <<- c(warning_vec, 0)
    }, warning = function(w) {
      # If there's warning, append 1s
      warning_vec <<- c(warning_vec, 1)
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = quasibinomial(link = "logit"),
                       control = list(maxit = 100))
    })
    assign(paste0("pc1_model_", c1_var), model)
    assign(paste0("w_", c1_var), warning_vec)
  }
  
  for (c1_var in c1_continuous){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    warning_vec <- get(paste0("w_", c1_var))
    tryCatch({
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = gaussian(link = "identity"))
      # If no warnings, append 0s
      warning_vec <<- c(warning_vec, 0)
      assign(paste0("w_", c1_var), c(get(paste0("w_", c1_var)), 0))
    }, warning = function(w) {
      # If there's warning, append 1s
      warning_vec <<- c(warning_vec, 1)
      model <<- svyglm(as.formula(c1_model_form),
                       design = data_boot_svy,
                       family = gaussian(link = "identity"))
    })
    assign(paste0("pc1_model_", c1_var), model)
    assign(paste0("w_", c1_var), warning_vec)
  }
  
  #---- Step 3: Fit a model for Y given X0, X1, C0 and C1 (Q model, betas) ----
  q_start <- Sys.time()
  pYx0x1c0c1_model <- WeMix::mix(
    as.formula(paste0(y, " ~ agey_v3 + (1 + agey_v3 |newid) +", # age as time scale
                      x0, " * agey_v3 *", x1, " + ",
                      paste0("first_", y), "+", # practice effect term
                      paste(c0, collapse = "+"), "+",
                      paste(c1, collapse = "+"))),
    data = data_boot_long,
    weights = c("w_obs", "r1wtrespb_rescaled"),
    cWeights = TRUE)
  # Since tryCatch needs to run the model twice, to save time, use length(warnings)
  if (length(warnings()) > 0){
    w_qmodel <- c(w_qmodel, 1)
    w_qmodel_msg <- c(w_qmodel_msg, attr(warnings(), "name"))
  } else {
    w_qmodel <- c(w_qmodel, 0)
    w_qmodel_msg <- c(w_qmodel_msg, NA_character_)
  }
  q_end  <- Sys.time()
  # In previous test runs
  # 6.99mins for n=1000, N=3905
  # 3.3 h for n = 24070, N = 82155
  
  #---- Step 4: Simulate post intervention DAG distribution----
  data_boot %<>%
    dplyr::mutate(
      # Intervention X (categorical)
      doX0 = sample(0:3, n, replace = TRUE, prob = as.vector(chd_edu_prop)),
      doX1 = sample(0:2, n, replace = TRUE, prob = as.vector(ctn_edu_prop))) %>%
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
  
  # Merge simulated variables with the long dataset
  data_boot_long %<>%
    select(-contains(c("doX0", "doX1"))) %>%
    left_join(data_boot %>% select(newid, contains(c("doX0", "doX1"))), by = "newid")
  
  # # predict(wemix) preduce the linear combination of coefficients and covariate 
  # # values + random intercept + random slope * wave.
  data_boot_long %<>%
    mutate(
      Y_doX0_doX1 = predict(
        pYx0x1c0c1_model, 
        newdata = data_boot_long %>% select(-all_of(x0), -all_of(x1), -all_of(c1)) %>%
          dplyr::rename(!!sym(x0) := doX0, !!sym(x1) := doX1) %>%
          # rename c1 to observed c1 variable names
          rename_with(~ gsub("_doX0", "", .x)),
        type = "response"),
      Y_doX0_doX1_round = round(Y_doX0_doX1, 2))
  
  #---- Step 5: MSM for potential outcomes ~ doX0*doX1 ----
  msm_start <- Sys.time()
  MSM_wtd_lmm <- WeMix::mix(
    Y_doX0_doX1_round ~ agey_v3 + (1 + agey_v3 |newid) + doX0 * doX1 * agey_v3,
    data = data_boot_long,
    weights = c("w_obs", "r1wtrespb_rescaled"),
    cWeights = TRUE)
  # Since tryCatch needs to run the model twice, to save time, use length(warnings)
  if (length(warnings()) > 0){
    w_msm <- c(w_msm, 1)
    w_msm_msg <- c(w_msm_msg, attr(warnings(), "name"))
  } else {
    w_msm <- c(w_msm, 0)
    w_msm_msg <- c(w_msm_msg, NA_character_)
  }
  msm_end <- Sys.time()
  
  coef_tib <- MSM_wtd_lmm$coef %>% t() %>% as.data.frame()
  colnames(coef_tib) <- gsub("(doX)([0-9])([0-9])", "\\1\\2_\\3", colnames(coef_tib))
  coef_tib %<>%
    mutate(RI_var = MSM_wtd_lmm$vars[[1]], # newid.intercept
           RS_var = MSM_wtd_lmm$vars[[2]], # newid.agey_v3 (slope)
           RIRS_cor = summary(MSM_wtd_lmm)$varsmat[["Corr1"]][[2]],
           res_var = MSM_wtd_lmm$vars[["Residual"]])
  end_time <- Sys.time()
  
  results_tib <- tibble(
    n_imp = num_imp,
    i_boot = i_boot,
    scenario_num = scenario_num,
    computing_time_min = difftime(end_time, start_time, units = "hours"),
    q_model_time_min = difftime(q_end, q_start, units = "hours"),
    msm_model_time_min = difftime(msm_end, msm_start, units = "hours")) %>%
    cbind(coef_tib)
  
  return(results_tib)
}

#---- Variables ----
c0 <- c(
  # "r1agey_v3", # age as timesclae so drop this
  "female", "first_child_v2", 
  "rameduc_2cat", "rafeduc_2cat", "ramoccup_c", "rafoccup_c",
  "childhood_1st_region", "rarural_child", 
  "first_hukou_2cat")
c1_continuous <- c("chldstate_30")
c1_binary <- c("marital_30", "rural_30", "occupation_30_2cat",
               "hukou_30_2cat")
# c1_multinomial <- c("hukou_30")
c1 <- c(c1_continuous, c1_binary)
# , c1_multinomial)

#---- Scenario tibble ----
# n_imp = 10
# n_bootstrap_per_job = 10
# n_bootstrap = 1100

# First run
n_imp = 1
n_bootstrap_per_job = 1
n_bootstrap = 10

scenario_tib <- expand_grid(
  imp = 1:n_imp,
  boot_i = c(paste0(seq(1, n_bootstrap - n_bootstrap_per_job + 1, 
                        by = n_bootstrap_per_job), "-",
                    seq(n_bootstrap_per_job, n_bootstrap, 
                        by = n_bootstrap_per_job)))) %>%
  separate(boot_i, into = c("boot_i_start", "boot_i_end"), sep = "-") %>%
  mutate_at(vars(boot_i_start), ~ifelse(boot_i_start == 1, 0, .))

# Make sure that scenario_num is not greater than all the scenario index
if (scenario_num > nrow(scenario_tib)) {
  print(paste0("out of bound senario number", scenario_num))
  # supply default value
  scenario_num <- 1
}

# print values just to make sure:
print(scenario_num)
scenario <- scenario_tib[scenario_num, ]
boot_i_start <- scenario$boot_i_start
boot_i_end <- scenario$boot_i_end
num_imp <- scenario$imp

print(boot_i_start)
print(boot_i_end)
print(num_imp)

# Warning number capture vectors (start with 0)
w_c1s <- setNames(rep(list(numeric()), length(c1)), paste0("w_", c1))
list2env(w_c1s, envir = .GlobalEnv)

w_qmodel <- w_msm <- numeric()
w_qmodel_msg <- w_msm_msg <- vector()

#---- bootstrap ----
for (i_boot in boot_i_start:boot_i_end) {
  print(i_boot)
  # boot_i_start <- 0
  # i_boot <- 1
  start <- Sys.time() # record starting time
  result <- 
    joint_effect_g_comp(
      imp_pmm_10, imp_pmm_long,
      x0 = "childhood_edu", x1 = "continuing_edu", y = "imrc",
      c0 = c0, c1 = c1, bootstrap = i_boot != 0)
  end <- Sys.time() # record ending time
  saveRDS(result, paste0(project_folder, "model_results/",
                         paste0("MSM_coef_", i_boot, ".RDS")))
  
  w_boot_i <- tibble(
    i_boot = i_boot,
    scenario = scenario_num,
    duration = difftime(end, start, units = 'mins'),
    w_qmodel = sum(w_qmodel),
    w_msm = sum(w_msm),
    w_qmodel_msg = w_qmodel_msg,
    w_msm_msg = w_msm_msg) %>%
    bind_cols(as.tibble(lapply(mget(paste0("w_", c1)), sum))) # c1 model warnings
  saveRDS(w_boot_i, paste0(project_folder, "warnings/",
                           paste0("warnings_", i_boot, ".RDS")))
}



#---- OLD ----
# Will move to bootstrap results script
# coef_tib %>%
#   mutate(
#     # Both education vs. No formal education at all
#     slope_11_00 = doX0_1 + doX1_1 + `doX0_1:doX1_1` + `wave:doX0_1` + `wave:doX1_1` + `wave:doX0_1:doX1_1`,
#     slope_21_00 = doX0_2 + doX1_1 + `doX0_2:doX1_1` + `wave:doX0_2` + `wave:doX1_1` + `wave:doX0_2:doX1_1`,
#     slope_31_00 = doX0_3 + doX1_1 + `doX0_3:doX1_1` + `wave:doX0_3` + `wave:doX1_1` + `wave:doX0_3:doX1_1`,
#     slope_12_00 = doX0_1 + doX1_2 + `doX0_1:doX1_2` + `wave:doX0_1` + `wave:doX1_2` + `wave:doX0_1:doX1_2`,
#     slope_22_00 = doX0_2 + doX1_2 + `doX0_2:doX1_2` + `wave:doX0_2` + `wave:doX1_2` + `wave:doX0_2:doX1_2`,
#     slope_32_00 = doX0_3 + doX1_2 + `doX0_3:doX1_2` + `wave:doX0_3` + `wave:doX1_2` + `wave:doX0_3:doX1_2`,
#     # Only continuing education vs. No formal education at all
#     slope_01_00 = doX1_1 + `wave:doX1_1`,
#     slope_02_00 = doX1_2 + `wave:doX1_2`,
#     # Both education vs. No continuing education with the same early life edu intervention
#     slope_11_10 = doX1_1 + `doX0_1:doX1_1` + `wave:doX1_1` + `wave:doX0_1:doX1_1`,
#     slope_21_20 = doX1_1 + `doX0_2:doX1_1` + `wave:doX1_1` + `wave:doX0_2:doX1_1`,
#     slope_31_30 = doX1_1 + `doX0_3:doX1_1` + `wave:doX1_1` + `wave:doX0_3:doX1_1`,
#     slope_12_10 = doX1_2 + `doX0_1:doX1_2` + `wave:doX1_2` + `wave:doX0_1:doX1_2`,
#     slope_22_20 = doX1_2 + `doX0_2:doX1_2` + `wave:doX1_2` + `wave:doX0_2:doX1_2`,
#     slope_32_30 = doX1_2 + `doX0_3:doX1_2` + `wave:doX1_2` + `wave:doX0_3:doX1_2`,
#     # Only early life education (no continuing education) vs. No formal education at all
#     slope_10_00 = doX0_1 + `wave:doX0_1`,
#     slope_20_00 = doX0_2 + `wave:doX0_2`,
#     slope_30_00 = doX0_3 + `wave:doX0_3`)

