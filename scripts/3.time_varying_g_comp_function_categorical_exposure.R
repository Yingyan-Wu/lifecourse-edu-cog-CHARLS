# Time-varying g comp function
# Created by: Yingyan Wu

require(survey, nnet, magrittr) # package for multinomial logistic regression 
options(scipen = 999)
pacman::p_load("haven", "magrittr", "plyr", "tidyverse", "survey", "nnet")
select <- dplyr::select

imp_pmm_40 <- readRDS(here::here("data", "analysis_data", "imp_pmm_40_stacked.RDS"))
imp_pmm_40 %<>%
  dplyr::mutate(occupation_30_5cat = 
                  as.factor(case_when(occupation_30 %in% c(1, 2) ~ 1,
                                      occupation_30 %in% c(3, 4, 5) ~ 2,
                                      occupation_30 == 6 ~ 3,
                                      occupation_30 == 7 ~ 4,
                                      occupation_30 == 8 ~ 5)),
                occupation_30_2cat = case_when(occupation_30 %in% c(1, 2) ~ 0, # agriculture
                                               occupation_30 %in% c(3:8) ~ 1), # non-agriculture
                rameduc_2cat = case_when(rameduc_c %in% 1:3 ~ 1, # with formal edu
                                         rameduc_c == 0 ~ 0),# no formal edu
                rafeduc_2cat = case_when(rafeduc_c %in% 1:3 ~ 1, 
                                         rafeduc_c == 0 ~ 0),
                first_hukou_2cat = case_when(first_hukou == 1 ~ 0, # agricultural hukou
                                             first_hukou %in% 2:3 ~ 1), # non agricultural hukou
                childhood_edu = factor(as.character(childhood_edu)),
                continuing_edu = factor(as.character(continuing_edu)),
                chld_edu_2cat_10 = case_when(childhood_edu == 1 ~ 1,
                                             childhood_edu == 0 ~ 0),
                chld_edu_2cat_20 = case_when(childhood_edu == 2 ~ 1,
                                             childhood_edu == 0 ~ 0),
                chld_edu_2cat_30 = case_when(childhood_edu == 3 ~ 1,
                                             childhood_edu == 0 ~ 0),
                ctn_edu_2cat_10 = case_when(continuing_edu == 1 ~ 1,
                                            continuing_edu == 0 ~ 0),
                ctn_edu_2cat_20 = case_when(continuing_edu == 2 ~ 1,
                                            continuing_edu == 0 ~ 0),
                hukou_30_2cat = case_when(hukou_30 == 1 ~ 0,
                                          hukou_30 %in% 2:3 ~ 1))
c0 <- c("r1agey_v3", "female_v2", "first_child_v2", 
        "rameduc_2cat", "rafeduc_2cat", "ramoccup_c", "rafoccup_c",
        "childhood_1st_region", "rarural_child", 
        "first_hukou_2cat")
c1_continuous <- c("chldstate_30")
c1_binary <- c("marital_30", "rural_30", "occupation_30_2cat",
               "hukou_30_2cat")
# c1_multinomial <- c("hukou_30")
c1 <- c(c1_continuous, c1_binary)
# , c1_multinomial)
num_imp = 1

test_dataset <- imp_pmm_40 %>% filter(imp == num_imp) %>%
  select(ID, childhood_edu, continuing_edu, 
         contains("chld_edu_"), contains("ctn_edu_"),
         all_of(c0), all_of(c1),
         paste0("r", 1:2, "imrc"),
         paste0("r", 3, "imrc_cleaned"),
         paste0("r", 4:5, "imrc_eqt"),
         paste0("r", 1:2, "dlrc"),
         paste0("r", 3, "dlrc_cleaned"),
         paste0("r", 4:5, "dlrc_eqt"),
         paste0("r", 1:5, "wtrespb")) %>%
  dplyr::rename_at(vars(contains(c("imrc", "dlrc"))),
                   function(x) str_replace(x, "_eqt|_cleaned", ""))
  # head(1000)

rctib_long <- test_dataset %>%
  select(ID, contains(c("imrc", "dlrc"))) %>%
  pivot_longer(cols = !ID, 
               names_to = c("wave", ".value"), 
               names_pattern = "r(.)(.*)") %>%
  mutate(wave = as.numeric(wave))

imrc_long <- rctib_long %>% dplyr::select(-dlrc) %>%
  na.omit() %>%
  arrange(ID, wave) %>%
  group_by(ID) %>%
  dplyr::mutate(num_occ_imrc = 1:n()) %>%
  ungroup()

dlrc_long <- rctib_long %>% dplyr::select(-imrc) %>%
  na.omit() %>%
  arrange(ID, wave) %>%
  group_by(ID) %>%
  dplyr::mutate(num_occ_dlrc = 1:n()) %>%
  ungroup()

test_long <- test_dataset %>%
  select(-contains(c("imrc", "dlrc"))) %>%
  left_join(imrc_long, by = "ID") %>%
  full_join(dlrc_long, by = c("ID", "wave")) %>%
  mutate(first_imrc = ifelse(num_occ_imrc == 1, 1, 0),
         first_dlrc = ifelse(num_occ_dlrc == 1, 1, 0))

rm(dlrc_long, imrc_long, rctib_long, imp_pmm_40)

#---- function ----
# x0, x1 to be categorical (factored already)
# Dataset were filtered to contain only one imputation before feed into the function
joint_effect_g_comp <- function(dataset, x0, x1, c0, c1, y, bootstrap = TRUE){
  dataset <- test_dataset
  dataset_long <- test_long
  x0 = "childhood_edu"
  x1 = "continuing_edu"
  y = "imrc"
  c0 = c0
  c1 = c1
  bootstrap = F
  
  if (bootstrap == T){
    data_boot <- 
      dataset[sample(nrow(dataset), nrow(dataset), replace = TRUE), ]
  } else {
    data_boot <- dataset
  }
  
  data_boot %<>%
    mutate(newid = 1:n())
  
  n <- nrow(data_boot)
  
  data_boot_long <- data_boot %>% select(ID, newid) %>%
    left_join(dataset_long, by = "ID", relationship = "many-to-many") %>%
    filter(!is.na(!!sym(y)))
  
  data_boot_svy <- svydesign(ids = ~newid, weights = ~r1wtrespb, 
                             data = data_boot)
  
  #---- Step 1: Obtain f(x0), f(x1) from the observed distribution ----
  #Estimating marginal expectation of X0 and X1
  # ordered logistic regression intercept only model did not work here
  # multinomial logistic regression prediction does not work well (all in the categories with largest prop)
  chd_edu_prop <- svymean(~childhood_edu, data_boot_svy)
  ctn_edu_prop <- svymean(~continuing_edu, data_boot_svy)
  
  #---- Step 2: Fit a model for C1 given X0 and C0 (lambdas)----
  # # The documentation of nnet::multinom about weights argument is not clear
  # # In the doc "weights: optional case weights in fitting."
  # # Test if weights in nnet::multinom is applying survey weight
  # test1 <- nnet::multinom(ctn_edu_2cat_10 ~ female_v2 + rural_30,
  #                         data = data_boot,
  #                         weights = r1wtrespb)
  # summary(test1)
  # test2 <- svyglm(ctn_edu_2cat_10 ~ female_v2 + rural_30,
  #                 design = data_boot_svy, 
  #                 family = quasibinomial(link = "logit"))
  # summary(test2)
  # # The results are almost the same
  
  # # No multinomial variables any more
  # for (c1_var in c1_multinomial){
  #   c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
  #                           paste(c0, collapse = "+"))
  #   model <- nnet::multinom(as.formula(c1_model_form), data = data_boot,
  #                           weights = r1wtrespb)
  #   assign(paste0("pc1_model_", c1_var), model)
  # }
  
  for (c1_var in c1_binary){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    model <- svyglm(as.formula(c1_model_form),
                    design = data_boot_svy,
                    family = quasibinomial(link = "logit"))
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  for (c1_var in c1_continuous){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    model <- 
      # lm(as.formula(c1_model_form), data = data_boot,
      #    weights = r1wtrespb) # the same as svyglm
      svyglm(as.formula(c1_model_form),
             design = data_boot_svy,
             family = gaussian(link = "identity"))
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  #---- Step 3: Fit a model for Y given X0, X1, C0 and C1 (Q model, betas) ----
  # p_load("usethis")
  # usethis::edit_r_environ("project")
  # change R_MAX_VSIZE=64000000000 in the environ file
  data_boot_long %<>% mutate(w_obs = 1)
  # a <- Sys.time()
  # tictoc::tic()
  # pYx0x1c0c1_model <- WeMix::mix(
    # as.formula(paste0(y, " ~ wave + (1 + wave |newid) +", x0, " * wave *", x1, " + ",
    #                   paste(c0, collapse = "+"), "+",
    #                   paste(c1, collapse = "+"))),
  #   data = data_boot_long,
  #   weights = c("w_obs", "r1wtrespb"),
  #   cWeights = TRUE)
  # # 67s for n=1000, N=3874
  # # 3.3 h for n = 24070, N = 82155
  # tictoc::toc()
  
  # save(pYx0x1c0c1_model, file = here::here("data", "model_results", "q_model_imp1.Rdata"))
  load(here::here("data", "model_results", "q_model_imp1.Rdata"))
  
  #---- Step 4: Simulate post intervention DAG distribution----
  set.seed(62283)
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
                                  type = var_type)))
    } else {
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
  # # # Sanity check
  # data_boot %>% select(contains("doX0")) %>% apply(., 2, table)
  # data_boot %>% select(contains("occupation_30_2cat")) %>% apply(., 2, table)
  # data_boot %>% select(contains("hukou_30")) %>% apply(., 2, table)
  
  # Merge simulated variables with the long dataset
  data_boot_long %<>%
    left_join(data_boot %>% select(newid, contains(c("doX0", "doX1"))), by = "newid")
  
  # Pending on cleaning the variables!!!!
  data_boot_long %<>%
    mutate(marital_30_doX0 = as.factor(marital_30_doX0),
           rural_30_doX0 = as.factor(rural_30_doX0))
  
  # Simulate Potential outcomes
  # # Test to see what predict wemix model produce
  # test_mat <- data_boot_long %>% head(1000) %>% 
  #   select(newid, wave, all_of(x0), all_of(x1), w_obs, r1wtrespb, imrc)
  # test <- WeMix::mix(
  #   as.formula(paste0(y, " ~ wave + (wave|newid) +", x0, " * ", x1)), 
  #   data = test_mat,
  #   weights = c("w_obs", "r1wtrespb"),
  #   cWeights = TRUE)
  # test_mat$Y_test <- predict(test, 
  #                   newdata = test_mat, type = "response")
  # summary(test)
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
  # lmer_test <- lme4::lmer(
  #   formula = Y_doX0_doX1_round ~ wave + (1 + wave |newid) + wave * doX0_ * doX1_,
  #   data = data_boot_long %>% rename_at(vars(doX0, doX1), function(x) paste0(x, "_")),
  #   control = lme4::lmerControl(optimizer = "Nelder_Mead"))
  Sys.time()
  tictoc::tic()
  MSM_wtd_lmm <- WeMix::mix(
    Y_doX0_doX1_round ~ wave + (1 + wave |newid) + doX0 * doX1 * wave,
    data = data_boot_long,
    weights = c("w_obs", "r1wtrespb"),
    cWeights = TRUE)
  tictoc::toc()
  Sys.time()
  # summary(MSM_wtd_lmm)
  
  coef <- summary(lmer_test)$coefficients %>% t() %>% as.data.frame() %>%
    mutate(est = rownames(.)) %>% filter(est == "Estimate")
  
  slope_tib <- coef %>%
    mutate(
      slope_11_00 = doX0_1 + doX1_1 + `doX0_1:doX1_1` + `wave:doX0_1` + `wave:doX1_1` + `wave:doX0_1:doX1_1`,
      slope_21_00 = doX0_2 + doX1_1 + `doX0_2:doX1_1` + `wave:doX0_2` + `wave:doX1_1` + `wave:doX0_2:doX1_1`,
      slope_31_00 = doX0_3 + doX1_1 + `doX0_3:doX1_1` + `wave:doX0_3` + `wave:doX1_1` + `wave:doX0_3:doX1_1`,
      slope_12_00 = doX0_1 + doX1_2 + `doX0_1:doX1_2` + `wave:doX0_1` + `wave:doX1_2` + `wave:doX0_1:doX1_2`,
      slope_22_00 = doX0_2 + doX1_2 + `doX0_2:doX1_2` + `wave:doX0_2` + `wave:doX1_2` + `wave:doX0_2:doX1_2`,
      slope_32_00 = doX0_3 + doX1_2 + `doX0_3:doX1_2` + `wave:doX0_3` + `wave:doX1_2` + `wave:doX0_3:doX1_2`,
      slope_01_00 = doX1_1 + `wave:doX1_1`,
      slope_02_00 = doX1_2 + `wave:doX1_2`,
      slope_11_10 = doX1_1 + `doX0_1:doX1_1` + `wave:doX1_1` + `wave:doX0_1:doX1_1`,
      slope_21_20 = doX1_1 + `doX0_2:doX1_1` + `wave:doX1_1` + `wave:doX0_2:doX1_1`,
      slope_31_30 = doX1_1 + `doX0_3:doX1_1` + `wave:doX1_1` + `wave:doX0_3:doX1_1`,
      slope_12_10 = doX1_2 + `doX0_1:doX1_2` + `wave:doX1_2` + `wave:doX0_1:doX1_2`,
      slope_22_20 = doX1_2 + `doX0_2:doX1_2` + `wave:doX1_2` + `wave:doX0_2:doX1_2`,
      slope_32_30 = doX1_2 + `doX0_3:doX1_2` + `wave:doX1_2` + `wave:doX0_3:doX1_2`,
      slope_10_00 = doX0_1 + `wave:doX0_1`,
      slope_20_00 = doX0_2 + `wave:doX0_2`,
      slope_30_00 = doX0_3 + `wave:doX0_3`) %>%
    select(contains("slope"))
  
  return(g_comp_results <- data.frame(coef(MSM_wtd_lmm)))
}


estimate_df <- function(dataframe){
  estimate <- function(x){
    stats <- c(length(x), 
               mean(x,na.rm = T), 
               median(x,na.rm = T),
               sd(x,na.rm = T),
               min(x,na.rm = T),
               max(x,na.rm = T),
               # sum(x,na.rm = T), sum(is.na(x)),
               quantile(x,c(0.025, 0.975),na.rm = T)
    )
    names(stats) <- c("N", "Mean", "Median",
                      "Sd", "Min", "Max",
                      # "Sum", "Missing"
                      "p2.5th", "p97.5th"
    )
    return(round(stats, 4))
  }
  
  stats_tib <- dataframe %>%
    select(where(is.numeric)) %>%
    purrr::map_dfr(estimate) %>%
    as_tibble() %>%
    mutate("Effect" = colnames(dataframe)[-1]) %>%
    select(Effect, everything())
  
  return(stats_tib)
}

mediation_func_boot <- function(exposure, mediator, outcome){
  if (exists("pb")){
    pb$tick()$print()
  }
  map_dfr(1:bootsize, ~ mediation_func(exposure, mediator, outcome),
          .id = "replication") %>% estimate_df() %>%
    mutate(Exposure = exposure,
           Mediator = mediator,
           Outcome = outcome)
}

#---- OLD ----
# # Differentiate by doX's value (proportional, 0 and 1)
#   for (c1_var in c1){
#     var_type <- case_when(c1_var %in% c1_continuous ~ "response",
#                           c1_var %in% c1_binary ~ "response",
#                           c1_var %in% c1_multinomial ~ "class")
#     if (c1_var %in% c1_binary){
#       for (doX0_var in c("doX0", "doX0_0", "doX0_1")){
#         data_boot %<>%
#           mutate(
#             !!paste0(c1_var, "_", doX0_var) := 
#               rbinom(n, size = 1, prob = 
#                        predict(get(paste0("pc1_model_", c1_var)),
#                                newdata = data_boot %>% select(-all_of(x0)) %>%
#                                  dplyr::rename(!!sym(x0) := !!sym(doX0_var)),
#                                type = var_type)))
#       }
#     } else {
#       for (doX0_var in c("doX0", "doX0_0", "doX0_1")){
#         data_boot %<>%
#           mutate(!!paste0(c1_var, "_", doX0_var) :=
#                    predict(get(paste0("pc1_model_", c1_var)),
#                            newdata = data_boot %>% select(-all_of(x0)) %>%
#                              dplyr::rename(!!sym(x0) := !!sym(doX0_var)),
#                            type = var_type))
#       }
#     }
#   }

