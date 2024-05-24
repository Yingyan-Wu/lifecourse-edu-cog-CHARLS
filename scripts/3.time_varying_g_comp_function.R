# Time-varying g comp function
# Created by: Yingyan Wu

require(nnet) # package for multinomial logistic regression 

imp_pmm_40 <- readRDS(here::here("data", "analysis_data", "imp_pmm_40_stacked.RDS"))
imp_pmm_40 %<>%
  dplyr::mutate(occupation_30_5cat = 
                  as.factor(case_when(occupation_30 %in% c(1, 2) ~ 1,
                                      occupation_30 %in% c(3, 4, 5) ~ 2,
                                      occupation_30 == 6 ~ 3,
                                      occupation_30 == 7 ~ 4,
                                      occupation_30 == 8 ~ 5)),
                childhood_edu_10 = case_when(childhood_edu == 1 ~ 1,
                                             childhood_edu == 0 ~ 0),
                childhood_edu_20 = case_when(childhood_edu == 2 ~ 1,
                                             childhood_edu == 0 ~ 0),
                childhood_edu_30 = case_when(childhood_edu == 3 ~ 1,
                                             childhood_edu == 0 ~ 0),
                continuing_edu_10 = case_when(continuing_edu == 1 ~ 1,
                                              continuing_edu == 0 ~ 0),
                continuing_edu_20 = case_when(continuing_edu == 2 ~ 1,
                                              continuing_edu == 0 ~ 0))
c0 = c("r1agey_v3", "female_v2", "first_child_v2", 
       "rameduc_c", "rafeduc_c", "ramoccup_c", "rafoccup_c",
       "childhood_1st_region", "rarural_child", 
       "first_hukou")
c1_continuous <- c("chldstate_30")
c1_binary <- c("marital_30", "rural_30")
c1_mutinomial <- c("occupation_30_5cat", "hukou_30")
c1 <- c(c1_continuous, c1_binary, c1_mutinomial)
num_imp = 1

test_dataset <- imp_pmm_40 %>% filter(imp == num_imp) %>%
  select(ID, contains("childhood_edu_"), contains("continuing_edu_"),
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

rctib_long <- test_dataset %>%
  select(ID, contains(c("imrc", "dlrc"))) %>%
  pivot_longer(cols = !ID, 
               names_to = c("wave", ".value"), 
               names_pattern = "r(.)(.*)") 

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



#---- function ----
# x0, x1 needs to be binary (if categorical, dummy variables need to be created)
# Dataset were filtered to contain only one imputation before feed into the function
joint_effect_g_comp <- function(dataset, x0, x1, c0, c1, y, bootstrap = TRUE){
  dataset <- test_dataset
  dataset_long <- test_long
  x0 = "childhood_edu_10"
  x1 = "continuing_edu_10"
  y = "imrc"
  bootstrap = T
  
  if (bootstrap == T){
    data_boot <- 
      dataset[sample(nrow(dataset), nrow(dataset), replace = TRUE), ]
  } else {
    data_boot <- dataset
  }
  
  # Filter to index and comparison groups only (nonmissing binary x0 and x1)
  data_boot %<>%
    filter(!is.na(!!sym(x0)) & !is.na(!!sym(x1))) %>%
    mutate(newid = 1:n())
  
  data_boot_long <- data_boot %>% select(ID, newid) %>%
    left_join(dataset_long, by = "ID", relationship = "many-to-many") %>%
    filter(!is.na(!!sym(y)))
  
  #---- Step 1: Obtain f(x0), f(x1) from the observed distribution ----
  #Estimating marginal expectation of X0 and X1
  px0_model <- glm(as.formula(paste0(x0, "~ 1")), 
                   data = data_boot, weights = r1wtrespb, 
                   family = quasibinomial("identity"))
  px1_model <- glm(as.formula(paste0(x1, "~ 1")),  
                   data = data_boot, 
                   weights = r1wtrespb, 
                   family = quasibinomial("identity"))
  
  #---- Step 2: Fit a model for C1 given X0 and C0 (lambdas)----
  for (c1_var in c1_mutinomial){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    model <- nnet::multinom(as.formula(c1_model_form), data = data_boot,
                            weights = r1wtrespb)
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  for (c1_var in c1_binary){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    model <- glm(as.formula(c1_model_form), data = data_boot, weights = r1wtrespb,
                 family = binomial,  alpha = 1, # alpha = 1 lasso, 0 ridge
                 lambda = NULL)
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  for (c1_var in c1_continuous){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    model <- lm(as.formula(c1_model_form), data = data_boot,
                weights = r1wtrespb)
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  #---- Step 3: Fit a model for Y given X0, X1, C0 and C1 (Q model, betas) ----
  data_boot_long %<>% mutate(w_obs = 1)
  pYx0x1c0c1_model <- WeMix::mix(
    as.formula(paste0(y, " ~ wave + (1 |newid) +", x0, " * ", x1, " + ",
                      paste(c0, collapse = "+"), "+",
                      paste(c1, collapse = "+"))), 
    data = data_boot_long,
    weights = c("w_obs", "r1wtrespb"),
    cWeights = TRUE)
  
  #---- Step 4: Simulate post intervention DAG distribution----
  data_boot %<>%
    dplyr::mutate(
      # Intervention X
      doX0 = rbinom(n, size = 1, prob = 
                      predict(px0_model, data_boot, type = "response")),
      dox0_1 = 1,
      dox0_0 = 0,
      doX1 = rbinom(n, size = 1, prob = 
                      predict(px1_model, data_boot, type = "response")),
      dox1_1 = 1,
      dox1_0 = 0)
  
  # Simulate C1
  for (c1_var in c1){
    data_boot %<>%
      mutate(!!paste0(c1_var, "_doX0") :=
               predict(get(paste0("pc1_model_", c1_var)),
                       newdata = data_boot %>% select(-all_of(x0)) %>%
                         dplyr::rename(!!sym(x0) := doX0),
                       type = "class"),
             !!paste0(c1_var, "_dox0_0_doX1") :=
               predict(get(paste0("pc1_model_", c1_var)),
                       newdata = data_boot %>% 
                         select(-all_of(x0), -all_of(x1)) %>%
                         dplyr::rename(!!sym(x0) := dox0_0,
                                       !!sym(x1) := doX1),
                       type = "class"),
             !!paste0(c1_var, "_dox0_1_doX1") :=
               predict(get(paste0("pc1_model_", c1_var)),
                       newdata = data_boot %>% 
                         select(-all_of(x0), -all_of(x1)) %>%
                         dplyr::rename(!!sym(x0) := dox0_1,
                                       !!sym(x1) := doX1),
                       type = "class"))
  }
  
  # Simulate Potential outcomes
  data_boot %<>% 
    mutate(Y_doX0 = predict(pYx0x1c0c1_model, 
                            newdata = data_boot %>%
                              select(-all_of(x0), -all_of(c1)) %>%
                              dplyr::rename(!!sym(x0) := doX0,
                                            !!sym(c1[[1]])),
                            type = "response"))
  # Pending
  Y_doX_mdox0_PDE = predict(pYxmc_model, newdata = data_boot %>%
                              mutate(x = doX,
                                     m = m_doX), type = "response"),
  Y_dox1_mdoX_TIE = predict(pYxmc_model, newdata = data_boot %>%
                              mutate(x = dox1,
                                     m = m_doX), type = "response"),
  Y_doX_mdox1_TDE = predict(pYxmc_model, newdata = data_boot %>%
                              mutate(x = doX,
                                     m = m_dox1), type = "response"),
  Y_dox0_mdoX_PIE = predict(pYxmc_model, newdata = data_boot %>%
                              mutate(x = dox0,
                                     m = m_doX), type = "response"),
  # Fix M at 0
  Y_doX_dom0_CDEref = predict(pYxmc_model, newdata = data_boot %>%
                                mutate(x = doX,
                                       m = dom0), type = "response"),
  # Fix M at 1
  Y_doX_dom1_CDEind = predict(pYxmc_model, newdata = data_boot %>%
                                mutate(x = doX,
                                       m = dom1), type = "response"),
  # Stochastic CDE
  Y_doX_doM_CDEsto = predict(pYxmc_model, newdata = data_boot %>%
                               mutate(x = doX,
                                      m = doM), type = "response")
  )

#---- Step 5: MSM for potential outcomes ~ doXs ----
# Early life eductaion v.s. No early life education
Y_X0_model <- glm(Y_doX0 ~ doX0, data = data_boot,
                  family = gaussian(link = "identity"))
# continuing education|early life education v.s. 
# no continuing education|early life education
Y_x00_X1_model <- glm(Y...)

return(
  g_comp_results <- tibble(
    PDE = as.numeric(pde_model$coefficients[2]),
    TIE = as.numeric(tie_model$coefficients[2]),
    TDE = as.numeric(tde_model$coefficients[2]),
    PIE = as.numeric(pie_model$coefficients[2]),
    CDEref = as.numeric(cderef_model$coefficients[2]),
    CDEind = as.numeric(cdeind_model$coefficients[2]),
    CDEsto = as.numeric(cdesto_model$coefficients[2])
  )
)
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