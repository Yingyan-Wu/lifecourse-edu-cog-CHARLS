# Time-varying g comp function
# Created by: Yingyan Wu

require(nnet) # package for multinomial logistic regression 

#---- function ----
# x0, x1 needs to be binary (if categorical, dummy variables need to be created)
joint_effect_g_comp <- function(dataset, x0, x1, c0, c1, y, bootstrap = TRUE){
  if (bootstrap == T){
    data_boot <- 
      dataset[sample(nrow(dataset), nrow(dataset), replace = TRUE), ]
  } else {
    data_boot <- dataset
  }
  
  n = nrow(data_boot)
  #---- Step 1: Obtain f(x0), f(x1) from the observed distribution ----
  
  #Estimating marginal expectation of X0 and X1
  px0_model <- glm(as.formula(paste0(x0, "~ 1")), 
                   data = data_boot, family = binomial("identity"))
  px1_model <- glm(as.formula(paste0(x1, "~ 1")),  
                   data = data_boot, family = binomial("identity"))
  
  #---- Step 2: Fit a model for C1 given X0 and C0 (lambdas)----
  for (c1_var in c1){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            paste(c0, collapse = "+"))
    model <- nnet::multinom(as.formula(c1_model_form), data = data_boot)
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  #---- Step 3: Fit a model for Y given X0, X1, C0 and C1 (Q model, betas) ----
  pYx0x1c0c1_model <- glm(
    as.formula(paste0(y, " ~ ", x0, " * ", x1, " + ",
                      paste(c0, collapse = "+"), "+",
                      paste(c1, collapse = "+"))), 
    data = data_boot, family = gaussian(link = "identity"))
  
  #---- Step 4: Simulate post intervention DAG distribution----
  data_boot %<>%
    mutate(
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