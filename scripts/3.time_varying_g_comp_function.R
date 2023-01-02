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
  px0_model <- glm(x0 ~ 1, data = data_boot, family = binomial("identity"))
  px1_model <- glm(x1 ~ 1, data = data_boot, family = binomial("identity"))
  
  #---- Step 2: Fit a model for C1 given X0 and C0 (lambdas)----
  for (c1_var in c1){
    c1_model_form <- paste0(c1_var, " ~ ", x0, " + ", 
                            glue_collapse(c0, sep = "+"))
    model <- multinom(as.formula(c1_model_form), data = data_boot)
    assign(paste0("pc1_model_", c1_var), model)
  }
  
  #---- Step 3: Fit a model for Y given X0, X1, C0 and C1 (Q model, betas) ----
  pYx0x1c0c1_model <- glm(formula(
    glue("y ~ x0 + x1 + ", 
         glue_collapse(c0, sep = "+"), "+",
         glue_collapse(c1, sep = "+"))), 
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
                      predict(px1_model, data_boot, type = "response"))
      dox1_1 = 1,
      dox1_0 = 0,
      
      # Simulate C1
      # pending
      
      m_dox0 = rbinom(n, size = 1, prob =
                        predict(pMxc_model, newdata = data_boot %>%
                                  mutate(x = dox0), type = "response")),
      # Potential M for TDE or PAI
      m_dox1 = rbinom(n, size = 1, prob =
                        predict(pMxc_model, newdata = data_boot %>%
                                  mutate(x = dox1), type = "response")),
      # Potential M for TE, PIE/TIE
      m_doX = rbinom(n, size = 1, prob =
                       predict(pMxc_model, newdata = data_boot %>%
                                 mutate(x = doX), type = "response")),
      # Potential M for CDE ref
      dom0 = 0,
      # Potential M for CDE ind
      dom1 = 1,
      # Potential M for stochastic CDE
      # randomly assign the mediator with its observed prevalence 
      # but break the link between the mediator and its determinants
      doM =rbinom(n, size = 1, prob =
                    predict(pm_model, data_boot, type = "response")),
      # Potential outcomes
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
  
  #---- Step 3: MSM for direct and indirect effects components ----
  pde_model <- glm(Y_doX_mdox0_PDE ~ doX, data = data_boot, 
                   family = gaussian(link = "identity"))
  tie_model <- glm(Y_dox1_mdoX_TIE ~ doX, data = data_boot, 
                   family = gaussian(link = "identity"))
  tde_model <- glm(Y_doX_mdox1_TDE ~ doX, data = data_boot, 
                   family = gaussian(link = "identity"))
  pie_model <- glm(Y_dox0_mdoX_PIE ~ doX, data = data_boot, 
                   family = gaussian(link = "identity"))
  cderef_model <- glm(Y_doX_dom0_CDEref ~ doX, data = data_boot, 
                      family = gaussian(link = "identity"))
  cdeind_model <- glm(Y_doX_dom1_CDEind ~ doX, data = data_boot, 
                      family = gaussian(link = "identity"))
  cdesto_model <- glm(Y_doX_doM_CDEsto ~ doX, data = data_boot, 
                      family = gaussian(link = "identity"))
  
  return(
    mediat_results <- tibble(
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
}


mediation_func <- function(x, m, y){
  dataset <- salsa_imp_final
  covar_num <- c("log_income_ind", "BlAge", "edu_yr",
                 paste0("bl_", substring(outcome, 5)))
  covar_cat <- c("female", "occupation_cat", 
                 "bl_marital_status", "adl_cat", "iadl_cat", 
                 "father_occupation_cat", "mother_occupation_cat",
                 "father_edu_cat", "mother_edu_cat", 
                 "food_deprivation_cat")
  data_boot <- 
    dataset[sample(nrow(dataset), nrow(dataset), replace = TRUE), ] %>%
    filter(!is.na(x))
  n = nrow(data_boot)
  if (length(unique(data_boot[, paste0(x)])) == 2){
    #---- Step 1: Fit a parametric model (correctly specified)----
    # and obtain parameters of the variable distributions, using the original sample
    #The simultaneous additive risk equations are given by
    
    #Estimating marginal expectation of X
    px_model <- glm(formula(glue("{x} ~ 1")), data = data_boot, 
                    family = binomial("identity"))
    
    # Model for stochastic M
    pm_model <- glm(formula(glue("{m} ~ 1")), data = data_boot, 
                    family = binomial(link = "identity"))
    
    #True mediator regression (betas)
    pMxc_model <- glm(formula(
      glue("{m} ~ {x} + ", 
           glue_collapse(glue("factor({covar_cat})"), sep="+"), "+",
           glue_collapse(covar_num, sep = "+"))), 
      data = data_boot, family = binomial(link ="logit"))
    
    #True outcome regression (thetas)
    pYxmc_model <- glm(formula(
      glue("{y} ~ {x} + {m} + {x}*{m} + ", 
           glue_collapse(glue("factor({covar_cat})"), sep="+"), "+",
           glue_collapse(covar_num, sep = "+"))),
      data = data_boot, 
      family = gaussian(link = "identity"))
    
    #---- Step 2: Simulate post intervention DAG distribution----
    data_boot %<>%
      mutate(
        # Intervention X
        doX = rbinom(n, size = 1, prob = 
                       predict(px_model, data_boot, type = "response")),
        dox1 = 1,
        dox0 = 0,
        
        # Simulate M
        # Potential M for PDE or RIE
        m_dox0 = rbinom(n, size = 1, prob =
                          predict(pMxc_model, newdata = data_boot %>%
                                    mutate(x = dox0), type = "response")),
        # Potential M for TDE or PAI
        m_dox1 = rbinom(n, size = 1, prob =
                          predict(pMxc_model, newdata = data_boot %>%
                                    mutate(x = dox1), type = "response")),
        # Potential M for TE, PIE/TIE
        m_doX = rbinom(n, size = 1, prob =
                         predict(pMxc_model, newdata = data_boot %>%
                                   mutate(x = doX), type = "response")),
        # Potential M for CDE ref
        dom0 = 0,
        # Potential M for CDE ind
        dom1 = 1,
        # Potential M for stochastic CDE
        # randomly assign the mediator with its observed prevalence 
        # but break the link between the mediator and its determinants
        doM =rbinom(n, size = 1, prob =
                      predict(pm_model, data_boot, type = "response")),
        # Potential outcomes
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
    
    #---- Step 3: MSM for direct and indirect effects components ----
    pde_model <- glm(Y_doX_mdox0_PDE ~ doX, data = data_boot, 
                     family = gaussian(link = "identity"))
    tie_model <- glm(Y_dox1_mdoX_TIE ~ doX, data = data_boot, 
                     family = gaussian(link = "identity"))
    tde_model <- glm(Y_doX_mdox1_TDE ~ doX, data = data_boot, 
                     family = gaussian(link = "identity"))
    pie_model <- glm(Y_dox0_mdoX_PIE ~ doX, data = data_boot, 
                     family = gaussian(link = "identity"))
    cderef_model <- glm(Y_doX_dom0_CDEref ~ doX, data = data_boot, 
                        family = gaussian(link = "identity"))
    cdeind_model <- glm(Y_doX_dom1_CDEind ~ doX, data = data_boot, 
                        family = gaussian(link = "identity"))
    cdesto_model <- glm(Y_doX_doM_CDEsto ~ doX, data = data_boot, 
                        family = gaussian(link = "identity"))
    
    return(
      mediat_results <- tibble(
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