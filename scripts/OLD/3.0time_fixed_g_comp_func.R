# Time-fixed g comp function
# For SER abstract
# Created by: Yingyan Wu

#---- function ----
#---- ** single time function ----
g_comp_x <- function(dataset, x, c, y, bootstrap = TRUE, wt){
  # set.seed(seed)
  
  if (bootstrap == T){
    data_boot <- 
      dataset[sample(nrow(dataset), nrow(dataset), replace = TRUE), ]
  } else {
    data_boot <- dataset
  }
  
  n = nrow(data_boot)
  #---- Step 1: Obtain f(x) from the observed distribution ----
  #Estimating marginal expectation of X0 and X1
  px_model <- glm(as.formula(paste0(x, "~ 1")), 
                  data = data_boot, 
                  weights = data_boot[, wt][[1]],
                  family = binomial("identity"))
  
  #---- Step 2: Fit a model for Y given X and c (Q model, betas) ----
  pYxc_model <- glm(
    as.formula(paste0(y, " ~ ", x, " + ",
                      paste(c, collapse = "+"))), 
    data = data_boot, weights = data_boot[, wt][[1]], 
    family = gaussian(link = "identity"))
  
  #---- Step 3: Simulate post intervention DAG distribution ----
  data_boot %<>%
    mutate(
      # Intervention X
      doX = rbinom(n, size = 1, 
                   prob = predict(px_model, data_boot, type = "response")))
  # Potential outcome
  # For some reason it won't work with mutate
  data_boot$Y_doX <-  predict(pYxc_model, 
                              newdata = data_boot %>% select(-all_of(x)) %>% 
                                dplyr::rename(!!sym(x) := doX),
                              type = "response")
  
  
  #---- Step 4: fit MSM to obtain estimate ---- 
  MSM_model <- glm(Y_doX ~ doX, data = data_boot, 
                   weights = data_boot[, wt][[1]],
                   family = gaussian("identity"))
  
    return(coef(MSM_model)[2])
}


# Test
# data_boot <- charls_selected
# x = "childhood_edu1"
# c = covariates
# y = "r1imrc"
# rm(data_boot)
# rm(x)
# rm(c)
# rm(y)
# rm(px_model)
# rm(pYxc_model)
# rm(MSM_model)

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