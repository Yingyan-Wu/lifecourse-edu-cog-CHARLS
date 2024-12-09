# Functions which can be useful during the dataset construction process
# Created by Yingyan Wu
# May.8.2022
# Some of the functions are from Dr. Roch Nianogo's EPI 200C course scripts

#---- Required packages ----
require("ggpubr")

#---- proc sum descriptive ----
# modified from Dr. Nianogo's code
proc_summ <- function(variable, more = F){
  if (more){
    var_naomit <- na.omit(variable) # remove missing values
    summ <- tibble(
      N = length(var_naomit), 
      mean = mean(var_naomit), 
      sd = sd(var_naomit), 
      min = min(var_naomit), 
      P1 = quantile(var_naomit, probs = 0.01),
      P10 = quantile(var_naomit, probs = 0.1), 
      P25 = quantile(var_naomit, probs = 0.25), 
      median = median(var_naomit), 
      P75 = quantile(var_naomit, probs = 0.75), 
      P90 = quantile(var_naomit, probs = 0.90), 
      P99 = quantile(var_naomit, probs = 0.99),
      max = max(var_naomit),
      N_missing = sum(is.na(variable)))
  } else {
    var_naomit <- na.omit(variable) # remove missing values
    summ <- tibble(
      N = length(var_naomit), 
      mean = mean(var_naomit), 
      sd = sd(var_naomit), 
      min = min(var_naomit), 
      P25 = quantile(var_naomit, probs = 0.25), 
      median = median(var_naomit), 
      P75 = quantile(var_naomit, probs = 0.75),
      max = max(var_naomit),
      N_missing = sum(is.na(variable)))
  }
  return(summ)
}

#---- proc freq descriptive ----
# Only for one single variable's prop table
proc_freq <- function(dataset, variable){
  dataset %>%
    dplyr::count({{variable}}) %>%
    mutate("prop(%)" = prop.table(n)*100,
           "cumulative prop(%)" = cumsum(n)*100/sum(n))
}

proc_freq_wt <- function(dataset, variable, weight){
  dataset %>% 
    dplyr::count({{variable}}, wt = {{weight}}) %>% 
    mutate("weighted prop(%)" = prop.table(n) * 100,
           "cumulative prop(%)" = cumsum(n)*100/sum(n))
}

#---- proc univariate ----
proc_univariate <- function(variable){
  output_list <- vector(mode = "list")
  var_naomit <- na.omit(variable) # remove missing values
  
  output_list$Stats <- tibble(
    N = length(var_naomit),
    Mean = mean(var_naomit),
    Std = sd(var_naomit),
    Min = min(var_naomit),
    P10 = unname(quantile(var_naomit, probs = 0.1)),
    P25 = unname(quantile(var_naomit, probs = 0.25)),
    Median = median(var_naomit),
    P75 = unname(quantile(var_naomit, probs = 0.75)),
    P90 = unname(quantile(var_naomit, probs = 0.90)),
    Max = max(var_naomit),
    N_missing = sum(is.na(variable))) %>%
    as.data.frame()
  
  output_list$Quantiles <- tibble(
    perc = c(0, 1, 5, 10, 25, 50, 75, 90, 95, 99, 100),
    Quantile = quantile(var_naomit, perc/100),
    Level = case_when(perc == 0 ~ paste0(perc, "% Min"),
                      perc == 50 ~ paste0(perc, "% Median"),
                      perc == 100 ~ paste0(perc, "% Max"),
                      TRUE ~ paste0(perc, "%"))) %>%
    select(Level, Quantile) %>%
    as.data.frame()
  
  output_list$`Extreme Observations` <- tibble(
    Lowest = head(sort(var_naomit), 5),
    Highest = tail(sort(var_naomit), 5)) %>%
    as.data.frame()
  
  # Pending on passing the variable name to the axis label
  hist <- ggplot() +
    geom_histogram(aes(x = var_naomit)) +
    labs(x = "Variable") +
    coord_flip() +
    theme_minimal()
  
  boxplot <- ggplot() +
    geom_boxplot(aes(x = var_naomit)) + labs(x = "Variable") +
    coord_flip() + theme_minimal()
  
  output_list$`Distribution Plot` <-
    ggpubr::ggarrange(hist, boxplot,
                      ncol = 2, nrow = 1, align = c("hv"), widths = c(2, 1))
  # 
  # output_list$hist <- hist
  # output_list$boxplot <- boxplot
  
  return(output_list)
}

#---- impute ages -----
# adpated from Crystal Shaw's code
# https://github.com/Mayeda-Research-Group/exposure-trajectories/blob/master/RScripts/functions/impute_ages.R

impute_ages <- function(vector, survey_years){
  # For complete vectors or vectors missing every value, do nothing
  if(sum(is.na(vector)) == 0 | sum(is.na(vector)) == length(vector)){
    return(vector)
  } else{
    gap <- diff(survey_years)
    nonmissing_age <- which(!is.na(vector))
    # Since wave 5 is not harmonized, use use previous waves if nonmissing
    # If all missing but wave 5, use wave 5
    if (max(nonmissing_age) == 5){
      nonmissing_age_up <- nonmissing_age[nonmissing_age != 5]
      last_age <- ifelse(length(nonmissing_age_up) == 0, 5,
                         max(nonmissing_age_up))
    } else{
      last_age <- max(nonmissing_age)
    }
    
    missing_age <- which(is.na(vector))
    
    for (slot in missing_age){
      if(slot < last_age){
        vector[slot] <- vector[last_age] - sum(gap[slot:(last_age - 1)])
      } else {
        vector[slot] <- vector[last_age] + sum(gap[last_age:(slot - 1)])
      }
    }
    
    # for(slot in missing_age){
    #   if(slot < last_age){
    #     vector[slot] <- vector[last_age] - 2*(last_age - slot)
    #   } else{
    #     vector[slot] <- vector[last_age] + 2*(slot - last_age)
    #   }
    # }
    return(vector)
  }
}

# # Test
# # 2011, 2013, 2015, 2018, 2020
# survey_yrs <- c(2011, 2013, 2015, 2018, 2020)
# vector <- c(NA, 75, 77, 80, 82)
# impute_ages(vector, survey_yrs)
# vector <- c(75, 77, 79, 82, NA)
# impute_ages(vector, survey_yrs)
# vector <- c(75, NA, NA, 82, 84)
# impute_ages(vector, survey_yrs)
# vector <- c(NA, NA, 43, NA, 50)
# impute_ages(vector, survey_yrs)
