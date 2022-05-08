# Functions which can be useful during the dataset construction process
# Created by Yingyan Wu
# May.8.2022
# Some of the functions are from Dr. Roch Nianogo's EPI 200C course scripts

#---- proc sum descriptive ----
# modified from Dr. Nianogo's code
proc_sum <- function(variable, more = F){
  if (more){
    var_naomit <- na.omit(variable) # remove missing values
    summ <- data.frame() # create vector to save info
    summ[1,1] <- length(var_naomit) # N
    summ[1,2] <- mean(var_naomit) # mean
    summ[1,3] <- sd(var_naomit) # standard deviation
    summ[1,4] <- min(var_naomit) # minimum
    summ[1,5] <- unname(quantile(var_naomit, probs = 0.1)) #P10
    summ[1,6] <- unname(quantile(var_naomit, probs = 0.25)) #P25
    summ[1,7] <- median(var_naomit) #median
    summ[1,8] <- unname(quantile(var_naomit, probs = 0.75)) #P75
    summ[1,9] <- unname(quantile(var_naomit, probs = 0.90)) #P90
    summ[1,10] <- max(var_naomit) # maximum
    summ[1,11] <- sum(is.na(variable)) # number of missing values
    names(summ) <- c("N", "Mean", "Std", 
                     "Min", "P10", "P25", "Median", "P75", "P90", "Max", 
                     "N_missing")
  } else{
    var_naomit <- na.omit(variable) # remove missing values
    summ <- data.frame() # create vector to save info
    summ[1,1] <- length(var_naomit) # N
    summ[1,2] <- mean(var_naomit) # mean
    summ[1,3] <- median(var_naomit) #median
    summ[1,4] <- sd(var_naomit) # standard deviation
    summ[1,5] <- min(var_naomit) # minimum
    summ[1,6] <- max(var_naomit) # maximum
    summ[1,7] <- sum(is.na(variable)) # number of missing values
    names(summ) <- c("N", "Mean", "Median", "Std", 
                     "Min", "Max", "N_missing") 
  }
  return(round(summ, 4))
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