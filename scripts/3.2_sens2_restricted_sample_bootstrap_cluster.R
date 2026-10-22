# Time-varying g comp (GEE) -- cluster script
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
library(geepack)
library(splines)
# No scientific notation
options(scipen = 999)
# Needed if plyr was loaded
# select <- dplyr::select

#---- Load the data ----
# Paths (local)
# load(here::here("data", "analysis_data", "imp_pmm_20_wide_long_2011.RData"))
# source(here::here("scripts", "3.0_GEE_time_varying_g_comp_function_cat_exp.R"))

# # Paths cluster
project_folder <- paste0("/u/home/y/yingyanw/edu_cog_CHARLS/")
load(paste0(project_folder, "data/imp_pmm_20_wide_long_2011.RData"))
source(paste0(project_folder, "scripts/3.0_GEE_time_varying_g_comp_function_cat_exp.R"))

#---- Scenario tibble ----
n_imp = 20
n_bootstrap_per_job = 500
n_bootstrap = 1000

scenario_tib <- expand_grid(
  imp = 1:n_imp,
  boot_i = c(paste0(seq(1, n_bootstrap - n_bootstrap_per_job + 1, 
                        by = n_bootstrap_per_job), "-",
                    seq(n_bootstrap_per_job, n_bootstrap, 
                        by = n_bootstrap_per_job)))) %>%
  separate(boot_i, into = c("boot_i_start", "boot_i_end"), sep = "-") %>%
  mutate_at(vars(boot_i_start), ~ifelse(boot_i_start == 1, 0, .))

scenario_tib <- rbind(scenario_tib %>% mutate(y = "imrc"),
                      scenario_tib %>% mutate(y = "dlrc"))

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
y <- scenario$y

print(boot_i_start)
print(boot_i_end)
print(num_imp)
print(y)

#---- Variables ----
x0 = "childhood_edu"
x1 = "continuing_edu_2cat"
print(y)
svy_wt = "n1_wtrespb" # Sensitivity analysis for restrcting to 2011 people only
c0 <- c(
  "n1_agey_v3", # Follow up time as time scale
  "female", "first_child_v2", 
  "rameduc_c", "rafeduc_c", "ramoccup_c", "rafoccup_c",
  "childhood_1st_region", "rarural_child", 
  "first_hukou_2cat")
c1_continuous <- c("chldstate_30")
c1_binary <- c("marital_30", "rural_30", "hukou_30_2cat")
c1_multinomial <- c("occupation_30_3cat")
c1 <- c(c1_continuous, c1_binary, c1_multinomial)
# , c1_multinomial)

ipcw_c <- c("n1_agey_v3", "n1_iadl_2cat", "n1_cesd10_cleaned", 
            "n1_hibpe", "n1_stroke", "n1_hearte", "n1_cancre", "n1_diabe",
            paste0("n1_", y))

# Warning number capture vectors (start with 0)
# Warnings for C1 models
w_c1s <- setNames(rep(list(numeric()), length(c1)), paste0("w_", c1))
# list2env(w_c1s, envir = .GlobalEnv)

# Warnings for IPCW models
w_pcnum <- w_pcden <- numeric()
#  Warnings for q model and MSM
w_qmodel <- w_msm <- numeric()
w_qmodel_msg <- w_msm_msg <- vector()

#---- bootstrap ----
for (i_boot in boot_i_start:boot_i_end) {
  print(i_boot)
  print(svy_wt)
  # boot_i_start <- 0
  # i_boot <- 1000
  (start <- Sys.time()) # starting time
  result <- 
    joint_effect_g_comp(imp_pmm_2011, imp_pmm_long_2011,
                        bootstrap = i_boot != 0)
  (end <- Sys.time()) # ending time
  # saveRDS(result, paste0(project_folder, "/model_results/",
  #                        paste0("MSM_coef_", y, 
  #                               "_imp_", num_imp,"_boot_", i_boot, ".RDS")))
  
  # save the bootstraps
  write_csv(result, append = (i_boot != boot_i_start), 
            file = paste0(project_folder, "/sens2_model_results/", 
                          "MSM_coef_", y, "_scenario_", scenario_num, ".csv"))
  
  w_boot_i <- tibble(
    i_boot = i_boot,
    imp = num_imp,
    scenario = scenario_num,
    duration = difftime(end, start, units = 'sec'),
    w_qmodel = sum(w_qmodel),
    w_msm = sum(w_msm),
    w_qmodel_msg = paste(w_qmodel_msg,collapse="\n"),
    w_msm_msg = paste(w_msm_msg, collapse="\n")) %>%
    bind_cols(as_tibble(do.call(rbind, lapply(w_c1s, sum)) %>% t()))
  # bind_cols(as_tibble(lapply(mget(paste0("w_", c1)), sum))) # c1 model warnings
  # saveRDS(w_boot_i, paste0(project_folder, "/warnings/",
  #                          paste0("warnings_", y, 
  #                                 "_imp_", num_imp,"_boot_", i_boot, ".RDS")))
  write_csv(w_boot_i, append = (i_boot != boot_i_start), 
            file = paste0(project_folder, "/sens2_warnings/", 
                          "warnings_", y, "_scenario_", scenario_num, ".csv"))
}

