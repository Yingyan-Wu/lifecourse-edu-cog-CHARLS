# Bootstrap for main analysis and sensitivity analysis -- cluster
# Created by: Yingyan Wu
# Date: Nov.3.2024

#---- Package loading + option specifications ----
library(tidyverse)
library(magrittr)
library(survey)
library(geepack)
library(splines)
library(doParallel)
# No scientific notation
options(scipen = 999)
# Needed if plyr was loaded
# select <- dplyr::select

#---- Load the data & function ----
# # Paths (local)
load(here::here("data", "analysis_data", "imp_pmm_20_wide_long.RData"))
source(here::here("scripts", "3.0.X_GEE_time_varying_g_comp_function_cat_exp.R"))

#---- Variables ----
x0 = "childhood_edu"
x1 = "continuing_edu_2cat"
y = "imrc"
svy_wt <- "n1_wtrespb_rscl"
c0 <- c(
  "n1_agey_v3", # Follow up time as time scale
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

ipcw_c <- c("n1_agey_v3", "n1_iadl_2cat", "n1_cesd10_cleaned", 
            "n1_hibpe", "n1_stroke", "n1_hearte", "n1_cancre", "n1_diabe",
            paste0("n1_", y))
w_c1s <- setNames(rep(list(numeric()), length(c1)), paste0("w_", c1))
# list2env(w_c1s, envir = .GlobalEnv)

# Warnings for IPCW models
w_pcnum <- w_pcden <- numeric()
#  Warnings for q model and MSM
w_qmodel <- w_msm <- numeric()
w_qmodel_msg <- w_msm_msg <- vector()

#---- Parallel computing main analysis ----
M = 10
i_boot = 0
reqpackage <- c("tidyverse", "magrittr", "survey", "geepack", "splines") 
# For some reason, dopar need to specify these packages to be required to run.
num_imp = 1
scenario_num = 1
tictoc::tic()
result <- 
  joint_effect_g_comp(imp_pmm_stacked, imp_pmm_long,
                      bootstrap = F)
tictoc::toc()

predMSM_long <- result %>%
  select(n_imp, i_boot, starts_with("mean_predMSM")) %>%
  pivot_longer(cols = c(-n_imp, -i_boot),
               names_to = c("PE", "intervention", "fu_time_yrs"),
               names_pattern = "mean_predMSM_(.*)_(.*)_yr(.)",
               values_to = "rc") %>%
  mutate(fu_time_yrs = as.numeric(fu_time_yrs),
         label = case_when(
           intervention == "00" ~ "No formal education at all",
           intervention == "10" ~ "Elementary school or less & No adulthood edu",
           intervention == "20" ~ "Middle school level & No adulthood edu",
           intervention == "30" ~ "College and above & No adulthood edu",
           intervention == "01" ~ "Only adulthood edu",
           intervention == "11" ~ "Elementary school or less & Adulthood edu",
           intervention == "21" ~ "Middle school level & Adulthood edu",
           intervention == "31" ~ "College and above & Adulthood edu"),
         label_adult = case_when(
           intervention %in% c("00", "10", "20", "30") ~ "No adulthood edu",
           intervention %in% c("01", "11", "21", "31") ~ "Adulthood edu"),
         comp_childhood_only = case_when(intervention %in% c(00, 10, 20, 30) ~ 1,
                                         TRUE ~ 0),
         comp_adult_only = case_when(intervention %in% c("00", "01") ~ 1,
                                     TRUE ~ 0),
         comp_adult_childhood = case_when(intervention %in% c("00", "01") ~ "No early life formal education",
                                          intervention %in% c(10, 11) ~ "Elementary school or less",
                                          intervention %in% c(20, 21) ~ "Middle school level",
                                          intervention %in% c(30, 31) ~ "College and above"),
         intervention = factor(intervention, 
                               levels = c("00", "01", "10", "11", "20", "21", "30", "31"))) %>%
  arrange(fu_time_yrs, intervention)

predMSM_long %<>%
  mutate(label = factor(label, levels = unique(predMSM_long$label)),
         label_adult = factor(label_adult, levels = unique(predMSM_long$label_adult)),
         comp_adult_childhood = factor(
           comp_adult_childhood, levels = unique(predMSM_long$comp_adult_childhood)))

color_palette <- c("#E9C4A6", "#99806c", "#54B663", "#3B7F46",
                   "#5698c3", "#3271ae", "#9B4BDD", "#65318E")
predMSM_long %>%
  filter(PE == "noPE") %>%
  ggplot(aes(x = fu_time_yrs, y = rc, group = label)) +
  geom_line(aes(color = label)) + 
  facet_grid(cols = vars(comp_adult_childhood)) +
  scale_color_manual(name = NULL, values = color_palette) +
  scale_fill_manual(name = NULL, values = color_palette) +
  # labels = label_adult) +
  scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
  scale_y_continuous(breaks = 1:6) +
  labs(x = "Follow-up time (years)", y = "Word recall scores") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, color = "black"), 
        strip.text.x = element_text(size = 10, color = "black"),
        strip.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, color = "black"))
