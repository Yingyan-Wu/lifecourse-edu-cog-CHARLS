#  for prediction continuous education
# Created by: Yingyan Wu
# Apr.11.2024

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "plyr", "tidyverse", "magrittr","haven", "survey", "glue",
       "labelled", "lubridate")

#---- Load the data ----
load(here::here("data", "analysis_data", "charls_selected_pred_cont.RData"))

#---- Check the age ----
charls_selected %>%
  filter(r5agey_v3 < 45) %>%
  nrow() # 155

charls_selected %>%
  dplyr::filter(r5agey_v3 < 45) %>%
  dplyr::select(contains("agey_v3")) %>% view()

#---- Continuing education stratified by region ----
with(charls_selected, table(continuing_edu, region_eng, useNA = "ifany"))

