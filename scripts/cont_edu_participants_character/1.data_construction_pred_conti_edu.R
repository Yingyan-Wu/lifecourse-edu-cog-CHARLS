# CHARLS_data_construction for prediction continuous education
# Created by: Yingyan Wu
# Mar 12.2024

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "plyr", "tidyverse", "magrittr","haven", "survey", "glue",
       "labelled", "lubridate")

#---- load the data ----
load(here::here("data", "analysis_data", "charls_cleaned.RData"))

#---- Reclassify variables ----
charls %<>%
  mutate(occupation_30_3cat = case_when(occupation_30 %in% c(1, 2) ~ 1,
                                        occupation_30 %in% c(3, 4, 5) ~ 2,
                                        occupation_30 == 6 ~ 3,
                                        occupation_30 == 7 ~ 4,
                                        occupation_30 == 8 ~ 5))

#---- Dropping people ----
##---- 1. Dropping those who still missing age and gender ----
# test <- charls %>%
#   filter(is.na(female_v2) & is.na(r1agey_v3)) %>%
#   select(contains("edu"), contains("imrc"), contains("dlrc"))
# table(colSums(is.na(test)))

# These 5 people did not have the outcome measures and the eductaion variables
charls %>%
  filter(is.na(female_v2) & is.na(r1agey_v3)) %>% nrow()
charls_selected <- charls %>%
  filter(!is.na(female_v2) | !is.na(r1agey_v3))

#---- Select variables ----
charls_selected %<>%
  select(ID, 
         # Weights
         paste0("r", 1:5, "wtrespb"),
         # Birthyear
         rabyear_v2,
         # Age and gender
         paste0("r", 1:5, "agey_v3"), female_v2, 
         # Education
         childhood_edu, continuing_edu, edu_cat,
         # Word recall scores
         contains("imrc"), contains("dlrc"), 
         # Childhood residence
         childhood_1st_resi, childhood_1st_region, rarural_child, 
         # Parental SES
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
         # Birth order
         first_child_v2, 
         # Occupation
         contains("first_occupation"), contains("occupation_30"), 
         # Marital status
         contains("first_maritalsts"), marital_30, maritalsts_30_age, 
         # Adulthood Rural and hukou
         paste0("h", 1:5, "rural"), region_eng, province_eng, rural_30, 
         contains("first_hukou"), contains("hukou_30"), 
         # Number of children at 30 years old
         contains("chldstate_30"))

#---- Dropping people ----
##---- 1. missing continuing education info ----
charls_selected %>% filter(is.na(continuing_edu)) %>% nrow() #n = 828
charls_selected %<>%
  filter(!is.na(continuing_edu))

#---- Save the data ----
# Difference with main dataset is that main dataset exclude for those missing one outcome
# The analysis in later scripts were conducted in the main dataset
# If this paper is back in place, multiple imputation should be run in this dataset
# and analysis needs to rerun in the imputed dataset
save(charls, charls_selected, 
     file = here::here("data", "analysis_data", "charls_selected_pred_cont.RData"))
