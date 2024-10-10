# Analytic dataset construction
# Created by: Yingyan Wu
# Oct.10.2024

#---- Package loading + preparation ----
#clear the console & environment
rm(list = ls())

# Packages
if (!require("pacman")){
  install.packages("pacman", repos = 'http://cran.us.r-project.org')
}
p_load("haven", "magrittr", "plyr", "tidyverse", "mice", "DataExplorer")

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the dataset ----
imp_pmm_40 <- readRDS(here::here("data", "analysis_data", "imp_pmm_40_stacked.RDS"))

###---- prepare the data ----
imp_pmm_40 %<>%
  dplyr::mutate(
    childhood_edu = factor(as.character(childhood_edu)), # drop order
    continuing_edu = factor(as.character(continuing_edu)), # drop order
    continuing_edu_2cat = factor(case_when(continuing_edu == 0 ~ 0,
                                           continuing_edu %in% c(1, 2) ~ 1)),
    occupation_30_5cat =
      as.factor(case_when(occupation_30 %in% c(1, 2) ~ 1,
                          occupation_30 %in% c(3, 4, 5) ~ 2,
                          occupation_30 == 6 ~ 3,
                          occupation_30 == 7 ~ 4,
                          occupation_30 == 8 ~ 5)),
    occupation_30_2cat = as.factor(case_when(occupation_30 %in% c(1, 2) ~ 0, # agriculture
                                             occupation_30 %in% c(3:8) ~ 1)), # non-agriculture
    rameduc_2cat = as.factor(case_when(rameduc_c %in% 1:3 ~ 1, # with formal edu
                                       rameduc_c == 0 ~ 0)),# no formal edu
    rafeduc_2cat = as.factor(case_when(rafeduc_c %in% 1:3 ~ 1, 
                                       rafeduc_c == 0 ~ 0)),
    first_hukou_2cat = as.factor(case_when(first_hukou == 1 ~ 0, # agricultural hukou
                                           first_hukou %in% 2:3 ~ 1)), # non agricultural hukou
    # childhood_edu = factor(as.character(childhood_edu)), # drop order
    # continuing_edu = factor(as.character(continuing_edu)), # drop order
    hukou_30_2cat = as.factor(case_when(hukou_30 == 1 ~ 0,
                                        hukou_30 %in% 2:3 ~ 1)),
    across(ends_with("agey_v3"), ~ .x - r1agey_v3,
           .names = "{.col}fu_time")) %>%
  dplyr::rename_at(vars(ends_with("fu_time")),
                   function(x) str_replace(x, "agey_v3", "")) %>%
  group_by(imp) %>%
  # mutate(r1wtrespb_rescaled = r1wtrespb/mean(r1wtrespb)) %>%
  # # Rescale to have mean of 1 to make the survey weighted mixed model faster
  # # tested in previous attempts, the results are the same beofre and after rescaling
  ungroup()

# # Sanity check
# imp_pmm_40 %>% group_by(imp) %>%
#   summarise(mean(r1wtrespb_rescaled, na.rm = TRUE),
#             sd(r1wtrespb_rescaled, na.rm = TRUE))

# Create censor indicator
rc_temp <- imp_pmm_40 %>%
  select(paste0("r", 1:2, "imrc"),
         paste0("r", 3, "imrc_cleaned"),
         paste0("r", 4:5, "imrc_eqt"),
         paste0("r", 1:2, "dlrc"),
         paste0("r", 3, "dlrc_cleaned"),
         paste0("r", 4:5, "dlrc_eqt")) %>%
  set_colnames(c(paste0("r", 1:5, "imrc"), paste0("r", 1:5, "dlrc"))) %>%
  mutate(across(everything(), 
                function(x) ifelse(is.na(x), 1, 0),
                .names = "{.col}_censor"))

imp_pmm_40 <- cbind(imp_pmm_40, rc_temp %>% select(ends_with("_censor")))

#---- Analytic dataset ----
# Long dataset 
rctib_long <- imp_pmm_40 %>%
  select(ID, imp, contains("agey_v3"), 
         contains("fu_time"),
         paste0("r", 1:2, "imrc"),
         paste0("r", 3, "imrc_cleaned"),
         paste0("r", 4:5, "imrc_eqt"),
         paste0("r", 1:2, "dlrc"),
         paste0("r", 3, "dlrc_cleaned"),
         paste0("r", 4:5, "dlrc_eqt")) %>%
  dplyr::rename_at(vars(contains(c("imrc", "dlrc"))),
                   function(x) str_replace(x, "_eqt|_cleaned", "")) %>%
  pivot_longer(cols = !c(ID, imp), 
               names_to = c("wave", ".value"), 
               names_pattern = "r(.)(.*)") %>%
  mutate(wave = as.numeric(wave)) %>%
  dplyr::arrange(ID, imp, agey_v3, fu_time) %>%
  filter(!(is.na(imrc) & is.na(dlrc))) %>%
  mutate(
    first_imrc = ifelse(!is.na(imrc) & row_number() == min(which(!is.na(imrc))), 1, 0),
    first_dlrc = ifelse(!is.na(dlrc) & row_number() == min(which(!is.na(dlrc))), 1, 0))

imp_pmm_long <- imp_pmm_40 %>% 
  select(-contains(c("imrc", "dlrc", "agey_v3", "fu_time"))) %>%
  left_join(rctib_long, by = c("ID", "imp"))

###---- Save the data ----
save(imp_pmm_40, imp_pmm_long,
     file = here::here("data", "analysis_data", "imp_pmm_40_wide_long.RData"))
