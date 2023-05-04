# Data export for equating method
# Created by: Yingyan Wu
# Some code was adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# Mar.13.2023

# For the specific missing characters in stata, the data was read using 
# the package `readstata13` and checked the distribution of refused responses
# and no response for the word recall scores
# The code for reading data using readstata13 was commented out below.

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "glue", "gtsummary")

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the datasets ----
#---- **harmonized ----
harmonized_variables <- c("ID", "householdID", "pnc", "communityID",
                          "ID_w1", "householdID_w1", 
                          # Weights
                          paste0("r", 1:4, "wtrespb"),
                          paste0("r", 1:4, "agey"),
                          "ragender",
                          paste0("h", 1:4, "rural"),
                          paste0("r", 1:4, "hukou"),
                          "raeduc_c",
                          paste0("r", 1:4, "mstat"),
                          paste0("r", 1:4, "lbrf_c"), # labor force status
                          paste0("r", 1:4, "imrc"), # immediate word recall
                          paste0("r", 1:4, "dlrc") # delayed word recall
)

harmonized <- read_dta(here::here("data", "raw_data", "harmonized",
                                    "H_CHARLS_D_Data.dta"),
                        col_select = all_of(harmonized_variables))

# harmonized <- read.dta13(here::here("data", "raw_data", "harmonized",
#                                     "H_CHARLS_D_Data.dta"),
#                          missing.type = TRUE,
#                          select.cols = all_of(harmonized_variables))

#---- Core data for cleaning the imrc and dlrc ----
# Adapted from Mingfei Dong's data reading script, 
# Crystal's reading result snippet from make_figures.R in exposure_trajectories
# and the IDRE website
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("health_status_and_functioning", "cognition")

folder_index <- c(6, 7) # 2015, 2018 waves only

for (i in folder_index){
  # get all file names
  files <- list.files(path = folders[i], full.names = TRUE)
  files <- files[str_detect(files, regex(glue_collapse(datasets, sep="|"), 
                                         ignore_case = T))]
  # Read in as list
  list <- lapply(files, read_dta)
  # list <- lapply(files, read.dta13)
  names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
  # Assign names for each wave's list
  assign(paste0("wave", which(folder_index == i)), list)
}

rm(list)

# ---- **join the datasets ----
# Waves
charls_w3 <- join_all(wave1, 
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w3_", x)})
# colnames(charls_w3)

charls_w4 <- join_all(wave2, 
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w4_", x)})
# colnames(charls_w4)

#---- ** join to a single dataset (wave 1 baseline) ----
# Merge all the waves (All the participants!)
charls <- join_all(c(list(charls_w3, charls_w4)),
                   by = c("ID"), # "ID_w234" = "ID", # somehow it didn't work anymore. Rename the ID_w234 in w1 above
                   # "householdID", "communityID"),# Pending: needs to figure out this later
                   type = "full") %>% 
  as_tibble()

gc()

#---- Age cleaning ----
age_y <- harmonized %>% dplyr::select(contains("agey"), -contains("LHagey")) %>% 
  apply(., 1, impute_ages) %>% t()

harmonized[, paste0("r", 1:4, "age_y")] <- age_y
rm(age_y)

harmonized %<>%
  select(-contains("agey"))

# Sanity check
harmonized %>% select(contains("age_y")) %>%
  apply(., 2, proc_sum, more = T)

#---- Gender ----
harmonized %<>% 
  mutate(female = case_when(ragender == 1 ~ 0,
                            ragender == 2 ~ 1))
harmonized %<>%
  select(-ragender)

#---- Early life edu ----
harmonized %<>%
  mutate(
    childhood_edu = case_when(raeduc_c == 1 ~ 0,
                              raeduc_c %in% c(2, 3, 4) ~ 1,
                              raeduc_c %in% c(5, 6, 7) ~ 2,
                              raeduc_c %in% c(8, 9, 10) ~ 3))

#---- imrc and dlrc cleaning ----
harmonized %<>%
  left_join(charls %>% select(ID, w3_dc006s11, w3_dc006s12,
                              w4_dc028_w4_s11, w4_dc028_w4_s12,
                              w3_dc027s11, w3_dc027s12,
                              w4_dc047_w4_s11, w4_dc047_w4_s12),
            by = "ID")

# immediate word recall
with(harmonized %>% filter(r3imrc == 0),
     table(w3_dc006s11, w3_dc006s12, useNA = "ifany"))
with(harmonized %>% filter(r4imrc == 0),
     table(w4_dc028_w4_s11, w4_dc028_w4_s12, useNA = "ifany"))
# There are 1877 participants with non responses (can't recall any of the words)
# But the rest 653 participants refused to recall (needs to be NAed)

harmonized %<>% 
  mutate(r3imrc_cleaned = case_when(
    r3imrc == 0 & w3_dc006s12 == 12 ~ NA_real_,
    # Note: if using read.dta13, the value will be "12 Refuse to Recall" for dc006s12s
    TRUE ~ as.numeric(r3imrc)),
    r4imrc_cleaned = case_when(
      r4imrc == 0 & w4_dc028_w4_s12 == 12 ~ NA_real_,
      TRUE ~ as.numeric(r4imrc)))
# Sanity check
with(harmonized, table(r3imrc_cleaned, r3imrc, useNA = "ifany"))
with(harmonized, table(r4imrc_cleaned, w4_dc028_w4_s12, useNA = "ifany"))

# Delayed word recall
with(harmonized %>% filter(r3dlrc == 0),
     table(w3_dc027s11, w3_dc027s12, useNA = "ifany"))
with(harmonized %>% filter(r4dlrc == 0),
     table(w4_dc047_w4_s11, w4_dc047_w4_s12, useNA = "ifany"))
# There are 2334 participants with non responses (can't recall any of the words)
# But the rest 505 participants seem to be NAed

harmonized %<>% 
  mutate(r3dlrc_cleaned = case_when(r3dlrc == 0 & w3_dc027s12 == 12 ~ NA_real_,
                                    TRUE ~ r3dlrc),
         r4dlrc_cleaned = case_when(r4dlrc == 0 & w4_dc047_w4_s12 == 12 ~ NA_real_,
                                    TRUE ~ r4dlrc))
# Sanity check
with(harmonized, table(r3dlrc_cleaned, r3dlrc, useNA = "ifany"))
with(harmonized, table(r4dlrc_cleaned, r4dlrc, useNA = "ifany"))

#---- Baseline Table 1 ----
table1 <- harmonized %>%
  select(r1age_y, female, raeduc_c, childhood_edu, h1rural,
         r1hukou, h1rural, r1imrc, r1dlrc) %>%
  labelled::set_variable_labels(
    r1age_y = "Baseline age in years",
    female = "Female (%)",
    raeduc_c = "Early life education level (%)",
    childhood_edu = "Early life education level (simplified) (%)",
    h1rural = "Rural residence (%)",
    r1hukou = "Hukou status (%)",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    h1rural = c("Yes" = 1, "No" = 0),
    childhood_edu = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3)) %>%
  modify_if(is.labelled, to_factor) %>%
  modify_if(is.factor,
            function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ c("{mean} ({sd})",
                                          "({min}, {max})")),
    type = all_continuous() ~ "continuous2",
    # by = childhood_edu20,
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing") %>%
  # modify_spanning_header(starts_with("stat_") ~ "**Baseline CES-D**") %>%
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )

table1 %>% as_flex_table() %>%
  flextable::save_as_docx(path = here::here("output", "tables", 
                                            "table1_equating_sample.docx"))
#---- 2015 to 2018 long dataset ----
harmonized_long_1518 <- harmonized %>%
  select(ID:pnc, female, childhood_edu, 
         contains("ra"), contains(c("3", "4")), -contains(c("1", "2"))) %>%
  dplyr::rename_at(c("h3rural", "h4rural"), function(x) sub("h", "r", x)) %>%
  pivot_longer(cols = !ID:raeduc_c,
               names_to = c("wave", ".value"),
               names_pattern = ("r(.)(.*)"))

#---- ** Table 1 ----
table1_1518 <- harmonized_long_1518 %>%
  select(age_y, female, raeduc_c, childhood_edu, rural,
         hukou, imrc_cleaned, dlrc_cleaned, wave) %>%
  labelled::set_variable_labels(
    age_y = "Age at waves",
    female = "Female (%)",
    raeduc_c = "Early life education level (%)",
    childhood_edu = "Early life education level (simplified) (%)",
    rural = "Rural residence (%)",
    hukou = "Hukou status (%)",
    imrc_cleaned = "Immediate word recall", 
    dlrc_cleaned = "Delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    rural = c("Yes" = 1, "No" = 0),
    childhood_edu = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3),
    wave = c("2015" = "3", "2018" = "4")) %>%
  modify_if(is.labelled, to_factor) %>%
  modify_if(is.factor,
            function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ c("{mean} ({sd})",
                                          "({min}, {max})")),
    type = all_continuous() ~ "continuous2",
    by = wave,
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing") %>%
  add_overall() %>%
  # modify_spanning_header(starts_with("stat_") ~ "**Baseline CES-D**") %>%
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )

table1_1518 %>%  as_flex_table() %>%
  flextable::save_as_docx(path = here::here("output", "tables", 
                                            "table1_equating_sample_15_18.docx"))

#---- Save the dataset ----
save(harmonized, harmonized_long_1518, file = here::here("data", "analysis_data", 
                                                         "charls_equating_samp.RData"))


