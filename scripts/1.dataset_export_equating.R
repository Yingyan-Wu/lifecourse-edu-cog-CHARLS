# Data export for equating method
# Created by: Yingyan Wu
# Some code adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# Mar.13.2023

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "gtsummary")

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the datasets ----
#---- **harmonized ----
harmonized_variables <- c("ID", "householdID", "pnc", "communityID",
                          "ID_w1", "householdID_w1", 
                          # paste0("inw", 1:4),
                          # paste0("r", 1:4, "iwstat"),
                          # Weights
                          paste0("r", 1:4, "wtrespb"),
                          # "hacohort_c",
                          # "rabyear", "rabmonth", "rabday", 
                          paste0("r", 1:4, "agey"),
                          # "radyear", "radmonth",
                          "ragender",
                          # "rabplace_c",
                          paste0("h", 1:4, "rural"),
                          paste0("r", 1:4, "hukou"),
                          "raeduc_c",
                          paste0("r", 1:4, "mstat"),
                          paste0("r", 1:4, "lbrf_c"), # labor force status
                          paste0("r", 1:4, "imrc"), # immediate word recall
                          paste0("r", 1:4, "dlrc") # delayed word recall
                          )

harmonized <- haven::read_dta(here::here("data", "raw_data", "harmonized",
                                         "H_CHARLS_D_Data.dta"),
                              col_select = all_of(harmonized_variables))

#---- Age cleaning ----
age_y <- harmonized %>% dplyr::select(contains("agey"), -contains("LHagey")) %>% 
  apply(., 1, impute_ages) %>% t()

harmonized[, paste0("r", 1:4, "age_y")] <- age_y
rm(age_y)

harmonized %<>%
  select(-contains("agey"))

# Sanity check
harmonized %>% select(contains("agey")) %>%
  apply(., 2, proc_sum, more = T)

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

#---- Table 1 ----
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

table1 %>%  as_flex_table() %>%
  flextable::save_as_docx(path = here::here("output", "tables", 
                                            "table1_equating_sample.docx"))
#---- Save the dataset ----
saveRDS(harmonized, file = here::here("data", "analysis_data", 
                                      "charls_equating_samp.RDS"))
