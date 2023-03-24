# 1.1 equating method for memory scores
# Created by Yingyan Wu
# Mar.14.2023

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "gtsummary", "equate")

#---- Load the datasets ----
load(here::here("data", "analysis_data", 
                "charls_equating_samp.RData"))

#---- 1. Restrict to a calibration group ----
# Restrict to a “calibration group” that has a common age range, 
# and common education range across the time points. 
# This is a highly selected, “biased” sample.
summary(harmonized$r3age_y)
summary(harmonized$r4age_y)

summary(charls_selected$childhood_edu)

charls_selected <- harmonized_long_1518 %>%
  na.omit() %>%
  filter(age_y >= 60 & age_y < 90 & raeduc_c >= 4 & raeduc_c != 10 &
           hukou != 3 & hukou != 4) 

#---- **Table 1 ----
table1_1518 <- charls_selected %>%
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
  add_p() %>%
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )

table1_1518 %>%  as_flex_table() %>%
  flextable::save_as_docx(path = here::here("output", "tables",
                                            "table1_equating_calibrate_sample_15_18.docx"))

#---- Generate weights ----
# Generate weights, to ensure the age/education distributions are the same 
# in each study wave. This will remove the aging and education effects from the 
# equating algorithm. You can generate weights using any approach; 
# AG uses IPW but other options work.




a.            In this biased sample, be sure that every possible value of the focal and reference variables are included.

3.            Perform equipercentile equating using the R EQUATE package.

4.            Apply the equipercentile equating algorithm to the full sample in the focal group.
