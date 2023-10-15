# CHARLS_data_table_1_proposal
# Created by: Yingyan Wu
# Nov.05.2022

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "glue", 
       "survey", "labelled", "gtsummary")
# Do not load "summarytools" as it contains conflicted functions as dplyr

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the data ----
load(here::here("data", "analysis_data", "charls_selected.RData"))
charls_selected %<>%
  labelled::set_value_labels(
    edu_cat = c("No Formal Education (Illiterate)" = 0,
                "No formal education + continuing education without a diploma" = 1,
                "No formal education + continuing education with diploma" = 2,
                "Elementary school or less + no continuing education" = 3,
                "Elementary school or less + continuing education without a diploma" = 4,
                "Elementary school or less + continuing education with a diploma" = 5,
                "Vocational school/middle school/high school + no continuing education" = 6,
                "Vocational school/middle school/high school + continuing education without a diploma" = 7,
                "Vocational school/middle school/high school + continuing education with a diploma" = 8,
                "College & above + no continuing education" = 9,
                "College & above + continuing education without a diploma" = 10,
                "College & above + continuing education with a diploma" = 11))


#---- Table 1 ----
table_1 <- charls_selected %>% 
  filter(r1agey_v3 >= 45) %>%
  select(r1agey_v3, 
         female_v2,
         edu_cat,
         r1lbrf_c,
         h1rural,
         r1mstat_c_v2,
         rameduc_c, rafeduc_c, ramomoccup_c_v2, 
         radadoccup_c_v2,
         r1imrc, r1dlrc) %>%
  labelled::set_variable_labels(
    r1agey_v3 = "Baseline age in years",
    female_v2 = "Female (%)",
    edu_cat = "Education level (%)",
    r1lbrf_c = "Labor force participation (%)",
    h1rural = "Rural residence (%)",
    r1mstat_c_v2 = "Marital status (%)",
    rameduc_c = "Mother's education level (%)", 
    rafeduc_c = "Father's education level (%)", 
    ramomoccup_c_v2 = "Mother's occupation before age 17 (%)", 
    radadoccup_c_v2 = "Father's occupation before age 17 (%)",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female_v2 = c("Yes" = 1, "No" = 0),
    r1lbrf_c = c("Agriculture related employed" = 1,
                 "Agriculture related self-employed" = 2,
                 "Non-agriculture employed" = 3,
                 "Non-agriculture self-employed" = 4,
                 "Non-agriculture family business" = 5,
                 "Unemployed" = 6,
                 "Retired" = 7,
                 "Never work" = 8),
    h1rural = c("Yes" = 1, "No" = 0),
    r1mstat_c_v2 = c("Married/Partnered" = 1,
                     "Separated/Divorced/Never married" = 2,
                     "Widowed"	= 3),
    rameduc_c = c("No formal education illiterate" = 0,
                  "Elementary school or less" = 1,
                  "Vocational school/middle school/high school" = 2,
                  "College & above" = 3), 
    rafeduc_c = c("No formal education illiterate" = 0,
                  "Elementary school or less" = 1,
                  "Vocational school/middle school/high school" = 2,
                  "College & above" = 3) , 
    ramomoccup_c_v2 = c("Farming" = 1,
                        "Non-Agricultural" = 2), 
    radadoccup_c_v2 = c("Farming" = 1,
                        "Non-Agricultural" = 2)) %>%
  modify_if(is.labelled, to_factor) %>%
  modify_if(is.factor, 
            function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ c("{mean} ({sd})",
                                          "({min}, {max})")),
    type = all_continuous() ~ "continuous2",
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

table_1 %>% as_flex_table()

#---- **save the table ----
table_1 %>% as_flex_table() %>% flextable::save_as_docx(
  path = here::here("output", "tables", "table1_proposal.docx"))
# # The table will be formatted in a linked excel
# writexl::write_xlsx(as_tibble(table_1), 
#                     here::here("output", "tables", "table1_proposal.xlsx"))


#---- Table 1 for weighted ----
charls_selected_svy <- charls_selected %>%
  filter(r1agey_v3 >= 45 & !is.na(r1wtrespb)) %>%
  select(r1agey_v3, female_v2, h1rural, continuing_edu,
         childhood_edu,
         # edu_cat, rameduc_c, rafeduc_c, ramomoccup_c_v2, radadoccup_c_v2,
         r1imrc, r1dlrc, r1wtrespb) %>%
  labelled::set_variable_labels(
    r1agey_v3 = "Baseline age in years",
    female_v2 = "Female (%)",
    # edu_cat = "Education level (%)",
    continuing_edu = "Continuing education (%)",
    childhood_edu = "Early life education",
    h1rural = "Rural residence (%)",
    # rameduc_c = "Mother's education level (%)", 
    # rafeduc_c = "Father's education level (%)", 
    # ramomoccup_c_v2 = "Mother's occupation (%)", 
    # radadoccup_c_v2 = "Father's occupation (%)",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female_v2 = c("Yes" = 1, "No" = 0),
    h1rural = c("Yes" = 1, "No" = 0),
    continuing_edu = c("No continuing education" = 0, 
                       "Continuing education without a diploma" = 1, 
                       "Continuing education with a diploma" = 2),
    childhood_edu = c("No Formal Education (Illiterate)" = 0, 
                      "Elementary school or less" = 1, 
                      "Vocational school/middle school/high school" = 2, 
                      "College & above" = 3)) %>%
  # rameduc_c = c(
  #   "No formal education" = 0,
  #   "Elementary school or less" = 1,
  #   "Vocational school/middle school/high school" = 2,
  #   "College & above" = 3), 
  # rafeduc_c = c(
  #   "No formal education" = 0,
  #   "Elementary school or less" = 1,
  #   "Vocational school/middle school/high school" = 2,
  #   "College & above" = 3) , 
  # ramomoccup_c_v2 = c("Farming" = 1,
#                     "Non-Agricultural" = 2), 
# radadoccup_c_v2 = c("Farming" = 1,
#                     "Non-Agricultural" = 2)) %>%
modify_if(is.labelled, to_factor) %>%
  modify_if(is.factor,
            function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  survey::svydesign(~1, data = ., weights = ~r1wtrespb)

table1 <- tbl_svysummary(
  data = charls_selected_svy,
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)",
                   all_continuous() ~ c("{mean} ({sd})")),
  type = all_continuous() ~ "continuous",
  digits = list(all_continuous() ~ 1,
                all_categorical() ~ c(0, 1)),
  by = "childhood_edu") %>% 
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )  %>%
  modify_header(
    list(stat_1	 ~ "**No Formal Education (Illiterate)**, N = {n_unweighted} ({style_percent(p,digits = 1)}%)",
         stat_2 ~ "**Elementary school or less**, N = {n_unweighted} ({style_percent(p,digits = 1)}%)",
         stat_3 ~ "**Vocational school/middle school/high school**, N = {n_unweighted} ({style_percent(p,digits = 1)}%)",
         stat_4 ~ "**College & above**, N = {n_unweighted} ({style_percent(p,digits = 1)}%)"))

table1 %>% as_flex_table() 

table1 %>% as_flex_table() %>% flextable::save_as_docx(
  path = here::here("output", "tables", "table1_F99_Oct15.docx"))
