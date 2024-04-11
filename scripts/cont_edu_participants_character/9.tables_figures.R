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

#---- Load the data ----
load(here::here("data", "analysis_data", "charls_selected_pred_cont.RData"))

#---- Table 1 ----
table_1 <- charls_selected %>% 
  filter(r1agey_v3 >= 45) %>%
  select(r1agey_v3, continuing_edu,
         # Early life predictors
         rabyear_v2, female_v2,
         first_child_v2, 
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
         childhood_1st_resi, childhood_1st_region, rarural_child, 
         first_hukou, 
         # Early-adulthood predictors
         childhood_edu, occupation_30_5cat, marital_30, hukou_30, rural_30, 
         chldstate_30) %>%
  labelled::set_variable_labels(
    r1agey_v3 = "Baseline age at 2011 in years",
    continuing_edu = "Continuing education level (%)",
    rabyear_v2 = "Birth year",
    female_v2 = "Female (%)",
    first_child_v2 = "Birth order: first child (%)",
    rameduc_c = "Mother's education level (%)", 
    rafeduc_c = "Father's education level (%)", 
    ramoccup_c = "Mother's occupation before age 17 (%)", 
    rafoccup_c = "Father's occupation before age 17 (%)",
    childhood_1st_resi = "Childhood first residence (%)", 
    childhood_1st_region = "Childhood first region (%)",
    rarural_child = "Childhood rural residence (%)",
    first_hukou = "First hukou status (%)", 
    # Early-adulthood predictors
    childhood_edu = "Early life education level (%)",
    occupation_30_5cat = "Occupation at age 30 (%)",
    marital_30 = "Marital status at age 30 (%)",
    rural_30 = "Rural residence at age 30 (%)",
    hukou_30 = "Hukou status at age 30 (%)",
    chldstate_30 = "Number of Children at age 30 (%)") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    continuing_edu = c("No continuing edu" = 0, "Continuing edu w/o a diploma" = 1,
                       "Continuing edu w/ a diploma" = 2),
    female_v2 = c("Yes" = 1, "No" = 0),
    # first_child_v2 = c("Yes" = 1, "No" = 0),
    rameduc_c = c("No formal education (illiterate)" = 0,
                  "Elementary school or less" = 1,
                  "Middle school" = 2,
                  "College and above" = 3),
    rafeduc_c = c("No formal education (illiterate)" = 0,
                  "Elementary school or less" = 1,
                  "Middle school" = 2,
                  "College and above" = 3),
    ramoccup_c = c("Agricultural" = 1, "Non-agricultural" = 0),
    rafoccup_c = c("Agricultural" = 1, "Non-agricultural" = 0),
    childhood_1st_region = c("East" = 1,
                             "Middle" = 2,
                             "West" = 3,
                             "Northeast" = 4),
    # rarural_child = c("Yes" = 1, "No" = 0),
    first_hukou = c("Agricultural hukou" = 1, "Non-agricultural hukou" = 2,
                    "Unified residence hukou" = 3, "Do not have hukou" = 4),
    childhood_edu = c("No formal education (illiterate)" = 0,
                      "Elementary school or less" = 1,
                      "Middle school" = 2,
                      "College and above" = 3),
    occupation_30_5cat = c("Agriculture related" = 1,
                      "Non-agriculture related" = 2,
                      "Unemployed" = 3,
                      "Retired" = 4,
                      "Never work" = 5),
    # marital_30 = c("Yes" = 1, "No" = 0),
    hukou_30 = c("Agricultural hukou" = 1, "Non-agricultural hukou" = 2,
                    "Unified residence hukou" = 3, "Do not have hukou" = 4),
    # rural_30 = c("Yes" = 1, "No" = 0)
    ) %>%
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
    by = continuing_edu,
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
  path = here::here("output", "tables", "table1_continuing_pred.docx"))
# # The table will be formatted in a linked excel
# writexl::write_xlsx(as_tibble(table_1), 
#                     here::here("output", "tables", "table1_proposal.xlsx"))
