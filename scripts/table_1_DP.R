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
load(here::here("data", "analysis_data", "charls_proposal.RData"))
charls %<>%
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
table_1 <- charls %>% 
  mutate(r1age = 2011 - rabyear,
         female = case_when(ragender == 1 ~ 0,
                            ragender == 2 ~ 1)) %>%
  filter(r1age >= 45) %>%
  select(r1age, 
         female,
         edu_cat,
         r1lbrf_c,
         h1rural,
         r1mstat,
         rameduc_c, rafeduc_c, ramomoccup_c, 
         radadoccup_c,
         r1imrc, r1dlrc) %>%
  labelled::set_variable_labels(
    r1age = "Baseline age in years",
    female = "Female (%)",
    edu_cat = "Education level (%)",
    r1lbrf_c = "Labor force participation (%)",
    h1rural = "Rural residence (%)",
    r1mstat = "Marital status (%)",
    rameduc_c = "Mother's education level (%)", 
    rafeduc_c = "Father's education level (%)", 
    ramomoccup_c = "Mother's occupation before age 17 (%)", 
    radadoccup_c = "Father's occupation before age 17 (%)",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    r1lbrf_c = c("Agriculture related employed" = 1,
                 "Agriculture related self-employed" = 2,
                 "Non-agriculture employed" = 3,
                 "Non-agriculture self-employed" = 4,
                 "Non-agriculture family business" = 5,
                 "Unemployed" = 6,
                 "Retired" = 7,
                 "Never work" = 8),
    h1rural = c("Yes" = 1, "No" = 0),
    r1mstat = c("Married" = 1,
                "Partnered" = 3,
                "Separated" = 4,
                "Divorced" = 5,
                "Widowed"	= 7,
                "Never married" = 8),
    rameduc_c = c(
      "No formal education illiterate" = 1,
      "Did not finish elementary school but capable of reading and/or writing" = 2,
      "Sishu" = 3,
      "Elementary school" = 4,
      "Middle school" = 5,
      "High school" = 6,
      "Vocational school" = 7,
      "Two/Three Year College/Associate degree" = 8,
      "Four Year College/Bachelor's degree" = 9,
      "Post-graduate(Master)" = 10,
      "Post-graduate(PhD)" = 11), 
    rafeduc_c = c(
      "No formal education illiterate" = 1,
      "Did not finish elementary school but capable of reading and/or writing" = 2,
      "Sishu" = 3,
      "Elementary school" = 4,
      "Middle school" = 5,
      "High school" = 6,
      "Vocational school" = 7,
      "Two/Three Year College/Associate degree" = 8,
      "Four Year College/Bachelor's degree" = 9,
      "Post-graduate(Master)" = 10,
      "Post-graduate(PhD)" = 11) , 
    ramomoccup_c = c("Farming" = 1,
                     "Non-Agricultural" = 2), 
    radadoccup_c = c("Farming" = 1,
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


#---- Table 1 for edu & dementia conference----
table_1_r <- charls %>% 
  mutate(r1age = 2011 - rabyear,
         female = case_when(ragender == 1 ~ 0,
                            ragender == 2 ~ 1),
         rameduc_c = case_when(rameduc_c == 1 ~ 0,
                               rameduc_c %in% c(2, 3, 4) ~ 1,
                               rameduc_c %in% c(5, 6, 7) ~ 2,
                               rameduc_c %in% c(8, 9, 10, 11) ~ 3),
         rafeduc_c = case_when(rafeduc_c == 1 ~ 0,
                               rafeduc_c %in% c(2, 3, 4) ~ 1,
                               rafeduc_c %in% c(5, 6, 7) ~ 2,
                               rafeduc_c %in% c(8, 9, 10, 11) ~ 3)) %>%
  filter(r1age >= 45) %>%
  select(r1age, 
         female,
         edu_cat,
         h1rural,
         rameduc_c, 
         rafeduc_c, 
         ramomoccup_c, 
         radadoccup_c,
         r1imrc, r1dlrc) %>%
  labelled::set_variable_labels(
    r1age = "Baseline age in years",
    female = "Female (%)",
    edu_cat = "Education level (%)",
    h1rural = "Rural residence (%)",
    rameduc_c = "Mother's education level (%)", 
    rafeduc_c = "Father's education level (%)", 
    ramomoccup_c = "Mother's occupation (%)", 
    radadoccup_c = "Father's occupation (%)",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    h1rural = c("Yes" = 1, "No" = 0),
    rameduc_c = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3), 
    rafeduc_c = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3) , 
    ramomoccup_c = c("Farming" = 1,
                     "Non-Agricultural" = 2), 
    radadoccup_c = c("Farming" = 1,
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

table_1_r %>% as_flex_table()

#---- **save the table ----
table_1_r %>% as_flex_table() %>% flextable::save_as_docx(
  path = here::here("output", "tables", "table1_edu_dementia_ucsf.docx"))
# # The table will be formatted in a linked excel
# writexl::write_xlsx(as_tibble(table_1), 
#                     here::here("output", "tables", "table1_proposal.xlsx"))


#---- histogram of eductaion level in CHARLS ----
charls %>% 
  mutate(early_life_edu = case_when(
    # No formal education
    raeduc_c == 1 ~ "No Formal Education",
    # less than vocational school (finished elementary school)
    raeduc_c %in% c(2, 3, 4) ~ "Elementary school or less",
    # vocational school/middle school/high school
    raeduc_c %in% c(5, 6, 7) ~ "Vocational school/middle school/high school",
    # college & above
    raeduc_c %in% c(8, 9, 10) ~ "College & above")) %>%
  select(early_life_edu) %>%
  filter(!is.na(early_life_edu)) %>%
  mutate(early_life_edu = 
           factor(
             early_life_edu, 
             levels = c("No Formal Education",
                        "Elementary school or less",
                        "Vocational school/middle school/high school",
                        "College & above"))) %>%
  group_by(early_life_edu) %>%
  dplyr::summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  ggplot() +
  geom_bar(aes(x = early_life_edu, y = perc*100), stat="identity") +
  scale_x_discrete(limits = rev) +
  labs(x = "", y = "%") +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12))



hist(charls$raeduc_c)
