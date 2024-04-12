# CHARLS_data_table_1_continuing_edu complete case
# Created by: Yingyan Wu
# Apr.11.2024

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "glue", 
       "survey", "labelled", "gtsummary")
# Do not load "summarytools" as it contains conflicted functions as dplyr

#---- Load the data ----
imp_pmm_40 <- readRDS(here::here("data", "analysis_data", "imp_pmm_40_stacked.RDS"))
imp_pmm_40 %<>%
  mutate(occupation_30_5cat = case_when(occupation_30 %in% c(1, 2) ~ 1,
                                        occupation_30 %in% c(3, 4, 5) ~ 2,
                                        occupation_30 == 6 ~ 3,
                                        occupation_30 == 7 ~ 4,
                                        occupation_30 == 8 ~ 5))

#---- Table 1 ----
n_imp = 40
labelled_data <- imp_pmm_40 %>%
  filter(r1agey_v3 >= 45) %>%
  dplyr::mutate(across(c("female_v2", "first_child_v2", "rarural_child",
                         "marital_30", "rural_30"),
                       function(x) factor(x, levels = c(1, 0),
                                          labels = c("Yes", "No")))) %>%
  dplyr::mutate(
    continuing_edu = factor(continuing_edu, levels = c(0, 1, 2),
                            labels = c("No continuing edu", 
                                       "Continuing edu w/o a diploma",
                                       "Continuing edu w/ a diploma")),
    rameduc_c = factor(rameduc_c, levels = c(0, 1, 2, 3),
                       labels = c("No formal education (illiterate)",
                                  "Elementary school or less",
                                  "Middle school",
                                  "College and above")),
    rafeduc_c = factor(rafeduc_c, levels = c(0, 1, 2, 3),
                       labels = c("No formal education (illiterate)",
                                  "Elementary school or less",
                                  "Middle school",
                                  "College and above")),
    ramoccup_c = factor(ramoccup_c, levels = c(0, 1),
                        labels = c("Non-agricultural", "Agricultural")),
    rafoccup_c = factor(rafoccup_c, levels = c(0, 1),
                        labels = c("Non-agricultural", "Agricultural")),
    childhood_1st_region = factor(childhood_1st_region, levels = c(1, 2, 3, 4),
                                  labels = c("East", "Middle", "West", "Northeast")),
    first_hukou = factor(first_hukou, levels = c(1, 2, 3, 4),
                         labels = c("Agricultural hukou", "Non-agricultural hukou",
                                    "Unified residence hukou", "Do not have hukou")),
    childhood_edu = factor(childhood_edu, levels = c(0, 1, 2, 3),
                           labels = c("No formal education (illiterate)",
                                      "Elementary school or less",
                                      "Middle school",
                                      "College and above")),
    occupation_30_5cat = factor(occupation_30_5cat, levels = c(1, 2, 3, 4, 5),
                                labels = c("Agriculture related",
                                           "Non-agriculture related",
                                           "Unemployed",
                                           "Retired",
                                           "Never work")),
    hukou_30 = factor(hukou_30, levels = c(1, 2, 3, 4),
                      labels = c("Agricultural hukou", "Non-agricultural hukou",
                                 "Unified residence hukou", "Do not have hukou"))) %>%
  labelled::set_variable_labels(
    r1agey_v3 = "Baseline age at 2011 in years",
    continuing_edu = "Continuing education level (%)",
    # rabyear_v2 = "Birth year",
    female_v2 = "Female (%)",
    first_child_v2 = "Birth order: first child (%)",
    rameduc_c = "Mother's education level (%)", 
    rafeduc_c = "Father's education level (%)", 
    ramoccup_c = "Mother's occupation before age 17 (%)", 
    rafoccup_c = "Father's occupation before age 17 (%)",
    childhood_1st_region = "Childhood first region (%)",
    rarural_child = "Childhood rural residence (%)",
    first_hukou = "First hukou status (%)", 
    # Early-adulthood predictors
    childhood_edu = "Early life education level (%)",
    occupation_30_5cat = "Occupation at age 30 (%)",
    marital_30 = "Marital status at age 30 (%)",
    rural_30 = "Rural residence at age 30 (%)",
    hukou_30 = "Hukou status at age 30 (%)",
    chldstate_30 = "Number of Children at age 30")


##---- categorical ----
cat_imp_svy <- labelled_data %>%
  select(continuing_edu,
         # Early life predictors
         female_v2,
         first_child_v2, 
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
         childhood_1st_region, rarural_child, 
         first_hukou, 
         # Early-adulthood predictors
         childhood_edu, occupation_30_5cat, marital_30, hukou_30, rural_30,
         r1wtrespb) %>%
  survey::svydesign(~ 1, data = ., weights = ~r1wtrespb)

temp_cat_weighted <- tbl_svysummary(
  data = cat_imp_svy,
  statistic = list(all_categorical() ~ c("{n_unweighted} {p}")), # For latter splitting
  by = continuing_edu,
  digits = all_categorical() ~ 1,
  missing = "no"
)

temp_cat_weighted %>% as_flex_table()

table1_cat_weighted <- temp_cat_weighted %>% as_tibble() %>%
  set_colnames(c("Characteristic", "no_cont_edu", 
                 "cont_edu_no_dplm", "cont_edu_dplm")) %>%
  filter(row_number() <= n() - 1) %>% # remove the last row in the table of median IQR for weights.
  separate(no_cont_edu, c("no_cont_edu_count", "no_cont_edu_prop"), sep = " ") %>%
  separate(cont_edu_no_dplm, c("cont_edu_no_dplm_count", "cont_edu_no_dplm_prop"), sep = " ") %>%
  separate(cont_edu_dplm, c("cont_edu_dplm_count", "cont_edu_dplm_prop"), sep = " ") %>%
  mutate(across(contains("count"),
                function(x) round(as.numeric(gsub(",", "", x))/n_imp))) %>%
  mutate(`No continuing edu` = paste0(no_cont_edu_count, " (", no_cont_edu_prop, ")"),
         `Continuing edu w/o a diploma` = paste0(cont_edu_no_dplm_count, " (", cont_edu_no_dplm_prop, ")"),
         `Continuing edu w a diploma` = paste0(cont_edu_dplm_count, " (", cont_edu_dplm_prop, ")")) %>%
  select(-contains("_")) %>%
  set_colnames(c("Characteristic", 
                 paste0(temp_cat_weighted$df_by$by_chr,
                        ",\n N=",
                        round(temp_cat_weighted$df_by$n_unweighted/n_imp),
                        "\n Weighted N=",
                        round(temp_cat_weighted$df_by$n/n_imp))))
table1_cat_weighted[table1_cat_weighted == "NA (NA)"] <- NA

##---- continuous ----
# Every continuous variable was imputed.
charls_imp_svy <- labelled_data %>%
  survey::svydesign(~ 1, data = ., weights = ~r1wtrespb)
temp_cont_tib <- data.frame()
for (i in 1:n_imp){
  temp_cont_svy <- labelled_data %>%
    filter(imp == i) %>%
    select(r1agey_v3, chldstate_30, continuing_edu, r1wtrespb) %>%
    survey::svydesign(~1, data = ., weights = ~r1wtrespb)
  
  temp_cont_tib %<>% rbind(cbind(
    as.data.frame(svyby(~ r1agey_v3, ~ continuing_edu, 
                        temp_cont_svy, svyvar)["r1agey_v3"]),
    as.data.frame(svyby(~ chldstate_30, ~ continuing_edu, 
                        temp_cont_svy, svyvar)["chldstate_30"])) %>%
      mutate(imp = i) %>%
      rownames_to_column("continuing_edu"))
}

temp_cont_var <- temp_cont_tib %>%
  group_by(continuing_edu) %>%
  dplyr::summarize(var_r1agey_v3 = mean(r1agey_v3),
                   var_chldstate_30 = mean(chldstate_30)) %>%
  as.data.frame()

r1agey_v3_svyvar <- temp_cont_var %>% select(var_r1agey_v3) %>% pull()
r1agey_v3_svymean <- svyby(~r1agey_v3, ~continuing_edu, 
                           charls_imp_svy, svymean)[["r1agey_v3"]]
chldstate_30_svyvar <- temp_cont_var %>% select(var_chldstate_30) %>% pull()
chldstate_30_svymean <- svyby(~chldstate_30, ~continuing_edu, 
                              charls_imp_svy, svymean)[["chldstate_30"]]

table1_cont_weighted <- tibble() %>%
  rbind(c(var_label(labelled_data$r1agey_v3),
          paste0(round(r1agey_v3_svymean, 1), " (", 
                 round(sqrt(r1agey_v3_svyvar), 1), ")")),
        c(var_label(labelled_data$chldstate_30),
          paste0(round(chldstate_30_svymean, 1), " (", 
                 round(sqrt(chldstate_30_svyvar), 1), ")"))) %>%
  set_colnames(c("Characteristic", 
                 paste0(temp_cat_weighted$df_by$by_chr,
                        ",\n N=",
                        round(temp_cat_weighted$df_by$n_unweighted/n_imp),
                        "\n Weighted N=",
                        round(temp_cat_weighted$df_by$n/n_imp))))

##---- Full table 1 ----
table1_weighted <- rbind(table1_cont_weighted[1, ], # r1age
                         table1_cat_weighted,
                         table1_cont_weighted[2, ]) # chldstate

#---- Save the output ----
writexl::write_xlsx(table1_weighted, 
                    here::here("output", "tables", "table1_cont_edu_pred_imp_weighted.xlsx"))



# #---- Complete case analysis ----
# #---- Table 1 ----
# table_1 <- charls_selected %>% 
#   filter(r1agey_v3 >= 45) %>%
#   select(r1agey_v3, continuing_edu,
#          # Early life predictors
#          rabyear_v2, female_v2,
#          first_child_v2, 
#          rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
#          childhood_1st_resi, childhood_1st_region, rarural_child, 
#          first_hukou, 
#          # Early-adulthood predictors
#          childhood_edu, occupation_30_5cat, marital_30, hukou_30, rural_30, 
#          chldstate_30) %>%
#   labelled::set_variable_labels(
#     r1agey_v3 = "Baseline age at 2011 in years",
#     continuing_edu = "Continuing education level (%)",
#     rabyear_v2 = "Birth year",
#     female_v2 = "Female (%)",
#     first_child_v2 = "Birth order: first child (%)",
#     rameduc_c = "Mother's education level (%)", 
#     rafeduc_c = "Father's education level (%)", 
#     ramoccup_c = "Mother's occupation before age 17 (%)", 
#     rafoccup_c = "Father's occupation before age 17 (%)",
#     childhood_1st_resi = "Childhood first residence (%)", 
#     childhood_1st_region = "Childhood first region (%)",
#     rarural_child = "Childhood rural residence (%)",
#     first_hukou = "First hukou status (%)", 
#     # Early-adulthood predictors
#     childhood_edu = "Early life education level (%)",
#     occupation_30_5cat = "Occupation at age 30 (%)",
#     marital_30 = "Marital status at age 30 (%)",
#     rural_30 = "Rural residence at age 30 (%)",
#     hukou_30 = "Hukou status at age 30 (%)",
#     chldstate_30 = "Number of Children at age 30 (%)") %>%
#   labelled::drop_unused_value_labels() %>%
#   labelled::set_value_labels(
#     continuing_edu = c("No continuing edu" = 0, "Continuing edu w/o a diploma" = 1,
#                        "Continuing edu w/ a diploma" = 2),
#     female_v2 = c("Yes" = 1, "No" = 0),
#     # first_child_v2 = c("Yes" = 1, "No" = 0),
#     rameduc_c = c("No formal education (illiterate)" = 0,
#                   "Elementary school or less" = 1,
#                   "Middle school" = 2,
#                   "College and above" = 3),
#     rafeduc_c = c("No formal education (illiterate)" = 0,
#                   "Elementary school or less" = 1,
#                   "Middle school" = 2,
#                   "College and above" = 3),
#     ramoccup_c = c("Agricultural" = 1, "Non-agricultural" = 0),
#     rafoccup_c = c("Agricultural" = 1, "Non-agricultural" = 0),
#     childhood_1st_region = c("East" = 1,
#                              "Middle" = 2,
#                              "West" = 3,
#                              "Northeast" = 4),
#     # rarural_child = c("Yes" = 1, "No" = 0),
#     first_hukou = c("Agricultural hukou" = 1, "Non-agricultural hukou" = 2,
#                     "Unified residence hukou" = 3, "Do not have hukou" = 4),
#     childhood_edu = c("No formal education (illiterate)" = 0,
#                       "Elementary school or less" = 1,
#                       "Middle school" = 2,
#                       "College and above" = 3),
#     occupation_30_5cat = c("Agriculture related" = 1,
#                            "Non-agriculture related" = 2,
#                            "Unemployed" = 3,
#                            "Retired" = 4,
#                            "Never work" = 5),
#     # marital_30 = c("Yes" = 1, "No" = 0),
#     hukou_30 = c("Agricultural hukou" = 1, "Non-agricultural hukou" = 2,
#                  "Unified residence hukou" = 3, "Do not have hukou" = 4),
#     # rural_30 = c("Yes" = 1, "No" = 0)
#   ) %>%
#   modify_if(is.labelled, to_factor) %>%
#   modify_if(is.factor, 
#             function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
#   tbl_summary(
#     statistic = list(all_categorical() ~ "{n} ({p}%)",
#                      all_continuous() ~ c("{mean} ({sd})",
#                                           "({min}, {max})")),
#     type = all_continuous() ~ "continuous2",
#     digits = list(all_continuous() ~ 1,
#                   all_categorical() ~ c(0, 1)),
#     by = continuing_edu,
#     missing = "ifany",
#     missing_text = "Missing") %>% 
#   # modify_spanning_header(starts_with("stat_") ~ "**Baseline CES-D**") %>%
#   bold_labels() %>%
#   modify_footnote(
#     update = list(
#       starts_with("stat_") ~
#         "Mean (SD) for continuous variables; n (%) for categorical variables")
#   )
# 
# table_1 %>% as_flex_table()
# 
# #---- **save the table ----
# table_1 %>% as_flex_table() %>% flextable::save_as_docx(
#   path = here::here("output", "tables", "table1_continuing_pred_cc.docx"))
