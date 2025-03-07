# CHARLS_data_table_1_MIed
# Created by: Yingyan Wu
# Mar.12.2024

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
load(here::here("data", "analysis_data", "imp_pmm_20_wide_long.RData"))

imp_pmm_stacked %<>%
  mutate(occupation_30_5cat = case_when(occupation_30 %in% c(1, 2) ~ 1,
                                        occupation_30 %in% c(3, 4, 5) ~ 2,
                                        occupation_30 == 6 ~ 3,
                                        occupation_30 == 7 ~ 4,
                                        occupation_30 == 8 ~ 5),
         edu_cat =  case_when(
           childhood_edu == 0 & continuing_edu_2cat == 0 ~ 0,
           childhood_edu == 0 & continuing_edu_2cat == 1 ~ 1,
           childhood_edu == 1 & continuing_edu_2cat == 0 ~ 2,
           childhood_edu == 1 & continuing_edu_2cat == 1 ~ 3,
           childhood_edu == 2 & continuing_edu_2cat == 0 ~ 4,
           childhood_edu == 2 & continuing_edu_2cat == 1 ~ 5,
           childhood_edu == 3 & continuing_edu_2cat == 0 ~ 6,
           childhood_edu == 3 & continuing_edu_2cat == 1 ~ 7))

#---- Labelled data for Table 1 ----
labelled_data <- imp_pmm_stacked %>%
  dplyr::mutate(across(c("female", "first_child_v2", "rarural_child",
                         # "continuing_edu_2cat",
                         "marital_30", "rural_30", 
                         "n1_iadl_2cat",  "n1_hibpe", "n1_stroke", 
                         "n1_hearte", "n1_cancre", "n1_diabe"),
                       function(x) factor(x, levels = c(1, 0),
                                          labels = c("Yes", "No")))) %>%
  dplyr::mutate(
    continuing_edu_2cat = factor(continuing_edu_2cat, levels = c(1, 0),
                                 labels = c("Adulthood education", "No adulthood education")),
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
    ramoccup_c = factor(ramoccup_c, levels = c(1, 0),
                        labels = c("Non-agricultural", "Agricultural")),
    rafoccup_c = factor(rafoccup_c, levels = c(1, 0),
                        labels = c("Non-agricultural", "Agricultural")),
    childhood_1st_region = factor(childhood_1st_region, levels = c(1, 2, 3, 4),
                                  labels = c("East", "Middle", "West", "Northeast")),
    first_hukou = factor(first_hukou, levels = c(1, 2, 3),
                         labels = c("Agricultural hukou", "Non-agricultural hukou",
                                    "Unified residence hukou")),
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
    hukou_30 = factor(hukou_30, levels = c(1, 2, 3),
                      labels = c("Agricultural hukou", "Non-agricultural hukou",
                                 "Unified residence hukou")),
    edu_cat = factor(edu_cat, levels = 0:7,
                     labels = c("No Formal Education (Illiterate)",
                                "Only adulthood education",
                                "Elementary school or less only",
                                "Elementary school or less + adulthood education",
                                "Middle school level only",
                                "Middle school level + adulthood education",
                                "College and above only",
                                "College and above + adulthood education"))) %>%
  labelled::set_variable_labels(
    n1_agey_v3 = "Baseline age (first participated wave) in years",
    continuing_edu_2cat = "Adulthood education",
    edu_cat = "Education level (%)",
    # rabyear_v2 = "Birth year",
    female = "Female (%)",
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
    chldstate_30 = "Number of Children at age 30",
    # Baseline health vars
    n1_iadl_2cat = "IADL difficulty (%)",
    n1_cesd10_cleaned = "CESD-10 score",
    n1_hibpe = "Self-reported hypertension (%)",
    n1_stroke = "Self-reported stroke (%)",
    n1_hearte = "Self-reported heart disease (%)",
    n1_cancre = "Self-reported cancer (%)",
    n1_diabe = "Self-reported diabetes (%)",
    # Baseline cognitive vars
    n1_imrc = "Baseline Immediate word recall",
    n1_dlrc = "Baseline delayed word recall")

#---- Stratified table 1 ----
n_imp = 20
##---- categorical ----
cat_imp_svy <- labelled_data %>%
  select(continuing_edu_2cat,
         # Early life predictors
         female,
         childhood_edu,
         first_child_v2, 
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
         childhood_1st_region, rarural_child, 
         first_hukou, 
         # Early-adulthood predictors
         occupation_30_5cat, marital_30, hukou_30, rural_30,
         n1_iadl_2cat, n1_hibpe, n1_stroke, n1_hearte, n1_cancre, n1_diabe,
         # baseline weights
         n1_wtrespb_rscl) %>%
  survey::svydesign(~ 1, data = ., weights = ~n1_wtrespb_rscl)

temp_cat_weighted <- tbl_svysummary(
  data = cat_imp_svy,
  statistic = list(all_categorical() ~ c("{n_unweighted} {p}")), # For latter splitting
  digits = all_categorical() ~ 1,
  by = "continuing_edu_2cat",
  missing = "no") %>%
  add_overall() %>%
  bold_labels()

temp_cat_weighted %>% as_flex_table()

unwtd_n <- temp_cat_weighted$table_styling$header$modify_stat_n_unweighted[6:8]
wtd_n <- temp_cat_weighted$table_styling$header$modify_stat_n[6:8]
table1_headers <- c(
  "Characteristic", 
  paste0("Overall \nN=", prettyNum(round(unwtd_n[1]/n_imp), big.mark = ","),
         "\nWeighted N=",  prettyNum(round(wtd_n[1]/n_imp),
                                     big.mark = ",")),
  paste0("Adulthood education \nN=", prettyNum(round(unwtd_n[2]/n_imp), big.mark = ","),
         "\nWeighted N=", prettyNum(round(wtd_n[2]/n_imp), big.mark = ",")),
  paste0("No adulthood education \nN=", prettyNum(round(unwtd_n[3]/n_imp), big.mark = ","),
         "\nWeighted N=", prettyNum(round(wtd_n[3]/n_imp), big.mark = ",")))

table1_cat_weighted <- temp_cat_weighted %>% as_tibble() %>%
  set_colnames(c("Characteristic", "overall", "adult_edu", "no_adult_edu")) %>%
  filter(row_number() <= n() - 1) %>% # remove the last row in the table of median IQR for weights.
  separate(overall, c("overall_count", "overall_prop"), sep = " ") %>%
  separate(adult_edu, c("adltedu_count", "adltedu_prop"), sep = " ") %>%
  separate(no_adult_edu, c("no_adltedu_count", "no_adltedu_prop"), sep = " ") %>%
  dplyr::mutate(across(contains("count"),
                       function(x) prettyNum(round(as.numeric(gsub(",", "", x))/n_imp), 
                                             big.mark = ","))) %>%
  mutate(
    `Overall` = paste0(overall_count, " (", overall_prop, ")"),
    `Adulthood education` = paste0(adltedu_count, " (", adltedu_prop, ")"),
    `No adulthood education` = paste0(no_adltedu_count, " (", no_adltedu_prop, ")")) %>%
  dplyr::select(-contains("_")) %>%
  set_colnames(table1_headers)
table1_cat_weighted[table1_cat_weighted == "NA (NA)"] <- NA

##---- continuous ----
# Every continuous variable was imputed.
cont_vars <- c("n1_agey_v3", "chldstate_30", "n1_cesd10_cleaned",
               "n1_imrc", "n1_dlrc")

charls_imp_svy <- labelled_data %>%
  survey::svydesign(~ 1, data = ., weights = ~n1_wtrespb_rscl)
charls_imp_svy_adltedu <- subset(charls_imp_svy, 
                                 continuing_edu_2cat == "Adulthood education")
charls_imp_svy_no_adltedu <-subset(charls_imp_svy, 
                                   continuing_edu_2cat == "No adulthood education")
temp_cont_mean <- svymean(reformulate(cont_vars), charls_imp_svy, na.rm = T) %>% 
  as.data.frame() %>% select(-SE) %>% set_colnames("overall_mean") %>%
  cbind(svymean(reformulate(cont_vars), charls_imp_svy_adltedu, na.rm = T) %>% 
          as.data.frame() %>% select(-SE) %>% set_colnames("adlt_edu_mean"),
        svymean(reformulate(cont_vars), charls_imp_svy_no_adltedu, na.rm = T) %>% 
          as.data.frame() %>% select(-SE) %>% set_colnames("no_adlt_edu_mean"))

temp_cont_tib <- data.frame()
for (i in 1:n_imp){
  temp_cont_svy <- labelled_data %>%
    filter(imp == i) %>%
    select(all_of(cont_vars), continuing_edu_2cat, n1_wtrespb_rscl) %>%
    survey::svydesign(~1, data = ., weights = ~n1_wtrespb_rscl)
  temp_cont_svy_adltedu <- subset(temp_cont_svy, 
                                  continuing_edu_2cat == "Adulthood education")
  temp_cont_svy_no_adltedu <-subset(temp_cont_svy, 
                                    continuing_edu_2cat == "No adulthood education")
  temp_svyvar <- svyvar(reformulate(cont_vars), temp_cont_svy, na.rm = T) %>% as.data.frame() %>%
    select(-SE) %>% unique() %>% as.matrix() %>% diag() %>% t() %>% as_tibble() %>%
    set_colnames(cont_vars) %>% mutate(adlt_edu = "Overall") %>%
    rbind(svyvar(reformulate(cont_vars), temp_cont_svy_adltedu, na.rm = T) %>% 
            as.data.frame() %>%
            select(-SE) %>% unique() %>% as.matrix() %>% diag() %>% t() %>% as_tibble() %>%
            set_colnames(cont_vars) %>% mutate(adlt_edu = "Adult edu"),
          svyvar(reformulate(cont_vars), temp_cont_svy_no_adltedu, na.rm = T) %>% 
            as.data.frame() %>%
            select(-SE) %>% unique() %>% as.matrix() %>% diag() %>% t() %>% as_tibble() %>%
            set_colnames(cont_vars) %>% mutate(adlt_edu = "No adult edu"))
  
  temp_cont_tib %<>% rbind(temp_svyvar %>%
                             mutate(imp = i))
}

temp_cont_var <- temp_cont_tib %>%
  mutate(adlt_edu = factor(adlt_edu, 
                           levels = c("Overall", "Adult edu", "No adult edu"))) %>%
  group_by(adlt_edu) %>%
  dplyr::summarize_all(mean) %>% as.data.frame()

table1_cont_weighted <- temp_cont_mean %>%
  cbind(t(temp_cont_var %>% select(-imp, -adlt_edu )) %>%
          set_colnames(paste0(c("overall", "adlt_edu", "no_adlt_edu"), "_var"))) %>%
  mutate(Characteristic = var_label(labelled_data[, cont_vars]) %>% unlist(), 
         across(contains("var"), ~ sqrt(.x), 
                .names = "{str_replace(.col, '_var', '_sd')}"),
         overall_mean_sd = paste0(sprintf(overall_mean, fmt = '%#.1f'), " (", 
                                  sprintf(overall_sd, fmt = '%#.1f'), ")"),
         adult_edu_mean_sd = paste0(sprintf(adlt_edu_mean, fmt = '%#.1f'), " (", 
                                    sprintf(adlt_edu_sd, fmt = '%#.1f'), ")"),
         no_adult_edu_mean_sd = paste0(sprintf(no_adlt_edu_mean, fmt = '%#.1f'), " (", 
                                       sprintf(no_adlt_edu_sd, fmt = '%#.1f'), ")")) %>%
  select(Characteristic, contains("mean_sd")) %>%
  as_tibble() %>%
  set_colnames(table1_headers)

##---- Full table 1 ----
table1_weighted <- rbind(table1_cont_weighted[1, ], # Baseline age
                         table1_cat_weighted[1:47, ], # early-life and early-adulthood cat covariates
                         table1_cont_weighted[2, ], # chldstate_30
                         table1_cat_weighted[48:51, ], # baseline health cat covariates
                         table1_cont_weighted[3:5, ]# CESD, imrc, dlrc
) 

##---- Save the output ----
writexl::write_xlsx(table1_weighted, 
                    here::here("output", "tables", "table1_imp_weighted_stratified.xlsx"))

# #---- OLD ----
# #---- unstratified Table 1 ----
# n_imp = 20
# ##---- categorical ----
# cat_imp_svy <- labelled_data %>%
#   select(# Early life predictors
#     female,
#     edu_cat,
#     first_child_v2, 
#     rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
#     childhood_1st_region, rarural_child, 
#     first_hukou, 
#     # Early-adulthood predictors
#     occupation_30_5cat, marital_30, hukou_30, rural_30,
#     n1_iadl_2cat, n1_hibpe, n1_stroke, n1_hearte, n1_cancre, n1_diabe,
#     # baseline weights
#     n1_wtrespb_rscl) %>%
#   survey::svydesign(~ 1, data = ., weights = ~n1_wtrespb_rscl)
# 
# temp_cat_weighted <- tbl_svysummary(
#   data = cat_imp_svy,
#   statistic = list(all_categorical() ~ c("{n_unweighted} {p}")), # For latter splitting
#   digits = all_categorical() ~ 1,
#   missing = "no"
# )
# 
# temp_cat_weighted %>% as_flex_table()
# 
# table1_cat_weighted <- temp_cat_weighted %>% as_tibble() %>%
#   set_colnames(c("Characteristic", "total")) %>%
#   filter(row_number() <= n() - 1) %>% # remove the last row in the table of median IQR for weights.
#   separate(total, c("total_count", "total_prop"), sep = " ") %>%
#   dplyr::mutate(across(contains("count"),
#                        function(x) round(as.numeric(gsub(",", "", x))/n_imp))) %>%
#   mutate(`total` = paste0(total_count, " (", total_prop, ")")) %>%
#   dplyr::select(-contains("_")) %>%
#   set_colnames(c("Characteristic", 
#                  paste0("N=", round(temp_cat_weighted$table_styling$header$modify_stat_N_unweighted[1]/n_imp),
#                         "\n Weighted N=", 
#                         round(temp_cat_weighted$table_styling$header$modify_stat_N[1]/n_imp))))
# table1_cat_weighted[table1_cat_weighted == "NA (NA)"] <- NA
# 
# ##---- continuous ----
# # Every continuous variable was imputed.
# cont_vars <- c("n1_agey_v3", "chldstate_30", "n1_cesd10_cleaned",
#                "n1_imrc", "n1_dlrc")
# 
# charls_imp_svy <- labelled_data %>%
#   survey::svydesign(~ 1, data = ., weights = ~n1_wtrespb_rscl)
# temp_cont_mean <- svymean(reformulate(cont_vars), charls_imp_svy, na.rm = T) %>% 
#   as.data.frame() %>% select(-SE)
# 
# temp_cont_tib <- data.frame()
# for (i in 1:n_imp){
#   temp_cont_svy <- labelled_data %>%
#     filter(imp == i) %>%
#     select(all_of(cont_vars), n1_wtrespb_rscl) %>%
#     survey::svydesign(~1, data = ., weights = ~n1_wtrespb_rscl)
#   
#   temp_svyvar <- 
#     svyvar(reformulate(cont_vars), temp_cont_svy, na.rm = T) %>% as.data.frame() %>%
#     select(-SE) %>% unique() %>% as.matrix() %>% diag() %>% t() %>% as_tibble() %>%
#     set_colnames(cont_vars)
#   
#   temp_cont_tib %<>% rbind(temp_svyvar %>%
#                              mutate(imp = i))
# }
# 
# temp_cont_var <- temp_cont_tib %>%
#   dplyr::summarize_all(mean) %>% as.data.frame()
# 
# table1_cont_weighted <- temp_cont_mean %>%
#   cbind(t(temp_cont_var %>% select(-imp))) %>%
#   set_colnames(c("mean", "var")) %>%
#   mutate(Characteristic = var_label(labelled_data[, cont_vars]) %>% unlist(), 
#          sd = sqrt(var),
#          fmt_mean_sd = paste0(sprintf(mean, fmt = '%#.1f'), " (", 
#                               sprintf(sd, fmt = '%#.1f'), ")")) %>%
#   select(Characteristic, fmt_mean_sd) %>%
#   as_tibble() %>%
#   set_colnames(c("Characteristic", 
#                  paste0("N=", round(temp_cat_weighted$table_styling$header$modify_stat_N_unweighted[1]/n_imp),
#                         "\n Weighted N=", 
#                         round(temp_cat_weighted$table_styling$header$modify_stat_N[1]/n_imp))))
# 
# ##---- Full table 1 ----
# table1_weighted <- rbind(table1_cont_weighted[1, ], # Baseline age
#                          table1_cat_weighted[1:51, ], # early-life and early-adulthood cat covariates
#                          table1_cont_weighted[2, ], # chldstate_30
#                          table1_cat_weighted[52:57, ], # baseline health cat covariates
#                          table1_cont_weighted[3:5, ]# CESD, imrc, dlrc
# ) 
# 
# ##---- Save the output ----
# writexl::write_xlsx(table1_weighted, 
#                     here::here("output", "tables", "table1_imp_weighted.xlsx"))