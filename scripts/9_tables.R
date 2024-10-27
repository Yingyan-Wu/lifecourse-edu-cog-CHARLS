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
imp_pmm_40 <- readRDS(here::here("data", "analysis_data", "imp_pmm_40_stacked.RDS"))
imp_pmm_40 %<>%
  mutate(occupation_30_5cat = case_when(occupation_30 %in% c(1, 2) ~ 1,
                                        occupation_30 %in% c(3, 4, 5) ~ 2,
                                        occupation_30 == 6 ~ 3,
                                        occupation_30 == 7 ~ 4,
                                        occupation_30 == 8 ~ 5),
         edu_cat =  case_when(
           childhood_edu == 0 & continuing_edu == 0 ~ 0,
           childhood_edu == 0 & continuing_edu == 1 ~ 1,
           childhood_edu == 0 & continuing_edu == 2 ~ 2,
           childhood_edu == 1 & continuing_edu == 0 ~ 3,
           childhood_edu == 1 & continuing_edu == 1 ~ 4,
           childhood_edu == 1 & continuing_edu == 2 ~ 5,
           childhood_edu == 2 & continuing_edu == 0 ~ 6,
           childhood_edu == 2 & continuing_edu == 1 ~ 7,
           childhood_edu == 2 & continuing_edu == 2 ~ 8,
           childhood_edu == 3 & continuing_edu == 0 ~ 9,
           childhood_edu == 3 & continuing_edu == 1 ~ 10,
           childhood_edu == 3 & continuing_edu == 2 ~ 11))

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
                                 "Unified residence hukou", "Do not have hukou")),
    edu_cat = factor(edu_cat, levels = 0:11,
                     labels = c("No Formal Education (Illiterate)",
                                "No formal education + continuing education without a diploma",
                                "No formal education + continuing education with diploma",
                                "Elementary school or less + no continuing education",
                                "Elementary school or less + continuing education without a diploma",
                                "Elementary school or less + continuing education with a diploma",
                                "Vocational school/middle school/high school + no continuing education",
                                "Vocational school/middle school/high school + continuing education without a diploma",
                                "Vocational school/middle school/high school + continuing education with a diploma",
                                "College & above + no continuing education",
                                "College & above + continuing education without a diploma",
                                "College & above + continuing education with a diploma"))
  ) %>%
  labelled::set_variable_labels(
    r1agey_v3 = "Baseline age at 2011 in years",
    continuing_edu = "Continuing education level (%)",
    edu_cat = "Education level (%)",
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
    chldstate_30 = "Number of Children at age 30",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall")


##---- categorical ----
cat_imp_svy <- labelled_data %>%
  select(# Early life predictors
         female_v2,
         edu_cat,
         first_child_v2, 
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
         childhood_1st_region, rarural_child, 
         first_hukou, 
         # Early-adulthood predictors
        occupation_30_5cat, marital_30, hukou_30, rural_30,
         r1wtrespb) %>%
  survey::svydesign(~ 1, data = ., weights = ~r1wtrespb)

temp_cat_weighted <- tbl_svysummary(
  data = cat_imp_svy,
  statistic = list(all_categorical() ~ c("{n_unweighted} {p}")), # For latter splitting
  digits = all_categorical() ~ 1,
  missing = "no"
)

temp_cat_weighted %>% as_flex_table()

table1_cat_weighted <- temp_cat_weighted %>% as_tibble() %>%
  set_colnames(c("Characteristic", "total")) %>%
  filter(row_number() <= n() - 1) %>% # remove the last row in the table of median IQR for weights.
  separate(total, c("total_count", "total_prop"), sep = " ") %>%
  dplyr::mutate(across(contains("count"),
                function(x) round(as.numeric(gsub(",", "", x))/n_imp))) %>%
  mutate(`total` = paste0(total_count, " (", total_prop, ")")) %>%
  dplyr::select(-contains("_")) %>%
  set_colnames(c("Characteristic", 
                 paste0("N=", round(temp_cat_weighted$N/n_imp),
                        "\n Weighted N=", 
                        round(temp_cat_weighted$table_styling$header$modify_stat_N[1]/n_imp))))
table1_cat_weighted[table1_cat_weighted == "NA (NA)"] <- NA

##---- continuous ----
# Every continuous variable was imputed.
charls_imp_svy <- labelled_data %>%
  survey::svydesign(~ 1, data = ., weights = ~r1wtrespb)
temp_cont_tib <- data.frame()
for (i in 1:n_imp){
  temp_cont_svy <- labelled_data %>%
    filter(imp == i) %>%
    select(r1agey_v3, chldstate_30, r1imrc, r1dlrc, r1wtrespb) %>%
    survey::svydesign(~1, data = ., weights = ~r1wtrespb)
  
  temp_cont_tib %<>% rbind(cbind(
    as.data.frame(svyvar(~r1agey_v3, temp_cont_svy, na.rm = T)["r1agey_v3"]),
    as.data.frame(svyvar(~chldstate_30, temp_cont_svy, na.rm = T)["chldstate_30"]),
    as.data.frame(svyvar(~r1imrc, temp_cont_svy, na.rm = T)["r1imrc"]),
    as.data.frame(svyvar(~r1dlrc, temp_cont_svy, na.rm = T)["r1dlrc"])) %>%
      mutate(imp = i))
}

temp_cont_var <- temp_cont_tib %>%
  set_colnames(c("r1agey_v3", "chldstate_30", "r1imrc", "r1dlrc", "imp")) %>%
  dplyr::summarize(var_r1agey_v3 = mean(r1agey_v3),
                   var_chldstate_30 = mean(chldstate_30),
                   var_r1imrc = mean(r1imrc),
                   var_r1dlrc = mean(r1dlrc)) %>%
  as.data.frame()

r1agey_v3_svyvar <- temp_cont_var %>% select(var_r1agey_v3) %>% pull()
r1agey_v3_svymean <- svymean(~r1agey_v3, charls_imp_svy)[["r1agey_v3"]]
chldstate_30_svyvar <- temp_cont_var %>% select(var_chldstate_30) %>% pull()
chldstate_30_svymean <- svymean(~chldstate_30, charls_imp_svy)[["chldstate_30"]]
r1imrc_svyvar <- temp_cont_var %>% select(var_r1imrc) %>% pull()
r1imrc_svymean <- svymean(~r1imrc, charls_imp_svy, na.rm = T)[["r1imrc"]]
r1dlrc_svyvar <- temp_cont_var %>% select(var_r1dlrc) %>% pull()
r1dlrc_svymean <- svymean(~r1dlrc, charls_imp_svy, na.rm = T)[["r1dlrc"]]

table1_cont_weighted <- tibble() %>%
  rbind(c(var_label(labelled_data$r1agey_v3),
          paste0(round(r1agey_v3_svymean, 1), " (", 
                 round(sqrt(r1agey_v3_svyvar), 1), ")")),
        c(var_label(labelled_data$chldstate_30),
          paste0(round(chldstate_30_svymean, 1), " (", 
                 round(sqrt(chldstate_30_svyvar), 1), ")")),
        c(var_label(labelled_data$r1imrc),
          paste0(round(r1imrc_svymean, 1), " (",
                 round(sqrt(r1imrc_svyvar), 1), ")")),
        c(var_label(labelled_data$r1dlrc),
          paste0(round(r1dlrc_svymean, 1), " (",
                 round(sqrt(r1dlrc_svyvar), 1), ")"))) %>%
  set_colnames(c("Characteristic", 
                 paste0("N=", round(temp_cat_weighted$N/n_imp),
                        "\n Weighted N=", 
                        round(temp_cat_weighted$table_styling$header$modify_stat_N[1]/n_imp))))

##---- Full table 1 ----
table1_weighted <- rbind(table1_cont_weighted[1, ], # r1age
                         table1_cat_weighted,
                         table1_cont_weighted[2:4, ]# chldstate, # r1imrc, r1dlrc
                        ) 

#---- Save the output ----
writexl::write_xlsx(table1_weighted, 
                    here::here("output", "tables", "table1_imp_weighted.xlsx"))
