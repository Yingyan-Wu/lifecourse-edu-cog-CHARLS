# Covariate balance plot by continuing education
# Created by Yingyan Wu
# Apr.11.2024

#---- Package loading + preparation ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "twang", "spatstat", "survey")
options(scipen = 999)

#---- Load the data ----
# load(here::here("data", "analysis_data", "charls_selected_pred_cont_cc.RData"))
imp_pmm_40 <- readRDS(here::here("data", "analysis_data", "imp_pmm_40_stacked.RDS"))
imp_pmm_40 %<>%
  dplyr::mutate(occupation_30_5cat = as.factor(case_when(occupation_30 %in% c(1, 2) ~ 1,
                                        occupation_30 %in% c(3, 4, 5) ~ 2,
                                        occupation_30 == 6 ~ 3,
                                        occupation_30 == 7 ~ 4,
                                        occupation_30 == 8 ~ 5)),
         unweight = 1,
         cont_edu_2cat = case_when(continuing_edu >= 1 ~ 1,
                                         continuing_edu == 0 ~ 0),
         cont_edu_dplm_rest = case_when(continuing_edu == 2 ~ 1,
                                        continuing_edu <= 1 ~ 0),
         cont_edu_dplm_nocont = case_when(continuing_edu == 2 ~ 1,
                                          continuing_edu == 0 ~ 0),
         cont_edu_dplm_nodplm = case_when(continuing_edu == 2 ~ 1,
                                          continuing_edu == 1 ~ 0),
         cont_edu_nodplm_nocont = case_when(continuing_edu == 1 ~ 1,
                                          continuing_edu == 0 ~ 0),
         first_hukou = as.factor(first_hukou),
         across(c("female_v2", "first_child_v2", "rarural_child",
                  "marital_30", "rural_30", 
                  "rafoccup_c", "ramoccup_c"), 
                function(x) as.numeric(as.character(x))))

imp_40_forcovbal <- imp_pmm_40 %>%
  select(r1agey_v3, contains("cont_edu"), unweight, r1wtrespb,
         # Early life predictors
         female_v2,
         first_child_v2, 
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
         # childhood_1st_resi, 
         childhood_1st_region, rarural_child, 
         first_hukou, 
         # Early-adulthood predictors
         childhood_edu, occupation_30_5cat, marital_30, hukou_30, rural_30, 
         chldstate_30) %>%
  as.data.frame()

selected_vars <- imp_40_forcovbal %>% 
  select(-contains("cont_edu"), -unweight, -r1wtrespb) %>% colnames()

#---- Covariate balance ----
checkcovbal <- function(data, weightvar, treatvar){
  # data = imp_40_forcovbal
  # weightvar = "unweight"
  # treatvar = "cont_edu_2cat"
  temp <- twang::bal.stat(data = data, 
                          vars = selected_vars,
                          w.all = data[, weightvar],
                          treat.var = treatvar, # 1 = continuing edu
                          sampw = data$unweight, 
                          estimand = "ATT", 
                          na.action = "exclude",
                          multinom = F)
  
  temp[[1]][, "var"] <- rownames(temp[[1]])
  covbal_toplot <- do.call("rbind", temp)
  
  # Label the variables
  covbal_toplot %<>%
    # filter(var %in% order_var) %>%
    arrange(desc(std.eff.sz)) %>%
    mutate(label = case_when(
      var == "r1agey_v3" ~ "Age at Baseline in years",
      var == "continuing_edu:0" ~ "No continuing edu",
      var == "continuing_edu:1" ~ "Continuing edu w/o a diploma",
      var == "continuing_edu:2" ~ "Continuing edu w/ a diploma",
      var == "rameduc_c:0" ~ "Mom: No formal education (illiterate)",
      var == "rameduc_c:1" ~ "Mom: Elementary school or less",
      var == "rameduc_c:2" ~ "Mom: Middle school",
      var == "rameduc_c:3" ~ "Mom: College and above",
      var == "rafeduc_c:0" ~ "Dad: No formal education (illiterate)",
      var == "rafeduc_c:1" ~ "Dad: Elementary school or less",
      var == "rafeduc_c:2" ~ "Dad: Middle school",
      var == "rafeduc_c:3" ~ "Dad: College and above",
      var == "ramoccup_c" ~ "Mom: Agricultural occupation",
      var == "rafoccup_c" ~ "Dad: Agricultural occupation",
      var == "childhood_1st_region:1" ~ "Childhood region: East",
      var == "childhood_1st_region:2" ~ "Childhood region: Middle",
      var == "childhood_1st_region:3" ~ "Childhood region: West",
      var == "childhood_1st_region:4" ~ "Childhood region: Northeast",
      var == "first_hukou:1" ~ "First hukou: Agricultural hukou",
      var == "first_hukou:2" ~ "First hukou: Non-agricultural hukou",
      var == "first_hukou:3" ~ "First hukou: Unified residence hukou",
      var == "first_hukou:4" ~ "First hukou: Do not have hukou",
      var == "childhood_edu:0" ~ "Early life edu: No formal education (illiterate)",
      var == "childhood_edu:1" ~ "Early life edu: Elementary school or less",
      var == "childhood_edu:2" ~ "Early life edu: Middle school",
      var == "childhood_edu:3" ~ "Early life edu: College and above",
      var == "occupation_30_5cat:1" ~ "Occupation at 30: Agriculture related",
      var == "occupation_30_5cat:2" ~ "Occupation at 30: Non-agriculture related",
      var == "occupation_30_5cat:3" ~ "Occupation at 30: Unemployed",
      var == "occupation_30_5cat:4" ~ "Occupation at 30: Retired",
      var == "occupation_30_5cat:5" ~ "Occupation at 30: Never work",
      var == "hukou_30:1" ~ "Hukou status at 30: Agricultural hukou",
      var == "hukou_30:2" ~ "Hukou status at 30: Non-agricultural hukou",
      var == "hukou_30:3" ~ "Hukou status at 30: Unified residence hukou",
      var == "hukou_30:4" ~ "Hukou status at 30: Do not have hukou",
      var == "first_child_v2" ~ "Birth order: First child",
      var == "marital_30" ~ "Married at age 30",
      var == "chldstate_30" ~ "Number of children at age 30",
      var == "occupation_30_5cat" ~ "Occupation at age 30",
      var == "first_hukou" ~ "First hukou status",
      var == "female_v2" ~ "Female",
      var == "rarural_child" ~ "Child Rural Residence",
      var == "rural_30" ~ "Rural Residence at age 30"))
  covbal_toplot %<>%
    mutate(var = factor(var, levels = rev(var))) %>%
    # mutate(var = factor(var, levels = rev(order_var))) %>%
    arrange(desc(var))
  covbal_toplot$label <- factor(covbal_toplot$label, 
                                levels = rev(covbal_toplot$label))
  
  # Generate data to plot covariate balance
  covbal_plot <- covbal_toplot %>%
    # ggplot(aes(x = std.eff.sz, y = var)) +
    ggplot(aes(x = std.eff.sz, y = label)) +
    geom_point(size = 2, color = "#0072B2") +
    geom_vline(xintercept = 0) +
    xlab("Standardized mean diff Mean(Cont edu) - Mean(No cont edu)/SD(No cont edu)") +
    theme_bw() +
    # geom_vline(xintercept = c(-0.2, 0.2), linetype = "dashed",
    #            color = "dark grey") +
    # sclae_x_continuous
    theme(aspect.ratio = 5/3, panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "none")
  
  return(list("covbal_data" = covbal_toplot, "covbal_plot" = covbal_plot))
}

#---- Covariate balance plot ----
order_var <- 
  c("childhood_edu:3", "childhood_edu:2", "childhood_edu:1", "childhood_edu:0", 
    "hukou_30:2" , "hukou_30:3", "hukou_30:1",
    "occupation_30_5cat:5",
    "occupation_30_5cat:4", "occupation_30_5cat:3", "occupation_30_5cat:2",
    "occupation_30_5cat:1",
    "rafoccup_c", "ramoccup_c", 
    "first_hukou:1", "first_hukou:2", "first_hukou:3", "first_hukou:4",
    "r1agey_v3", 
    "rameduc_c:2", "childhood_1st_region:1",
    "rafeduc_c:2", "rafeduc_c:3", "first_child_v2", "rameduc_c:1", 
    "rafeduc_c:1", "rameduc_c:3", "childhood_1st_region:4",
    "marital_30", 
    "childhood_1st_region:2", "childhood_1st_region:3", 
    "chldstate_30", "female_v2" , "rameduc_c:0", "rafeduc_c:0",
    "rarural_child",  "rural_30")

##---- any vs. no ----
any_no_unwtd_covbal <- 
  checkcovbal(imp_40_forcovbal, "unweight", "cont_edu_2cat")

any_no_wtd_covbal <- 
  checkcovbal(imp_40_forcovbal, "r1wtrespb", "cont_edu_2cat")

##---- diploma vs. rest ----
dplm_rest_unwtd_covbal <- 
  checkcovbal(imp_40_forcovbal, "unweight", "cont_edu_dplm_rest")

dplm_rest_wtd_covbal <- 
  checkcovbal(imp_40_forcovbal, "r1wtrespb", "cont_edu_dplm_rest")

##---- diploma vs. no diploma ----
dplm_nodplm_unwtd_covbal <- 
  checkcovbal(imp_40_forcovbal %>% filter(!is.na(cont_edu_dplm_nodplm)), 
              "unweight", "cont_edu_dplm_nodplm")

dplm_nodplm_wtd_covbal <- 
  checkcovbal(imp_40_forcovbal %>% filter(!is.na(cont_edu_dplm_nodplm)), 
              "r1wtrespb", "cont_edu_dplm_nodplm")

##---- no diploma vs. no cont edu ----
nodplm_nocont_unwtd_covbal <- 
  checkcovbal(imp_40_forcovbal %>% filter(!is.na(cont_edu_nodplm_nocont)), 
              "unweight", "cont_edu_nodplm_nocont")

nodplm_nocont_wtd_covbal <- 
  checkcovbal(imp_40_forcovbal %>% filter(!is.na(cont_edu_nodplm_nocont)), 
              "r1wtrespb", "cont_edu_nodplm_nocont")

#---- Save the covbal data ----
save(any_no_unwtd_covbal, any_no_wtd_covbal, 
     dplm_rest_unwtd_covbal, dplm_rest_wtd_covbal,
     dplm_nodplm_unwtd_covbal, dplm_nodplm_wtd_covbal,
     nodplm_nocont_unwtd_covbal, nodplm_nocont_wtd_covbal,
     file = here::here("data", "analysis_data", "cont_pred_covbal.RData"))

