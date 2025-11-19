# Multiple Imputation
# Created by: Yingyan Wu
# Dec.25.2022

#---- Package loading + preparation ----
#clear the console & environment
rm(list = ls())

# Packages
if (!require("pacman")){
  install.packages("pacman", repos = 'http://cran.us.r-project.org')
}
p_load("haven", "magrittr", "plyr", "tidyverse", "mice", "DataExplorer")

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the dataset ----
load(here::here("data", "analysis_data", "charls_selected.RData"))

#---- Check the missingness ----
colnames(charls_reframe_filtered)
# Drop those derived variables for the dataset to be imputed
charls_forimp <- charls_reframe_filtered %>%
  select(n1_agey_v3, female, childhood_edu, continuing_edu,
         first_child_v2, rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c,
         childhood_1st_region, rarural_child, first_hukou,
         chldstate_30, marital_30, rural_30, occupation_30, hukou_30,
         n1_wtrespb, starts_with("n1_"), -contains("iadl"),
         -c("n1_iwd_v2", "n1_fu_time", "n1_wave"))

DataExplorer::plot_missing(charls_forimp)
missing_summary <- DataExplorer::profile_missing(charls_forimp) %>%
  mutate(pct_missing = pct_missing*100) %>%
  arrange(pct_missing) %>%
  mutate(num_pct_missing = paste0(num_missing, " (", round(pct_missing, 1), "%)")) %>%
  mutate(feature_label = 
           case_when(feature == "n1_agey_v3" ~ "Baseline age (first participated wave) in years",
                     feature == "continuing_edu" ~ "Adulthood education",
                     feature == "childhood_edu" ~ "Early-life education",
                     feature == "female" ~ "Women",
                     feature == "first_child_v2" ~ "Birth order: first child",
                     feature == "rameduc_c" ~ "Mother's education attainment", 
                     feature == "rafeduc_c" ~ "Father's education attainment", 
                     feature == "ramoccup_c" ~ "Mother's occupation", 
                     feature == "rafoccup_c" ~ "Father's occupation",
                     feature == "childhood_1st_region" ~ "Childhood first region",
                     feature == "rarural_child" ~ "Childhood rural residence",
                     feature == "first_hukou" ~ "First hukou status", 
                     feature == "occupation_30" ~ "Occupation around age 30",
                     feature == "marital_30" ~ "Marital status around age 30",
                     feature == "rural_30" ~ "Rural residence around age 30",
                     feature == "hukou_30" ~ "Hukou status around age 30",
                     feature == "chldstate_30" ~ "Number of Children around age 30",
                     feature == "n1_wtrespb" ~ "Survey weight",
                     # Baseline health vars
                     feature == "n1_housewka" ~ "IADL: Doing work around the house or garden",
                     feature == "n1_mealsa" ~ "IADL: Meal preparation",
                     feature == "n1_medsa" ~ "IADL: Mediacation taking",
                     feature == "n1_shopa" ~ "IADL: Shopping for groceries",
                     feature == "n1_moneya" ~ "IADL: Finance management",
                     feature == "n1_cesd10_cleaned" ~ "CESD-10 score",
                     feature == "n1_hibpe" ~ "Self-reported hypertension",
                     feature == "n1_stroke" ~ "Self-reported stroke",
                     feature == "n1_hearte" ~ "Self-reported heart disease",
                     feature == "n1_cancre" ~ "Self-reported cancer",
                     feature == "n1_diabe" ~ "Self-reported diabetes",
                     feature == "n1_shlta" ~ "Self-rated health",
                     feature == "n1_socwk" ~ "Social activities",
                     # Baseline cognitive vars
                     feature == "n1_imrc" ~ "Baseline immediate word recall",
                     feature == "n1_dlrc" ~ "Baseline delayed word recall")) %>%
  select(feature, feature_label, num_pct_missing) %>%
  print(n = Inf)

writexl::write_xlsx(missing_summary, 
                    here::here("output", "tables", "missing_summary.xlsx"))

ordered_var_list <- paste0(missing_summary$feature)

#---- Data prep ----
# Running single imputation
# prep data by ordering by missingness
impute_data <- charls_forimp[, ordered_var_list] %>%
  as.data.frame() %>% print()
ncol(impute_data)
colnames(impute_data)
str(impute_data)

# Variable types
variable_length_mat <- impute_data %>%
  map_dfr(function(x) length(unique(x))) %>%
  pivot_longer(cols = everything(),
               names_to = "Var",
               values_to = "length")
table(variable_length_mat$length, useNA = "ifany")
# view(variable_length_mat %>% filter(length <= 30 & !Var %in% binary_vars))

binary_vars <- 
  variable_length_mat[variable_length_mat$length %in% c(2, 3), "Var"][["Var"]]

# Comment this out to treat them all as continuous variables
ordinal_vars <- c("childhood_edu", "continuing_edu", "rameduc_c", "rafeduc_c",
                  "n1_shlta")

cat_vars <- c("hukou_30", "occupation_30", "childhood_1st_region", 
              "first_hukou")

(cont_vars <- variable_length_mat %>%
    filter(!Var %in% c(binary_vars, ordinal_vars, cat_vars)) %>% pull(Var))

# Check
length(c(cont_vars, binary_vars, cat_vars, ordinal_vars))
ncol(impute_data)
sum(duplicated(c(cont_vars, binary_vars, cat_vars, ordinal_vars)))

# Check types
impute_data %>%
  map_dfc(function(x) class(x)[1]) %>%
  t() %>% print()

# Set classes by type
impute_data %<>%
  mutate(across(all_of(binary_vars), \(x) as.factor(x)),
         across(all_of(ordinal_vars), \(x) factor(x, ordered = T)),
         across(all_of(cat_vars), \(x) as.factor(x)),
         across(all_of(cont_vars), \(x) as.numeric(x)))

#---- Initiate imputation (PMM) ----
ini_imp <- mice(impute_data, maxit = 0,
                defaultMethod = c("pmm", "pmm", "pmm", "pmm"),
                seed = 62283)
# ini_imp$loggedEvents
(meth <- ini_imp$method)
(pred <- ini_imp$predictorMatrix)

#---- Run imputation ----
tictoc::tic()
imp_pmm_20 <- mice(impute_data, m = 20, maxit = 60, 
                   # increase iterations since some of the variables did not converge at 10 iterations
                   pred = pred, meth = meth,
                   n.burn = 10,
                   # burn in first 10 iterations
                   defaultMethod = c("pmm", "pmm", "pmm", "pmm"),
                   seed = 62283)
tictoc::toc()
# 60 iterations 10 imp: 13min
# 80 iterations 10 imp: 17min
# 100 iterations 10 imp: 23min
# 60 iterations 20 imp: 87.6 min

imp_pmm_20$loggedEvents # null
plot(imp_pmm_20)
# Compared to 40 imputation version (30 iterations). It did not make too much difference.
# stick with 20 imputations 60 iterations.
densityplot(imp_pmm_20)

save(imp_pmm_20, file = here::here("data", "analysis_data", "imp_pmm_20_refradt.RData"))

#---- Stack imputed dataset ----
load(here::here("data", "analysis_data", "imp_pmm_20_refradt.RData"))
imp_num = 20
imp_temp <- list()
for (i in 1:imp_num){
  imp_temp[[i]] <- complete(get(paste0("imp_pmm_", imp_num)),
                            action = i)
  imp_temp[[i]] <- cbind(
    charls_reframe_filtered %>%
      select(ID, paste0("n", 2:5, "_agey_v3"),
             paste0("n", 2:5, "_wtrespb"),
             paste0("n", 1:5, "_wave"),
             paste0("n", 1:5, "_iwd_v2"),
             paste0("n", 1:5, "_fu_time"),
             paste0("n", 2:5, "_imrc"),
             paste0("n", 2:5, "_dlrc"),
             contains(paste0("n", 2:5, "_"))),
    imp_temp[[i]][, colnames(impute_data)])
  imp_temp[[i]][, "imp"] <- i
}
imp_pmm_stacked <- do.call(rbind, imp_temp)

colnames(imp_pmm_stacked)

# Sanity check
imp_pmm_stacked %>%
  select(all_of(binary_vars)) %>%
  apply(., 2, function(x) table(x, useNA = "ifany"))
imp_pmm_stacked %>%
  select(all_of(cat_vars)) %>%
  apply(., 2, function(x) table(x, useNA = "ifany"))
imp_pmm_stacked %>%
  select(all_of(ordinal_vars)) %>%
  apply(., 2, function(x) table(x, useNA = "ifany"))
imp_pmm_stacked %>%
  select(all_of(cont_vars)) %>%
  apply(., 2, summary)
impute_data %>%
  select(all_of(cont_vars)) %>%
  apply(., 2, summary)
imp_pmm_stacked %>%
  map_dfr(class) %>% t()

#---- Save imputed data ----
saveRDS(imp_pmm_stacked, here::here("data", "analysis_data", "imp_pmm_20_stacked.RDS"))

#---- OLD ----
# # tictoc::tic()
# imp_pmm_40 <- mice(impute_data, m = 40, maxit = 30,
#                    pred = pred, meth = meth,
#                    n.burn = 10,
#                    defaultMethod = c("pmm", "pmm", "pmm", "pmm"),
#                    seed = 62283)
# # tictoc::toc()
# 
# imp_pmm_40$loggedEvents %>% view()
# # all events are related to weights (wave 4 weights ~ wave 3 weights)
# plot(imp_pmm_40)
# # Thank god it's fine but the burnin argument seems not doing anything.
# densityplot(imp_pmm_40)
# 
# save(imp_pmm_40, file = here::here("data", "analysis_data", "imp_pmm_40.RData"))
