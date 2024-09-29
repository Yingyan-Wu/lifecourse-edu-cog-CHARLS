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
colnames(charls_selected)
# Drop those derived variables for the dataset to be imputed
charls_selected_forimp <- charls_selected %>%
  select(r1agey_v3, female, childhood_edu, continuing_edu,
         first_child_v2, rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c,
         childhood_1st_region, rarural_child, first_hukou,
         chldstate_30, marital_30, rural_30, occupation_30, hukou_30,
         r1wtrespb)

DataExplorer::plot_missing(charls_selected_forimp)
missing_summary <- DataExplorer::profile_missing(charls_selected_forimp) %>%
  mutate(pct_missing = pct_missing*100) %>%
  arrange(pct_missing) %>%
  print(n = Inf)

writexl::write_xlsx(missing_summary, 
                    here::here("output", "tables", "missing_summary.xlsx"))

ordered_var_list <- c(paste0(missing_summary$feature))

#---- Data prep ----
# Running single imputation
# prep data by ordering by missingness
impute_data <- charls_selected_forimp[, ordered_var_list] %>%
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
ordinal_vars <- c("childhood_edu", "continuing_edu", "rameduc_c", "rafeduc_c")

cat_vars <- c("hukou_30", "occupation_30", "childhood_1st_region", 
              "first_hukou")

(cont_vars <- variable_length_mat %>%
    filter(!Var %in% c(binary_vars, ordinal_vars, cat_vars)) %>% pull(Var))

# Check
length(c(cont_vars, binary_vars, ordinal_vars, cat_vars))
sum(duplicated(c(cont_vars, binary_vars, ordinal_vars, cat_vars)))

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
imp_pmm_10 <- mice(impute_data, m = 10, maxit = 80, 
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

imp_pmm_10$loggedEvents # NULL
plot(imp_pmm_10)
# Compared to 40 imputation version (30 iterations). It did not make too much difference.
# stick with 10 imputations 80 iterations.
densityplot(imp_pmm_10)

save(imp_pmm_10, file = here::here("data", "analysis_data", "imp_pmm_10.RData"))

#---- Stack imputed dataset ----
load(here::here("data", "analysis_data", "imp_pmm_10.RData"))
imp_num = 10
imp_temp <- list()
for (i in 1:imp_num){
  imp_temp[[i]] <- complete(get(paste0("imp_pmm_", imp_num)),
                            action = i)
  imp_temp[[i]] <- cbind(
    charls_selected %>%
      select(ID, paste0("r", 2:5, "agey_v3"),
             paste0("r", 2:5, "wtrespb"),
             contains(c("imrc", "dlrc"))),
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

###---- prepare the data ----
imp_pmm_stacked %<>%
  dplyr::mutate(
    childhood_edu = factor(as.character(childhood_edu)), # drop order
    continuing_edu = factor(as.character(continuing_edu)), # drop order
    occupation_30_5cat =
      as.factor(case_when(occupation_30 %in% c(1, 2) ~ 1,
                          occupation_30 %in% c(3, 4, 5) ~ 2,
                          occupation_30 == 6 ~ 3,
                          occupation_30 == 7 ~ 4,
                          occupation_30 == 8 ~ 5)),
    occupation_30_2cat = as.factor(case_when(occupation_30 %in% c(1, 2) ~ 0, # agriculture
                                             occupation_30 %in% c(3:8) ~ 1)), # non-agriculture
    rameduc_2cat = as.factor(case_when(rameduc_c %in% 1:3 ~ 1, # with formal edu
                                       rameduc_c == 0 ~ 0)),# no formal edu
    rafeduc_2cat = as.factor(case_when(rafeduc_c %in% 1:3 ~ 1, 
                                       rafeduc_c == 0 ~ 0)),
    first_hukou_2cat = as.factor(case_when(first_hukou == 1 ~ 0, # agricultural hukou
                                           first_hukou %in% 2:3 ~ 1)), # non agricultural hukou
    # childhood_edu = factor(as.character(childhood_edu)), # drop order
    # continuing_edu = factor(as.character(continuing_edu)), # drop order
    hukou_30_2cat = as.factor(case_when(hukou_30 == 1 ~ 0,
                                        hukou_30 %in% 2:3 ~ 1)),
    across(ends_with("agey_v3"), ~ .x - r1agey_v3,
           .names = "{.col}fu_time")) %>%
  dplyr::rename_at(vars(ends_with("fu_time")),
                   function(x) str_replace(x, "agey_v3", "")) %>%
  group_by(imp) %>%
  mutate(r1wtrespb_rescaled = r1wtrespb/mean(r1wtrespb)) %>%
  # Rescale to have mean of 1 to make the survey weighted mixed model faster
  # tested in previous attempts, the results are the same beofre and after rescaling
  ungroup()

# # Sanity check
# imp_pmm_stacked %>% group_by(imp) %>%
#   summarise(mean(r1wtrespb_rescaled, na.rm = TRUE),
#             sd(r1wtrespb_rescaled, na.rm = TRUE))

#---- Analytic dataset ----
# Long dataset 
rctib_long <- imp_pmm_stacked %>%
  select(ID, imp, contains("agey_v3"), 
         contains("fu_time"),
         paste0("r", 1:2, "imrc"),
         paste0("r", 3, "imrc_cleaned"),
         paste0("r", 4:5, "imrc_eqt"),
         paste0("r", 1:2, "dlrc"),
         paste0("r", 3, "dlrc_cleaned"),
         paste0("r", 4:5, "dlrc_eqt")) %>%
  dplyr::rename_at(vars(contains(c("imrc", "dlrc"))),
                   function(x) str_replace(x, "_eqt|_cleaned", "")) %>%
  pivot_longer(cols = !c(ID, imp), 
               names_to = c("wave", ".value"), 
               names_pattern = "r(.)(.*)") %>%
  mutate(wave = as.numeric(wave)) %>%
  dplyr::arrange(ID, imp, agey_v3) %>%
  filter(!is.na(imrc) & !is.na(dlrc)) %>%
  mutate(
    first_imrc = ifelse(!is.na(imrc) & row_number() == min(which(!is.na(imrc))), 1, 0),
    first_dlrc = ifelse(!is.na(dlrc) & row_number() == min(which(!is.na(dlrc))), 1, 0))

imp_pmm_long <- imp_pmm_stacked %>% 
  select(-contains(c("imrc", "dlrc", "agey_v3", "fu_time"))) %>%
  left_join(rctib_long, by = c("ID", "imp"))

###---- Save the data ----
saveRDS(imp_pmm_stacked,
        here::here("data", "analysis_data", "imp_pmm_10_stacked.RDS"))

saveRDS(imp_pmm_long,
        file = here::here("data", "analysis_data", "imp_pmm_10_long.RDS"))

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
