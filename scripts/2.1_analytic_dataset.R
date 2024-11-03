# Analytic dataset construction
# Created by: Yingyan Wu
# Oct.10.2024

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
imp_pmm_stacked <- readRDS(here::here("data", "analysis_data", "imp_pmm_20_stacked.RDS"))
load(here::here("data", "analysis_data", "charls_selected.RData"))

#---- Prepare the data ----
imp_pmm_stacked %<>%
  left_join(charls_selected %>% select(ID, paste0("inw", 1:5)), by = "ID")
# Participating in waves indicators
imp_pmm_stacked %<>%
  mutate(n1_inw = case_when(n1_wave == 1 & inw1 == 1 ~ 1,
                            n1_wave == 2 & inw2 == 1 ~ 1,
                            n1_wave == 3 & inw3 == 1 ~ 1,
                            n1_wave == 4 & inw4 == 1 ~ 1,
                            n1_wave == 5 & inw5 == 1 ~ 1,
                            TRUE ~ 0),
         n2_inw = case_when(n2_wave == 2 & inw2 == 1 ~ 1,
                            n2_wave == 3 & inw3 == 1 ~ 1,
                            n2_wave == 4 & inw4 == 1 ~ 1,
                            n2_wave == 5 & inw5 == 1 ~ 1,
                            TRUE ~ 0),
         n3_inw = case_when(n3_wave == 3 & inw3 == 1 ~ 1,
                            n3_wave == 4 & inw4 == 1 ~ 1,
                            n3_wave == 5 & inw5 == 1 ~ 1,
                            TRUE ~ 0),
         n4_inw = case_when(n4_wave == 4 & inw4 == 1 ~ 1,
                            n4_wave == 5 & inw5 == 1 ~ 1,
                            TRUE ~ 0),
         n5_inw = case_when(n5_wave == 5 & inw5 == 1 ~ 1,
                            TRUE ~ 0))
# Check FU time and inw indicators
with(imp_pmm_stacked, table(n1_inw, !is.na(n1_fu_time), useNA = "ifany"))
with(imp_pmm_stacked, table(n2_inw, !is.na(n2_fu_time), useNA = "ifany"))
with(imp_pmm_stacked, table(n3_inw, !is.na(n3_fu_time), useNA = "ifany"))
with(imp_pmm_stacked, table(n4_inw, !is.na(n4_fu_time), useNA = "ifany"))
with(imp_pmm_stacked, table(n5_inw, !is.na(n5_fu_time), useNA = "ifany"))
imp_pmm_stacked %>% select(contains("fu_time")) %>%
  apply(., 2, summary)

# Clean the dataset & create variables for analysis
imp_pmm_stacked %<>%
  dplyr::mutate(
    childhood_edu = factor(as.character(childhood_edu)), # drop order
    continuing_edu = factor(as.character(continuing_edu)), # drop order
    continuing_edu_2cat = factor(case_when(continuing_edu == 0 ~ 0,
                                           continuing_edu %in% c(1, 2) ~ 1)),
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
    hukou_30_2cat = as.factor(case_when(hukou_30 == 1 ~ 0,
                                        hukou_30 %in% 2:3 ~ 1)),
    across(paste0("n1_", c("moneya", "medsa", "shopa", "mealsa", "housewka")), 
           ~as.numeric(as.character(.))),
    n1_iadl = 
      rowSums(across(paste0("n1_", c("moneya", "medsa", "shopa", "mealsa", "housewka")))),
    n1_iadl_2cat = case_when(n1_iadl == 0 ~ 0,
                             n1_iadl > 0 ~ 1),
    # impute fu_time for missing follow up people (imputing indicator variable: nx_inw)
    # Follow the actual FU time median
    n2_fu_time = case_when(is.na(n2_fu_time) ~ n1_fu_time + 2,
                           TRUE ~ n2_fu_time),
    n3_fu_time = case_when(is.na(n3_fu_time) ~ n2_fu_time + 2,
                           TRUE ~ n3_fu_time),
    n4_fu_time = case_when(is.na(n4_fu_time) ~ n3_fu_time + 3,
                           TRUE ~ n4_fu_time),
    n5_fu_time = case_when(is.na(n5_fu_time) ~ n4_fu_time + 2,
                           TRUE ~ n5_fu_time),
    unweight = 1) %>%
  group_by(imp) %>%
  mutate(mean_wtrespb = rowMeans(across(paste0("n", 1:5, "_wtrespb")), na.rm = T),
         min_wtrespb = pmin(n1_wtrespb, n2_wtrespb, n3_wtrespb, 
                            n4_wtrespb, n5_wtrespb, na.rm = T)) %>%
  ungroup()

# # Sanity check
# imp_pmm_stacked %>% group_by(imp) %>%
#   summarise(mean(r1wtrespb_rescaled, na.rm = TRUE),
#             sd(r1wtrespb_rescaled, na.rm = TRUE))
# imp_pmm_stacked %>%
#   select(contains(c("moneya", "medsa", "shopa", "mealsa", "housewka", "iadl"))) %>%
#   apply(., 2, table, useNA = "ifany")

#---- Check survey weight ----
imp_pmm_1 <- imp_pmm_stacked %>%
  filter(imp == 1)
imp_pmm_1 %>%
  select(contains(paste0("n", 1:5, "_wtrespb")), contains("wtrespb")) %>%
  apply(., 2, sum, na.rm = T)/1000000
with(imp_pmm_1, table(n1_wave, useNA = "ifany"))
charls_selected %>%
  select(contains("wtrespb")) %>%
  apply(., 2, sum, na.rm = T)

# imp_pmm_1 %>% select(contains("wtrespb")) %>% view()
colSums(is.na(imp_pmm_1 %>% select(contains("wtrespb"))))

# Check the weight distribution across waves
charls_selected %>%
  select(contains("wtrespb")) %>%
  pivot_longer(cols = everything(),
               names_to = "wave",
               values_to = "wtrespb",
               names_pattern = "r(.)wtrespb") %>%
  filter(wtrespb < 150000) %>%
  ggplot() +
  geom_density(aes(x = wtrespb, color = wave), alpha = 0.5) +
  theme_bw()
# Check the weight change within person
random_id <- charls_selected %>%
  filter(rowSums(is.na(across(contains("wtrespb")))) == 0) %>%
  pull(ID) %>%
  sample(., 1000, replace = F)
charls_selected %>%
  filter(ID %in% random_id) %>%
  select(ID, contains("wtrespb")) %>%
  pivot_longer(cols = !ID,
               names_to = "wave",
               values_to = "wtrespb",
               names_pattern = "r(.)wtrespb") %>%
  ggplot() +
  geom_line(aes(x = wave, y = wtrespb, group = ID, color = factor(ID))) +
  theme_bw() +
  theme(legend.position = "none")

imp_pmm_1 %>%
  filter(ID %in% random_id) %>%
  select(ID, paste0("n", 1:5, "_wtrespb")) %>%
  pivot_longer(cols = !ID,
               names_to = "follow_up",
               values_to = "wtrespb",
               names_pattern = "n(.)_wtrespb") %>%
  ggplot() +
  geom_line(aes(x = follow_up, y = wtrespb, group = ID, color = factor(ID))) +
  theme_bw() +
  theme(legend.position = "none")

##---- Rescale weights ----
mean_charls_rep_ss <- charls_selected %>%
  select(contains("wtrespb")) %>%
  apply(., 2, sum, na.rm = T) %>% as.vector() %>% mean() %>% print()

imp_pmm_stacked %<>% 
  group_by(imp) %>%
  mutate(n1_wtrespb_rscl = n1_wtrespb * (mean_charls_rep_ss/sum(n1_wtrespb))) %>%
  ungroup()

# Sanity check
imp_pmm_stacked %>%
  group_by(imp) %>%
  select(contains("wtrespb_rscl")) %>%
  dplyr::summarize(sum = sum(n1_wtrespb_rscl),
                   mean = mean(n1_wtrespb_rscl),
                   min = min(n1_wtrespb_rscl),
                   max = max(n1_wtrespb_rscl),
                   median = median(n1_wtrespb_rscl))

#---- Create censor indicator ----
for (n_fu in 1:5){
  imp_pmm_stacked %<>%
    mutate(!!paste0("n", n_fu, "_imrc_inw") := 
             case_when(!is.na(!!sym(paste0("n", n_fu, "_imrc"))) ~ 1,
                       is.na(!!sym(paste0("n", n_fu, "_imrc"))) ~ 0),
           !!paste0("n", n_fu, "_dlrc_inw") := 
             case_when(!is.na(!!sym(paste0("n", n_fu, "_dlrc"))) ~ 1,
                       is.na(!!sym(paste0("n", n_fu, "_dlrc"))) ~ 0))
}

for (n_fu in 1:5){
  print(with(imp_pmm_stacked, table(
    get(paste0("n", n_fu, "_imrc_inw")), 
    !is.na(get(paste0("n", n_fu, "_imrc"))), useNA = "ifany")))
}

for (n_fu in 1:5){
  print(with(imp_pmm_stacked, table(
    get(paste0("n", n_fu, "_dlrc_inw")), 
    !is.na(get(paste0("n", n_fu, "_dlrc"))), useNA = "ifany")))
}

#---- Analytic dataset ----
# Long only on outcome variables and follow up time
imp_pmm_long <- imp_pmm_stacked %>%
  pivot_longer(cols = contains(c("imrc", "dlrc", "fu_time")),
               # starts_with(paste0("n", 1:5)), 
               names_to = c("n_fu", ".value"),
               names_pattern = "n(.)_(.*)") %>%
  mutate(n_fu = as.numeric(as.character(n_fu))) %>%
  left_join(imp_pmm_stacked %>% select(ID, imp, contains(c("imrc", "dlrc"))),
            by = c("ID", "imp"))

#---- Restrict to 2011 participants only ----
imp_pmm_2011 <- imp_pmm_stacked %>%
  filter(n1_wave == 1)

imp_pmm_long_2011 <- imp_pmm_2011 %>%
  pivot_longer(cols = contains(c("imrc", "dlrc", "fu_time")),
               # starts_with(paste0("n", 1:5)), 
               names_to = c("n_fu", ".value"),
               names_pattern = "n(.)_(.*)") %>%
  mutate(n_fu = as.numeric(as.character(n_fu))) %>%
  left_join(imp_pmm_2011 %>% select(ID, imp, contains(c("imrc", "dlrc"))),
            by = c("ID", "imp"))

#---- Save the data ----
save(imp_pmm_stacked, imp_pmm_long,
     file = here::here("data", "analysis_data", "imp_pmm_20_wide_long.RData"))
save(imp_pmm_2011, imp_pmm_long_2011,
     file = here::here("data", "analysis_data", "imp_pmm_20_wide_long_2011.RData"))


#---- OLD ----
# mutate(r1wtrespb_rescaled = r1wtrespb/mean(r1wtrespb)) %>%
#   # Rescale to have mean of 1 to make the survey weighted mixed model faster
#   # tested in previous attempts, the results are the same beofre and after rescaling

# # Create censor indicator
# rc_temp <- imp_pmm_stacked %>%
#   select(paste0("r", 1:2, "imrc"),
#          paste0("r", 3, "imrc_cleaned"),
#          paste0("r", 4:5, "imrc_eqt"),
#          paste0("r", 1:2, "dlrc"),
#          paste0("r", 3, "dlrc_cleaned"),
#          paste0("r", 4:5, "dlrc_eqt")) %>%
#   set_colnames(c(paste0("r", 1:5, "imrc"), paste0("r", 1:5, "dlrc"))) %>%
#   mutate(across(everything(), 
#                 function(x) ifelse(is.na(x), 1, 0),
#                 .names = "{.col}_censor"))
# 
# imp_pmm_stacked <- cbind(imp_pmm_stacked, rc_temp %>% select(ends_with("_censor")))
#---- Analytic dataset ----
# # Long dataset 
# rctib_long <- imp_pmm_stacked %>%
#   select(ID, imp, contains("agey_v3"), 
#          contains("fu_time"),
#          paste0("r", 1:2, "imrc"),
#          paste0("r", 3, "imrc_cleaned"),
#          paste0("r", 4:5, "imrc_eqt"),
#          paste0("r", 1:2, "dlrc"),
#          paste0("r", 3, "dlrc_cleaned"),
#          paste0("r", 4:5, "dlrc_eqt"),
#          ends_with("censor")) %>%
#   dplyr::rename_at(vars(contains(c("imrc", "dlrc"))),
#                    function(x) str_replace(x, "_eqt|_cleaned", "")) %>%
#   pivot_longer(cols = !c(ID, imp), 
#                names_to = c("wave", ".value"), 
#                names_pattern = "r(.)(.*)") %>%
#   mutate(wave = as.numeric(wave)) %>%
#   dplyr::arrange(ID, imp, agey_v3, fu_time) %>%
#   # filter(!(is.na(imrc) & is.na(dlrc))) %>% # will use ipcw!
#   mutate(
#     first_imrc = ifelse(!is.na(imrc) & row_number() == min(which(!is.na(imrc))), 1, 0),
#     first_dlrc = ifelse(!is.na(dlrc) & row_number() == min(which(!is.na(dlrc))), 1, 0))
# 
# imp_pmm_long <- imp_pmm_stacked %>% 
#   select(-contains(c("imrc", "dlrc", "agey_v3", "fu_time"))) %>%
#   left_join(rctib_long, by = c("ID", "imp"))
