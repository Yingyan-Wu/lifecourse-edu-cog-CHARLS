# CHARLS_data_cognitive exploring
# Created by: Yingyan Wu
# Some code adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# Feb.03.2023

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "glue", 
       "survey", "lme4",  "labelled")
# Do not load "summarytools" as it contains conflicted functions as dplyr

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the datasets ----
#---- **harmonized ----
harmonized_variables <- c("ID", "householdID", "pnc", "communityID",
                          "ID_w1", "householdID_w1", 
                          "rabyear", "rabmonth", "rabday", 
                          "radyear", "radmonth",
                          paste0("r", 1:4, "agey"),
                          "ragender",
                          "raeduc_c",
                          paste0("r", 1:4, "imrc"), # immediate word recall
                          paste0("r", 1:4, "dlrc")
)

harmonized <- haven::read_dta(here::here("data", "raw_data", "harmonized",
                                         "H_CHARLS_D_Data.dta"),
                              col_select = all_of(harmonized_variables))
# view(harmonized %>%
#        filter(ID_w1 == ""))

harmonized_selected <- harmonized %>% filter(ID_w1 != "")

#---- Age distribution at each wave ----
harmonized_selected %>%
  select(contains("agey")) %>%
  apply(., 2, function(x) proc_sum(x, more = T))

harmonized_selected %>%
  select(ID, contains("agey")) %>%
  pivot_longer(cols = -ID,
               names_to = "wave",
               names_pattern = "r(.)agey",
               values_to = "agey") %>%
  mutate(wave = case_when(wave == 1 ~ 2011,
                          wave == 2 ~ 2013,
                          wave == 3 ~ 2015,
                          wave == 4 ~ 2018)) %>%
  ggplot(aes(x = agey)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ wave, scales = "free_x") +
  theme_minimal()

#---- Word recall ----
# Missingness
round(colSums(is.na(harmonized_selected %>%
                      select(contains("agey"), contains("imrc"),
                             contains("dlrc"))))/length(unique(harmonized$ID))*100, 2)

harmonized_selected %<>%
  mutate(imrc_balanced = case_when(!is.na(r1imrc) & !is.na(r2imrc) & 
                                     !is.na(r3imrc) & !is.na(r4imrc) ~ 1,
                                   TRUE ~ 0),
         dlrc_balanced = case_when(!is.na(r1dlrc) & !is.na(r2dlrc) & 
                                     !is.na(r3dlrc) & !is.na(r4dlrc) ~ 1,
                                   TRUE ~ 0))
with(harmonized_selected, table(imrc_balanced, useNA = "ifany"))
with(harmonized_selected, table(dlrc_balanced, useNA = "ifany"))

harmonized_long <- harmonized_selected %>%
  select(ID, contains("agey"), contains("imrc"),
         contains("dlrc"), imrc_balanced, dlrc_balanced) %>%
  pivot_longer(cols = -c(ID, imrc_balanced, dlrc_balanced),
               names_to = c("wave", ".value"),
               names_pattern = "r(\\d+)(.*)") %>%
  print()

random_id <- sample(harmonized_selected$ID, size = 50)

harmonized_long %>%
  filter(ID %in% random_id) %>%
  ggplot(aes(x = wave, y = imrc, group = ID, color = factor(ID))) +
  geom_line() +
  theme_minimal() +
  labs(x = "Wave",
       y = "Immediate word recall") +
  theme(legend.position = "none")

harmonized_long %>%
  filter(ID %in% random_id) %>%
  ggplot(aes(x = wave, y = dlrc, group = ID, color = factor(ID))) +
  geom_line() +
  theme_minimal() +
  labs(x = "Wave",
       y = "Delayed word recall") +
  theme(legend.position = "none")


#---- **empirical plot ----
harmonized_empirical_all <- harmonized_long %>%
  filter(!is.na(imrc)) %>%
  group_by(wave) %>%
  summarise_at(vars(imrc), 
               funs(n(), mean, sd)) %>%
  mutate(se = sd/sqrt(n),
         variable = "Immediate word recall") %>%
  rbind(harmonized_long %>%
          filter(!is.na(dlrc)) %>%
          group_by(wave) %>%
          summarise_at(vars(dlrc), 
                       funs(n(), mean, sd)) %>%
          mutate(se = sd/sqrt(n),
                 variable = "Delayed word recall")) %>%
  print()

harmonized_empirical_all %>%
  ggplot(aes(x = wave, y = mean, group = variable, color = variable)) + 
  geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se), width=.1) +
  geom_text(aes(label = paste0("n = ", n), y = mean + 2 * se), vjust = -.5) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x = "Wave",
       y = "Word recall",
       title = "Average word recall score +/- 2 SE at different wave
       all available data")

harmonized_empirical_balanced <- harmonized_long %>%
  filter(imrc_balanced == 1) %>%
  group_by(wave) %>%
  summarise_at(vars(imrc), 
               funs(n(), mean, sd)) %>%
  mutate(se = sd/sqrt(n),
         variable = "Immediate word recall\n(n = 7946)") %>%
  rbind(harmonized_long %>%
          filter(dlrc_balanced == 1) %>%
          group_by(wave) %>%
          summarise_at(vars(dlrc), 
                       funs(n(), mean, sd)) %>%
          mutate(se = sd/sqrt(n),
                 variable = "Delayed word recall\n(n = 7446)")) %>%
  print()

harmonized_empirical_balanced %>%
  ggplot(aes(x = wave, y = mean, group = variable, color = variable)) + 
  geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se), width=.1) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x = "Wave",
       y = "Word recall",
       title = "Average word recall score +/- 2 SE at different wave
       balanced data")

view(harmonized_long %>%
       filter(dlrc_balanced == 1 & wave == 4))

#---- Check wave 4 delay word recall ----
with(harmonized %>%
       mutate(r4rc_flag = case_when(r4imrc < r4dlrc ~ "immediate < delay",
                                    r4imrc == r4dlrc ~ "immediate = delay",
                                    r4imrc > r4dlrc ~ "immediate > delay")),
     summarytools::freq(r4rc_flag))

with(harmonized %>%
       mutate(r4rc_diff = r4imrc - r4dlrc) %>%
       labelled::set_variable_labels(r4rc_diff = "immediate - delay"),
     summarytools::freq(r4rc_diff))

with(harmonized,
     table(r4imrc < r4dlrc, useNA = "ifany"))

#---- Reading core data ----
#---- **main datasets ----
# Adapted from Mingfei Dong's data reading script, 
# Crystal's reading result snippet from make_figures.R in exposure_trajectories
# and the IDRE website
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("health_status_and_functioning", "cognition")
folder_index <- c(1:length(folders))[-c(1, 3, 5, 8)] # main waves only

# Read in function
# read_charls <- function(paths){
#   if (str_detect(paths, "2011")) {
#     haven::read_dta(paths) %>%
#       select(-"proxy") # pending: can't figure out how to rename proxy automatically
#   } else if (str_detect(paths, "Family_Information") & 
#              str_detect(paths, "2013|2015")){
#     haven::read_dta(paths) %>%
#       select(-c("versionID"))
#   } else if (str_detect(paths, "2013|2015")){
#     haven::read_dta(paths) %>%
#       select(-c("xrtype", "versionID"))
#   } else if (str_detect(paths, "Family_Information") & 
#              str_detect(paths, "2018")){
#     haven::read_dta(paths) %>%
#       select(-c("versionID"))
#   } else if (str_detect(paths, "2018")){
#     haven::read_dta(paths) %>%
#       select(-c("versionID",
#                 "xrgender", "ziwtime"))
#   }

# rename_with(.cols = "proxy",
#            function(x){paste0(gsub(".*/(.*)\\..*", "\\1", files),
#                               "_", x)})
# Add section name to proxy variable name
# }

for (i in folder_index){
  # get all file names
  files <- list.files(path = folders[i], full.names = TRUE)
  files <- files[str_detect(files, regex(glue_collapse(datasets, sep="|"), 
                                         ignore_case = T))]
  # Read in as list
  list <- lapply(files, read_dta)
  # list <- lapply(files, read_charls)
  names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
  # Assign names for each wave's list
  assign(paste0("wave", which(folder_index == i)), list)
}

rm(list)

# ---- **join the datasets ----
# Waves
charls_w1 <- join_all(wave1, 
                      by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w1_", x)})
# colnames(charls_w1)

charls_w2 <- join_all(wave2, 
                      by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w2_", x)})
# colnames(charls_w2)

charls_w3 <- join_all(wave3, 
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w3_", x)})
# colnames(charls_w3)

charls_w4 <- join_all(wave4, 
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w4_", x)})
# colnames(charls_w4)

harmonized_id <- harmonized %>% 
  select(ID, ID_w1) %>%
  dplyr::rename("ID_w234" = "ID")

charls_w1 %<>%
  left_join(harmonized_id, by = c("ID" = "ID_w1")) %>%
  dplyr::rename("ID_w1" = "ID")

charls_w1 %<>% dplyr::rename("ID" = "ID_w234")

#---- ** join to a single dataset (wave 1 baseline) ----
# Merge all the waves (All the participants!)
charls <- join_all(c(list(charls_w1, charls_w2, charls_w3, charls_w4)),
                   by = c("ID"), # "ID_w234" = "ID", # somehow it didn't work anymore. Rename the ID_w234 in w1 above
                   # "householdID", "communityID"),# Pending: needs to figure out this later
                   type = "full") %>% 
  as_tibble()

#---- Select to dlrc variables only ----

with(charls, table(w1_dc027s1, useNA = "ifany"))
with(charls, table(w4_dc047_w4_s11, useNA = "ifany"))
with(harmonized, table(r1dlrc, useNA = "ifany"))
with(harmonized, table(r4dlrc, useNA = "ifany"))

charls_cog_mat <- charls %>% select(contains("dc027s"), contains("dc047_w4_s"))
charls_cog_mat %<>%
  mutate(across(c(w1_dc027s11, w2_dc027s11, w3_dc027s11, w4_dc047_w4_s11),
                ~ case_when(. == 0 ~ NA_real_,
                            . == 11 ~ 0)))

charls_cog_mat[charls_cog_mat != 0 & !is.na(charls_cog_mat)] <- 1

with(charls_cog_mat, table(w3_dc027s12, useNA = "ifany"))
with(charls_cog_mat, table(w4_dc047_w4_s12, useNA = "ifany"))

charls %<>%
  mutate(w1_dlrc = 
           case_when(
             rowSums(is.na(charls_cog_mat %>% 
                             select(contains("w1_dc027s"), 
                                    -contains("s11")))) == 10 ~ NA_real_,
             TRUE ~ rowSums(charls_cog_mat %>% select(contains("w1_dc027s")), 
                           na.rm = T)),
         w2_dlrc = case_when(
           rowSums(is.na(charls_cog_mat %>% 
                           select(contains("w2_dc027s"), 
                                  -contains("s11")))) == 10 ~ NA_real_,
           TRUE ~ rowSums(charls_cog_mat %>% select(contains("w2_dc027s")), 
                         na.rm = T)),
         w3_dlrc = 
           case_when(w3_dc027s12 == 12 ~ NA_real_,
                     TRUE ~ rowSums(charls_cog_mat %>% 
                                     select(contains("w3_dc027s")), na.rm = T)),
         w4_dlrc = case_when(w4_dc047_w4_s12 == 12 ~ NA_real_,
                             TRUE ~ rowSums(
                               charls_cog_mat %>% 
                                 select(contains("dc047_w4_s")), na.rm = T)))

with(charls, table(w1_dlrc, useNA = "ifany"))
with(harmonized, table(r1dlrc, useNA = "ifany"))

with(charls, table(w2_dlrc, useNA = "ifany"))
with(harmonized, table(r2dlrc, useNA = "ifany"))

with(charls, table(w3_dlrc, useNA = "ifany"))
with(harmonized, table(r3dlrc, useNA = "ifany"))

with(charls, table(w4_dlrc, useNA = "ifany"))
with(harmonized, table(r4dlrc, useNA = "ifany"))
