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

#---- Harmonized dataset ----
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

#---- **Age distribution at each wave ----
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

#---- **Word recall ----
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
       y = "Immediate word recall",
       title = "Spaghetti plot for a random sample (n = 50)") +
  theme(legend.position = "none")

harmonized_long %>%
  filter(ID %in% random_id) %>%
  ggplot(aes(x = wave, y = dlrc, group = ID, color = factor(ID))) +
  geom_line() +
  theme_minimal() +
  labs(x = "Wave",
       y = "Delayed word recall",
       title = "Spaghetti plot for a random sample (n = 50)") +
  theme(legend.position = "none")


#---- ****empirical plot ----
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

#---- **Check wave 4 delay word recall ----
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

#---- **Check the word recall time ----
# This can be used after loading in core data in the data_construction script

# immediate word recall ends
charls %>% select(contains("DC008")) %>% head

# when delay word recall starts
charls %>% select(contains("DC026")) %>% head

word_recall_time <- charls %>% select(contains(c("dc008", "dc026")),
                                      -"w2_dc026", -"w2_dc008") %>%
  mutate(w1_dc008 = case_when(!is.na(w1_dc008_1) ~ 
                                paste0(w1_dc008_1, ":", w1_dc008_2),
                              TRUE ~ NA_character_),
         w1_dc026 = case_when(!is.na(w1_dc026_1) ~ 
                                paste0(w1_dc026_1, ":", w1_dc026_2),
                              TRUE ~ NA_character_)) %>%
  # select(w1_dc008, w1_dc026, w2_dc008_1, w2_dc026_1, w3_dc008, w3_dc026) %>%
  mutate(across(c("w1_dc008", "w1_dc026"), as.POSIXct,format="%H:%M")) %>%
  mutate(across(c(w2_dc008_1, w2_dc026_1, w3_dc008, w3_dc026), 
                as.POSIXct,format="%H:%M:%S")) %>%
  mutate(w1_time_diff = difftime(w1_dc026, w1_dc008, tz,
                                 units = c("mins")),
         w2_time_diff = difftime(w2_dc026_1, w2_dc008_1, tz,
                                 units = c("mins")),
         w3_time_diff = difftime(w3_dc026, w3_dc008, tz,
                                 units = c("mins")))

word_recall_time %>%
  select(contains("time_diff")) %>%
  mutate_all(as.numeric) %>%
  mutate(w1_time_diff = case_when(w1_time_diff < 0 ~ NA_real_,
                                  TRUE ~ w1_time_diff),
         w2_time_diff = case_when(w2_time_diff < 0 ~ NA_real_,
                                  TRUE ~ w2_time_diff),
         w3_time_diff = case_when(w3_time_diff < 0 ~ NA_real_,
                                  TRUE ~ w3_time_diff)) %>%
  apply(.,2, proc_sum, more = T)

view(word_recall_time %>%
       select(contains("time_diff")))

#---- Core data ----
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
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w1_", x)})
# colnames(charls_w1)

charls_w2 <- join_all(wave2, 
                      # by = c("ID", "householdID", "communityID"),
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

rm(list = paste0("wave", 1:4))
gc()

#---- Checking 0 in harmonized ----
# immediate word recall
harmonized_r3imrc_0_id <- harmonized %>% filter(r3imrc == 0) %>% pull(ID)
view(charls %>% 
       filter(ID %in% harmonized_r3imrc_0_id) %>%
       select(contains(c("w3_dc006s"))))
with(charls %>% filter(ID %in% harmonized_r3imrc_0_id),
     table(w3_dc006s11, w3_dc006s12, useNA = "ifany"))
# Two 0s are NAs

harmonized_r4imrc_0_id <- harmonized %>% filter(r4imrc == 0) %>% pull(ID)
view(charls %>% 
       filter(ID %in% harmonized_r4imrc_0_id) %>%
       select(contains(c("dc028_w4_s", "dc029_w4_s", "dc030_w4_s"))))
with(charls %>% filter(ID %in% harmonized_r4imrc_0_id),
     table(w4_dc028_w4_s11, w4_dc028_w4_s12, useNA = "ifany"))
# There are 1877 participants with non responses (can't recall any of the words)
# But the rest 653 participants seem to be NAed

# Delayed word recall
harmonized_r3dlrc_0_id <- harmonized %>% filter(r3dlrc == 0) %>% pull(ID)
view(charls %>% 
       filter(ID %in% harmonized_r3imrc_0_id) %>%
       select(contains(c("w3_dc027s"))))
with(charls %>% filter(ID %in% harmonized_r3imrc_0_id),
     table(w3_dc027s11, w3_dc027s12, useNA = "ifany"))
# 0s are nonresponses

harmonized_r4dlrc_0_id <- harmonized %>% filter(r4imrc == 0) %>% pull(ID)
view(charls %>% 
       filter(ID %in% harmonized_r4imrc_0_id) %>%
       select(contains(c("dc047_w4_s"))))
with(charls %>% filter(ID %in% harmonized_r4imrc_0_id),
     table(w4_dc047_w4_s11, w4_dc047_w4_s12, useNA = "ifany"))
# There are 1877 participants with non responses (can't recall any of the words)
# But the rest 653 participants seem to be NAed


#---- Immediate word recall ----
# Wave 2
charls %>% filter(w2_dc006_cnt == 3) %>%
  select(contains("w2_dc006_1")) %>%
  apply(., 2, table, useNA = "ifany")

charls %>% select(contains("w2_dc006_")) %>%
  apply(., 2, table, useNA = "ifany")

charls %>% select(contains("w1_dc006s")) %>%
  filter(w1_dc006s11 == 11) %>%
  apply(., 2, table, useNA = "ifany")

imrc_mat<- charls %>% select(
  contains(c("dc006s", # for wave 1 and 3
             "w2_dc006_1_s", "w2_dc006_2_s", "w2_dc006_3_s",
             "dc028_w4_s", "dc029_w4_s", "dc030_w4_s")))
colnames(imrc_mat)

imrc_mat %<>%
  mutate(across(ends_with("s11"), ~ case_when(.x == 0 ~ NA_real_,
                                              .x == 11 ~ 0)))

# Sanity check
imrc_mat %>% select(ends_with("s11")) %>%
  apply(., 2, function(x) table(x, useNA = "ifany"))
imrc_mat %>% select(ends_with("s12")) %>%
  apply(., 2, function(x) table(x, useNA = "ifany"))

imrc_mat %>% apply(., 2, function(x) table(x, useNA = "ifany"))

# put every non missing and non zero value to be 1 besides s12
imrc_mat[imrc_mat != 0 & !is.na(imrc_mat)] <- 1

charls %<>%
  mutate(w1_imrc = 
           case_when(
             w1_dc006s11 == 11 ~ 0,
             rowSums(is.na(imrc_mat%>%
                             select(contains("w1_dc006s"),
                                    -contains("s11")))) == 10 ~ NA_real_,
             TRUE ~ rowSums(imrc_mat%>% select(contains("w1_dc006s")),
                            na.rm = T)),
         w2_imrc = case_when(
           w2_dc006_1_s11 == 11 ~ 0,
           rowSums(is.na(imrc_mat%>% select(
             contains("w2_dc006_1_s"), 
             -contains(c("s11", "s12"))))) == 10 ~ NA_real_,
           TRUE ~ rowSums(imrc_mat%>% 
                            select(contains("w2_dc006_1_s"), 
                                   -contains(c("s12", "s11"))), 
                          na.rm = T)),
         w3_imrc = case_when(
           w3_dc006s11 == 11 ~ 0,
           w3_dc006s12 == 12 ~ NA_real_,
           TRUE ~ rowSums(imrc_mat %>% 
                            select(contains("w3_dc006s")), na.rm = T)),
         w4_imrc = case_when(
           w4_dc028_w4_s11 == 11 ~ 0,
           w4_dc028_w4_s12 == 12 ~ NA_real_,
           is.na(w4_dc028_w4_s11) & is.na(w4_dc028_w4_s12) ~ NA_real_,
           TRUE ~ rowSums(imrc_mat %>% select(
             contains("dc028_w4_s"), -contains(c("s12", "s11"))), na.rm = T)))

with(charls, table(w1_imrc, useNA = "ifany"))
with(harmonized, table(r1imrc, useNA = "ifany"))
# Wave 1 is in a closer enough distribution (miscategorized 1 or 2 participants)

with(charls, table(w2_imrc, useNA = "ifany"))
with(harmonized, table(r2imrc, useNA = "ifany"))
# Can't figure this out!!!!!!

with(charls, table(w3_imrc, useNA = "ifany"))
with(harmonized, table(r3imrc, useNA = "ifany"))
# Wave 3 is in a closer enough distribution (miscategorized 1 or 2 participants)

with(charls, table(w4_imrc, useNA = "ifany"))
with(harmonized, table(r4imrc, useNA = "ifany"))
# There are 1877 participants with non responses (can't recall any of the words)
# But the rest 653 participants seem to be NAed

#---- Delayed word recall ----
with(charls, table(w1_dc027s1, useNA = "ifany"))
with(charls, table(w4_dc047_w4_s11, useNA = "ifany"))
with(harmonized, table(r1dlrc, useNA = "ifany"))
with(harmonized, table(r4dlrc, useNA = "ifany"))

dlrc_mat<- charls %>% select(contains("dc027s"), contains("dc047_w4_s"))
dlrc_mat%<>%
  mutate(across(c(w1_dc027s11, w2_dc027s11, w3_dc027s11, w4_dc047_w4_s11),
                ~ case_when(. == 0 ~ NA_real_,
                            . == 11 ~ 0)))

dlrc_mat[dlrc_mat!= 0 & !is.na(dlrc_mat)] <- 1

with(dlrc_mat, table(w3_dc027s12, useNA = "ifany"))
with(dlrc_mat, table(w4_dc047_w4_s12, useNA = "ifany"))

charls %<>%
  mutate(w1_dlrc = 
           case_when(
             rowSums(is.na(dlrc_mat %>% 
                             select(contains("w1_dc027s"), 
                                    -contains("s11")))) == 10 ~ NA_real_,
             TRUE ~ rowSums(dlrc_mat %>% select(contains("w1_dc027s")), 
                            na.rm = T)),
         w2_dlrc = case_when(
           rowSums(is.na(dlrc_mat%>% 
                           select(contains("w2_dc027s"), 
                                  -contains("s11")))) == 10 ~ NA_real_,
           TRUE ~ rowSums(dlrc_mat%>% select(contains("w2_dc027s")), 
                          na.rm = T)),
         w3_dlrc = 
           case_when(w3_dc027s12 == 12 ~ NA_real_,
                     TRUE ~ rowSums(dlrc_mat%>% 
                                      select(contains("w3_dc027s")), na.rm = T)),
         w4_dlrc = case_when(w4_dc047_w4_s12 == 12 ~ NA_real_,
                             TRUE ~ rowSums(
                               dlrc_mat%>% 
                                 select(contains("dc047_w4_s")), na.rm = T)))

with(charls, table(w1_dlrc, useNA = "ifany"))
with(harmonized, table(r1dlrc, useNA = "ifany"))

with(charls, table(w2_dlrc, useNA = "ifany"))
with(harmonized, table(r2dlrc, useNA = "ifany"))

with(charls, table(w3_dlrc, useNA = "ifany"))
with(harmonized, table(r3dlrc, useNA = "ifany"))

with(charls, table(w4_dlrc, useNA = "ifany"))
with(harmonized, table(r4dlrc, useNA = "ifany"))
