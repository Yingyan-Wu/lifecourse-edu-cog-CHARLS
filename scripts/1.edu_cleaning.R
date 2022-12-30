# CHARLS_data_education_data_cleaning
# Created by: Yingyan Wu
# Some code adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# Oct.19.2022

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
                          paste0("h", 1:4, "rural"),
                          paste0("r", 1:4, "agey"),
                          "ragender",
                          "raeduc_c",
                          "r1mstat",
                          "r1lbrf_c",
                          paste0("r", 1:4, "imrc"), # immediate word recall
                          paste0("r", 1:4, "dlrc"),
                          "rameduc_c", "rafeduc_c", "ramomoccup_c", 
                          "radadoccup_c")

harmonized <- haven::read_dta(here::here("data", "raw_data", "harmonized",
                                         "H_CHARLS_D_Data.dta"),
                              col_select = all_of(harmonized_variables))
# view(harmonized %>%
#        filter(ID_w1 == ""))

# harmonized_selected <- harmonized %>% filter(ID_w1 != "")

#---- pending reading core data ----
#---- **main datasets ----
# Adapted from Mingfei Dong's data reading script, 
# Crystal's reading result snippet from make_figures.R in exposure_trajectories
# and the IDRE website
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("demographic_background", "health_status_and_functioning", 
              "family_information")
folder_index <- c(1:length(folders))[-c(1, 3, 5, 8)] # main waves only

# Read in function
read_charls <- function(paths){
  if (str_detect(paths, "2011")) {
    haven::read_dta(paths) %>%
      select(-"proxy") # pending: can't figure out how to rename proxy automatically
  } else if (str_detect(paths, "Family_Information") & 
             str_detect(paths, "2013|2015")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2013|2015")){
    haven::read_dta(paths) %>%
      select(-c("xrtype", "versionID"))
  } else if (str_detect(paths, "Family_Information") & 
             str_detect(paths, "2018")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2018")){
    haven::read_dta(paths) %>%
      select(-c("versionID",
                "xrgender", "ziwtime"))
  }

  # rename_with(.cols = "proxy",
  #            function(x){paste0(gsub(".*/(.*)\\..*", "\\1", files),
  #                               "_", x)})
  # Add section name to proxy variable name
}

for (i in folder_index){
  # get all file names
  files <- list.files(path = folders[i], full.names = TRUE)
  files <- files[str_detect(files, regex(glue_collapse(datasets, sep="|"), 
                                         ignore_case = T))]
  # Read in as list
  list <- lapply(files, read_charls)
  # list <- lapply(files, read_charls)
  names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
  # Assign names for each wave's list
  assign(paste0("wave", which(folder_index == i)), list)
}

rm(list)

#---- **life history datasets ----
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("demographic_background", "education_history", 
              "family_information")
folder_index <- c(1:length(folders))[5] # life history only

files <- list.files(path = folders[5], full.names = TRUE)
files <- files[str_detect(files, regex(glue_collapse(datasets, sep="|"), 
                                       ignore_case = T))]
# Read in as list
list <- lapply(files, haven::read_dta)
names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
# Assign names for each wave's list
assign("life_history", list)
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
                      # by = c("ID", "householdID", "communityID", "versionID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w2_", x)})
# colnames(charls_w2)

charls_w3 <- join_all(wave3, 
                      by = c("ID", "householdID", "communityID"),
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

charls_lh <- join_all(life_history, 
                      # by = c("ID", "householdID", "communityID", "versionID"), 
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID", "versionID"), 
              function(x){paste0("lh_", x)})

rm(life_history)
rm(list = paste0("wave", 1:4))
gc()
# Pending
# 3 more participants in harmonized dataset in wave 1
# harmonized_selected %>%
#   filter(!ID_w1 %in% charls_w1$ID)
# 
# view(charls_w1 %>% filter(str_detect(ID, "064604211")))

# ID from harmonized dataset
harmonized_id <- harmonized %>% 
  select(ID, ID_w1) %>%
  dplyr::rename("ID_w234" = "ID")
# Sanity check
table(!harmonized_id$ID_w234 %in% charls_w2$ID)
table(!harmonized_id$ID_w234 %in% charls_w3$ID)
table(!harmonized_id$ID_w234 %in% charls_w4$ID)

charls_w1 %<>%
  left_join(harmonized_id, by = c("ID" = "ID_w1")) %>%
  dplyr::rename("ID_w1" = "ID")

charls_w1 %<>% dplyr::rename("ID" = "ID_w234")

rm(harmonized_id)

#---- ** join to a single dataset (wave 1 baseline) ----
# Merge all the waves (All the participants!)
charls <- join_all(c(list(charls_w1, charls_w2, charls_w3, charls_w4,
                          charls_lh)),
                   by = c("ID"), # "ID_w234" = "ID", # somehow it didn't work anymore. Rename the ID_w234 in w1 above
                   # "householdID", "communityID"),# Pending: needs to figure out this later
                   type = "full") %>% 
  as_tibble()

rm(list = paste0("charls_", c(paste0("w", 1:4), "lh")))
charls %<>% remove_labels()

#---- Education variables ----
#---- **Adult Education ----
charls %<>%
  mutate_at(vars(starts_with("w4_bd007_w4_1_s")), 
            function(x) ifelse(x == 0, NA, x)) %>%
  mutate(w1_bd007 = coalesce(w1_bd007s1, w1_bd007s2, w1_bd007s3, w1_bd007s4,
                             w1_bd007s5, w1_bd007s6, w1_bd007s7, w1_bd007s8),
         w2_bd007 = coalesce(w2_bd007s1, w2_bd007s2, w2_bd007s3, w2_bd007s4,
                             w2_bd007s5, w2_bd007s6, w2_bd007s7, w2_bd007s8),
         w2_bd007_w2_1 = coalesce(w2_bd007_w2_1s1, w2_bd007_w2_1s2, 
                                  w2_bd007_w2_1s3, w2_bd007_w2_1s4, 
                                  w2_bd007_w2_1s5, w2_bd007_w2_1s6, 
                                  w2_bd007_w2_1s7, w2_bd007_w2_1s8),
         w3_bd007_w2_1 = coalesce(w3_bd007_w2_1s1, w3_bd007_w2_1s2,
                                  w3_bd007_w2_1s3, w3_bd007_w2_1s4,
                                  w3_bd007_w2_1s5, w3_bd007_w2_1s6,
                                  w3_bd007_w2_1s7, w3_bd007_w2_1s8),
         w4_bd007_w2_1 = coalesce(w4_bd007_w2_1_s1, w4_bd007_w2_1_s2,
                                  w4_bd007_w2_1_s3, w4_bd007_w2_1_s4,
                                  w4_bd007_w2_1_s5, w4_bd007_w2_1_s6,
                                  w4_bd007_w2_1_s7, w4_bd007_w2_1_s8),
         w4_bd007_w4_1 = coalesce(w4_bd007_w4_1_s1, w4_bd007_w4_1_s2, 
                                  w4_bd007_w4_1_s3, w4_bd007_w4_1_s4, 
                                  w4_bd007_w4_1_s5, w4_bd007_w4_1_s6, 
                                  w4_bd007_w4_1_s7, w4_bd007_w4_1_s8))

with(charls, table(w1_bd007, w2_bd007, useNA = "ifany"))
with(charls, table(w1_bd007, w2_bd007_w2_1, useNA = "ifany"))
# No overlap between these 2 variables (w2_bd007 and w2_bd007_w2_1)
with(charls, table(w2_bd007, w2_bd007_w2_1, useNA = "ifany"))
with(charls, table(w1_bd007s1, w1_bd007s1))
with(charls, table(w3_bd007_w2_1, useNA = "ifany"))

charls %<>%
  mutate(adult_edu = case_when(w4_bd007_w2_1 != 1|w3_bd007_w2_1 != 1|
                                 w2_bd007_w2_1 != 1|w2_bd007 != 1|
                                 w1_bd007 != 1 ~ 1,
                               is.na(w4_bd007_w2_1) & is.na(w3_bd007_w2_1) & 
                                 is.na(w2_bd007_w2_1) & is.na(w2_bd007) & 
                                 is.na(w1_bd007) ~ NA_real_,
                               TRUE ~ 0),
         adult_edu_dplm = case_when(w4_bd009_w2_1 == 1|w3_bd009_w2_1 == 1|
                                      w2_bd009_w2_1 == 1|w2_bd009 == 1|
                                      w1_bd009 == 1|w3_bd011 == 2 ~ 1, 
                                    # pending on being more generalized on setting 
                                    # those indicating adult education degree and 
                                    # the time obtaining the degree to have the diploma
                                    is.na(w4_bd009_w2_1) & is.na(w3_bd009_w2_1) & 
                                      is.na(w2_bd009_w2_1) & is.na(w2_bd009) & 
                                      is.na(w1_bd009) ~ NA_real_,
                                    TRUE ~ 0))

proc_freq(charls, adult_edu)
proc_freq(charls, adult_edu_dplm)
with(charls, table(adult_edu, adult_edu_dplm, useNA = "ifany"))
# For those reported not having adult edu but having diploma, check their degree
charls %>% filter(adult_edu == 0 & adult_edu_dplm == 1) %>% select(contains("bd011"))

charls %<>%
  mutate(adult_edu = case_when(adult_edu == 0 & adult_edu_dplm == 1 & 
                                 w3_bd011 %in% c(1, 2) ~ 1,
                               TRUE ~ adult_edu),
         adult_edu_dplm = case_when(
           (adult_edu == 0 & adult_edu_dplm == 1 & !w3_bd011 %in% c(1, 2))|
             (adult_edu == 1 & is.na(adult_edu_dplm))~ 0,
           TRUE ~ adult_edu_dplm))

with(charls, table(adult_edu, adult_edu_dplm, useNA = "ifany"))

#---- **Highest level of education attained ----
charls %<>%
  # Use harmonized education category for now unless future we found that it's better to not use it
  left_join(harmonized %>% select(ID, ragender, raeduc_c, rabyear, 
                                  h1rural, r1lbrf_c, r1mstat,
                                  paste0("r", 1:4, "imrc"), # immediate word recall
                                  paste0("r", 1:4, "dlrc"),
                                  rameduc_c, rafeduc_c, ramomoccup_c, 
                                  radadoccup_c), by = "ID")

# proc_freq(harmonized, raeduc_c)

#---- Edu + adult edu ----
charls %<>%
  mutate(edu_cat =  case_when(
    # No formal education at all
    raeduc_c == 1 & adult_edu == 0 ~ 0,
    # No formal education + adult education without diploma
    raeduc_c == 1 & adult_edu == 1 & adult_edu_dplm == 0 ~ 1,
    # No formal education + adult education with diploma
    raeduc_c == 1 & adult_edu == 1 & adult_edu_dplm == 1 ~ 2,
    # less than vocational school (finished elementary school) + no adult edu
    raeduc_c %in% c(2, 3, 4) & adult_edu == 0 ~ 3,
    # less than vocational school (finished elementary school) + adult edu without diploma
    raeduc_c %in% c(2, 3, 4) & adult_edu == 1 & adult_edu_dplm == 0 ~ 4,
    # less than vocational school (finished elementary school) + adult edu with diploma
    raeduc_c %in% c(2, 3, 4) & adult_edu == 1 & adult_edu_dplm == 1 ~ 5,
    # vocational school/middle school/high school + no adult edu
    raeduc_c %in% c(5, 6, 7) & adult_edu == 0 ~ 6,
    # vocational school/middle school/high school + adult edu without diploma
    raeduc_c %in% c(5, 6, 7) & adult_edu == 1 & adult_edu_dplm == 0 ~ 7,
    # vocational school/middle school/high school + adult edu with diploma
    raeduc_c %in% c(5, 6, 7) & adult_edu == 1 & adult_edu_dplm == 1 ~ 8,
    # college & above + no adult edu
    raeduc_c %in% c(8, 9, 10) & adult_edu == 0 ~ 9,
    # college & above + adult edu without diploma
    raeduc_c %in% c(8, 9, 10) & adult_edu == 1 & adult_edu_dplm == 0 ~ 10,
    # college & above + adult edu with diploma
    raeduc_c %in% c(8, 9, 10) & adult_edu == 1 & adult_edu_dplm == 1 ~ 11)) %>%
  labelled::set_value_labels(
    edu_cat = c("No Formal Education (Illiterate)" = 0,
                "No formal education + continuing education without a diploma" = 1,
                "No formal education + continuing education with diploma" = 2,
                "Elementary school or less + no continuing education" = 3,
                "Elementary school or less + continuing education without a diploma" = 4,
                "Elementary school or less + continuing education with a diploma" = 5,
                "vocational school/middle school/high school + no continuing education" = 6,
                "vocational school/middle school/high school + continuing education without a diploma" = 7,
                "vocational school/middle school/high school + continuing education with a diploma" = 8,
                "college & above + no continuing education" = 9,
                "college & above + continuing education without a diploma" = 10,
                "college & above + continuing education with a diploma" = 11))

proc_freq(charls, edu_cat)
with(charls, table(edu_cat, raeduc_c, useNA = "ifany"))
with(charls %>% filter(is.na(edu_cat) & raeduc_c == 1), 
     table(adult_edu, adult_edu_dplm, useNA = "ifany"))

#---- Adult edu time ----
proc_univariate(charls$w1_bd010)
with(charls %>% filter(adult_edu == 1), 
     table(w1_bd011, adult_edu_dplm,useNA = "ifany"))
with(charls %>% filter(adult_edu == 1), 
     table(w2_bd011, adult_edu_dplm,useNA = "ifany"))
with(charls %>% filter(adult_edu == 1), 
     table(w3_bd011, adult_edu_dplm,useNA = "ifany"))
with(charls %>% filter(adult_edu == 1), 
     table(w4_bd011, adult_edu_dplm,useNA = "ifany"))

charls %>% filter(w3_bd011 == 2 & adult_edu_dplm == 0) %>% 
  select(contains(c("bd009", "bd010", "edu")))

# All those indicate adult degree as others, set them to be NA
charls %<>% 
  mutate(across(.cols = ends_with("bd011"), 
                ~ case_when(. == 4 ~ NA_real_,
                            TRUE ~ .)))
# Year of the highest adult education degree obtained
with(charls, table(adult_edu_dplm, (!is.na(w1_bd010)|!is.na(w2_bd010)|
                                      !is.na(w3_bd010)|!is.na(w4_bd010)),
                   useNA = "ifany"))

# charls %<>% mutate(
#   adult_edu_dplm_year = case_when(!is.na(w1_bd010) ~ w1_bd010,
#                                   !is.na(w2_bd010) ~ w2_bd010,
#                                   !is.na(w3_bd010) ~ w3_bd010,
#                                   !is.na(w4_bd010) ~ w4_bd010),
#   adult_edu_start_year = case_when(!is.na(w1_bd010) ~ w1_bd010 - w1_bd008,
#                                    !is.na(w2_bd010) ~ w2_bd010 - w2_bd008,
#                                    !is.na(w3_bd010) ~ w3_bd010 - w3_bd008,
#                                    !is.na(w4_bd010) ~ w4_bd010 - w4_bd008_w2_1)) %>%
#   mutate(adult_edu_dplm_year = case_when(adult_edu_dplm_year > 2010 ~ NA_real_,
#                                          TRUE ~ adult_edu_dplm_year),
#          age_adult_edu_dplm = adult_edu_dplm_year - rabyear)
# 
# proc_univariate(charls$adult_edu_dplm_year)
# proc_univariate(charls$age_adult_edu_dplm)
# 
# view(charls %>% filter(age_adult_edu_dplm <= 23) %>%
#        select(contains(c("bd008", "bd009", "bd010", "edu"))))

# view(charls %>%
#   select(ID, adult_edu, adult_edu_dplm, adult_edu_dplm_year,
#          contains(c("bd011", "bd010", "bd009"))) %>%
#   filter(adult_edu_dplm == 1 & adult_edu_dplm_year > 2011))

#---- Education interruption 1967 - 1977 ----
proc_freq(charls, lh_e002)
with(charls, table(rabyear, lh_e002, useNA = "ifany"))
with(charls, table(lh_e003a, lh_e002, useNA = "ifany"))
with(charls, table(lh_e004, lh_e002, useNA = "ifany"))
length(charls %>% filter(rabyear <= 1962 & rabyear >= 1946))
# charls %>% filter(rabyear <= 1962 & rabyear >= 1946)

# Pending

#---- birth order ----
proc_freq(charls, w3_cc003_w3_1)

charls %<>%
  mutate(first_child = case_when(
    w1_cc002_1 > 0 |w1_cc002_3 > 0|w1_cc004_1 > 0|w1_cc004_3 > 0|
      w2_cc002_1_1_ > 0|w2_cc002_3_1_ > 0|w2_cc004_1_1_ > 0|w2_cc004_3_1_ > 0 ~ 0,
    is.na(w1_cc002_1) & is.na(w1_cc002_3) & is.na(w1_cc004_1) & is.na(w1_cc004_3) &
      is.na(w2_cc002_1_1_) & is.na(w2_cc002_3_1_) & is.na(w2_cc004_1_1_) & is.na(w2_cc004_3_1_) ~ NA_real_,
    TRUE ~ 1))
proc_freq(charls, first_child)

#---- Save the data ----
charls %<>%
  right_join(harmonized %>% select(ID, r1mstat), by = "ID")
save(charls, file = here::here("data", "analysis_data", "charls_proposal.RData"))

#---- OLD ----
#---- ** Edu level cleaning ----
# # Build the final version of highest education
# with(charls, table(w2_bd001_w2_4, w1_bd001, useNA = "ifany"))

# #---- ****Adult education exclusion ----
# # w1 edu level * w1 level contains adult edu
# with(charls, table(w1_bd001, w2_bd001_w2_1, useNA = "ifany"))
# # w1 edu level * w1 edu level (without adult edu, collected at w2)
# with(charls %>% filter(w2_bd001_w2_1 == 1), 
#      table(w1_bd001, w2_bd001_w2_3, useNA = "ifany"))
# with(charls %>% filter(w2_bd001_w2_1 == 2), 
#      table(w1_bd001, w2_bd001_w2_3, useNA = "ifany"))
# # w1 edu level (without adul edu collected at w2) * w2 edu level
# with(charls %>% filter(w2_bd001_w2_2 == 2), 
#      table(w2_bd001_w2_4, w2_bd001_w2_3, useNA = "ifany"))
# 
# with(charls %>% filter(w2_bd001_w2_1 == 2), 
#      table(w2_bd001_w2_4, w2_bd001_w2_3, useNA = "ifany"))
# 
# charls %>%
#   select(ID, w1_bd001, w2_bd001_w2_4, w3_bd001_w2_4, w4_bd001_w2_4) %>%
#   mutate(w2_bd001_w2_4 = case_when(w2_bd001_w2_4 == 12 ~ w1_bd001,
#                                    TRUE ~ w2_bd001_w2_4),
#          w3_bd001_w2_4 = case_when(w3_bd001_w2_4 == 12 ~ w2_bd001_w2_4,
#                                    TRUE ~ w3_bd001_w2_4),
#          w4_bd001_w2_4 = case_when(w4_bd001_w2_4 == 12 ~ w3_bd001_w2_4,
#                                    TRUE ~ w4_bd001_w2_4)) %>%
#   pivot_longer(!ID, names_to = "wave", names_pattern = "w(.)_bd001*",
#                values_to = "edu_level") %>%
#   ggplot(aes(x = wave, y = edu_level, group = ID, 
#              color = as.factor(ID))) +
#   geom_line() +
#   geom_point() +
#   theme_classic() +
#   labs(x = "Wave",
#        y = "Edu level") +
#   theme(legend.position = "none")


#---- **** build the variable ----
# mutate(edu_highest = case_when(
#   # Using w1 education level,
#   # if w2 confirm w1's response is correct and
#   # if w2 confirms that w1's response doesn't include adult edu or
#   !is.na(w1_bd001) & ((w2_bd001_w2_1 == 1 & w2_bd001_w2_2 == 2) |
#     # if w2 confirms the edu didn't change from w1
#       w2_bd001_w2_4 == 12|
#       # if w3 confirms that w1's response doesn't change (w2 needs to be NA)
#       )
#      or
#     is.na(w2_bd001) & w3_bd001_w2_4 == 12|
#     # if w4 confirms that w1's response doesn't change (w3 needs to be 12 or NA)
#     
#     ~ w1_bd001, 
#   # Using w1 education level collected in w2 (w2_bd001_w2_3),
#   # if w2 confirm w1's response is wrong or
#   # if w2 confirms that w1's response include adult edu
#   w2_bd001_w2_1 == 2 | w2_bd001_w2_2 == 1 ~ w2_bd001_w2_3,
#   # Using w2 education level (bd001)
#   # if w1 is missing 
#   is.na(w1_bd001) ~ w2_bd001,
#   # Set the education level to be 1 if the participants indicating them to be
#   # illiterate at wave 3 or wave 4
#   w3_bd001_w3_1 == 2  & w4_bd001_w3_1 == 2 ~ 1,
#   # Using w3 education level
#   # if w1 and w2 both missing
#   is.na(w1_bd001) & is.na(w2_bd001) ~ w3_bd001_w2_4,
#   
#   # Using w4 education level
#   # if w1, w2 and w3 both missing
#   is.na(w1_bd001) & is.na(w2_bd001) & is.na(w3_bd001_w2_4) ~ w4_bd001_w2_4,
#   
#   
#   # Using w1 education level 
#   # if all the other education related variables from further waves are missing
#   TRUE ~ w1_bd001
#   )) %>%
# # 
# 
# 
# # 12. No change
# !is.na(w4_bd001_w2_4) & w4_bd001_w2_4 != 12 ~ w4_bd001_w2_4,
# w4_bd001_w2_4 == 12 & !is.na(w3_bd001_w2_4) & w3_bd001_w2_4 != 12 ~ 
#   w3_bd001_w2_4,
# !is.na(w3_bd001_w2_4) & w3_bd001_w2_4 != 12 ~ w3_bd001_w2_4,
# w3_bd001_w2_4 == 12 & !is.na(w2_bd001_w2_4) & w2_bd001_w2_4 != 12 ~ 
#   w2_bd001_w2_4,
# !is.na(w2_bd001_w2_4) & w2_bd001_w2_4 != 12 ~ w2_bd001_w2_4,
# w2_bd001_w2_4 == 12 & !is.na(w1_bd001) ~ w1_bd001,
# !is.na(w2_bd001) & is.na(w1_bd001) ~ w2_bd001,
# !is.na(w1_bd001) ~ w1_bd001)) %>%

# 
# a <- charls %>% filter(is.na(edu_highest)) %>% select(contains("bd001"))
# table(rowSums(is.na(a)))
# view(a)
# 
# with(charls, table(w3_bd001_w3_1, w4_bd001_w3_1, useNA = "ifany"))
