# CHARLS_data_construction
# Created by: Yingyan Wu
# Some code adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# Dec.25.2022

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "plyr", "tidyverse", "magrittr","haven", "survey", "glue",
       "labelled", "lubridate")
# Do not load "summarytools" as it contains conflicted functions from dplyr

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the datasets ----
##---- harmonized ----
harmonized_variables <- c("ID", "householdID", "pnc", "communityID",
                          "ID_w1", "householdID_w1", 
                          paste0("inw", 1:4),
                          paste0("r", 1:4, "iwy"),
                          paste0("r", 1:4, "iwm"),
                          paste0("r", 1:4, "iwstat"),
                          # Weights
                          paste0("r", 1:4, "wtrespb"),
                          "hacohort_c",
                          "rabyear", "rabmonth", "rabday", 
                          paste0("r", 1:4, "agey"),
                          "radyear", "radmonth",
                          "ragender",
                          "rabplace_c",
                          paste0("h", 1:4, "rural"),
                          paste0("r", 1:4, "hukou"),
                          "raeduc_c",
                          paste0("r", 1:4, "mstat"),
                          paste0("r", 1:4, "lbrf_c"), # labor force status
                          paste0("r", 1:4, "imrc"), # immediate word recall
                          paste0("r", 1:4, "dlrc"), # delayed word recall
                          # parent's edu & occupation
                          "rameduc_c", "rafeduc_c", "ramomoccup_c", 
                          "radadoccup_c")

harmonized <- haven::read_dta(here::here("data", "raw_data", "harmonized",
                                         "H_CHARLS_D_Data.dta"),
                              col_select = all_of(harmonized_variables))
# view(harmonized %>%
#        filter(ID_w1 == ""))

# harmonized_selected <- harmonized %>% filter(ID_w1 != "")

##---- harmonized life history ----
harmonized_lh <- haven::read_dta(here::here("data", "raw_data", "harmonized",
                                            "H_CHARLS_LH_a.dta"))

##---- psu ----
# psuw1 <- haven::read_dta(here::here("data", "raw_data", "2011", 
#                                     "chinese_characters_clean", "psu.dta"))
# colnames(psuw1)
# 
# p_load("deeplr")
# psuw1 %<>%
#   mutate(province_eng = toEnglish2(
#     province, get_detect = T, 
#     auth_key = "a152826f-19ff-78ea-32a5-1b8a9b13a291:fx")$translation)
# 
# save(psuw1, file = here::here("data", "analysis_data", "psuw1.RData"))

load(here::here("data", "analysis_data", "psuw1.RData"))

harmonized %<>% left_join(psuw1 %>% select(communityID, province_eng), 
                          by = "communityID")
table(harmonized$province_eng,useNA = "ifany")

#---- Reading core data ----
##---- main datasets ----
# Adapted from Mingfei Dong's data reading script, 
# Crystal's reading result snippet from make_figures.R in exposure_trajectories
# and the IDRE website
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("demographic_background", "health_status_and_functioning", 
              "cognition",
              "work_retirement_and_pension", "work_retirement", "sample_infor",
              "family_information", "weights")
folder_index <- c(1:length(folders))[-c(1, 3, 5, 9)] # main waves only

# Read in function
read_charls <- function(paths){
  if (str_detect(paths, "2011") & str_detect(paths, "weights")) {
    haven::read_dta(paths)
  } else if (str_detect(paths, "2011")) {
    haven::read_dta(paths) %>%
      select(-c("proxy")) # pending: can't figure out how to rename proxy automatically
  } else if (str_detect(paths, "Health_Status_and_Functioning") &
             str_detect(paths, "2015")){
    haven::read_dta(paths) %>%
      select(-c("xrtype", "versionID", "proxy"))
  } else if (str_detect(paths, "Family_Information|Weights|Sample_Infor") &
             str_detect(paths, "2013|2015")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
    # } else if (str_detect(paths, "2015|2018") &
    #            str_detect(paths, "Sample_Infor")){
    #   haven::read_dta(paths) %>%
    #     select(-c("versionID"))
  } else if (str_detect(paths, "2013|2015")){
    haven::read_dta(paths) %>%
      select(-c("xrtype", "versionID"))
  } else if (str_detect(paths, "Family_Information") &
             str_detect(paths, "2018")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2018|2020") &
             str_detect(paths, "Weights|Sample_Infor")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2018") &
             str_detect(paths, "Health_Status_and_Functioning")){
    haven::read_dta(paths) %>%
      select(-c("versionID", "xrtype",
                "xrgender", "ziwtime"))
  } else if (str_detect(paths, "2018") &
             str_detect(paths, "Work_Retirement")){
    haven::read_dta(paths) %>%
      select(-c("versionID", "xrtype"))
  } else if (str_detect(paths, "2018") &
             str_detect(paths, "Cognition")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2018")){
    haven::read_dta(paths) %>%
      select(-c("versionID",
                "xrgender", "ziwtime"))
  } else if (str_detect(paths, "2020") &
             str_detect(paths, "Health_Status_and_Functioning")){
    haven::read_dta(paths) %>%
      select(-c("xrage", "xrtype", "ziwtime", "versionID"))
  } else if (str_detect(paths, "2020") &
             str_detect(paths, "Work_Retirement")){
    haven::read_dta(paths) %>%
      select(-c("xrage", "ziwtime", "versionID"))
  } else if (str_detect(paths, "2020") &
             str_detect(paths, "Family_Information")){
    haven::read_dta(paths) %>%
      select(-c("zchildnum", "versionID"))
  } else {
    haven::read_dta(paths)
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
  # list <- lapply(files, read_dta)
  list <- lapply(files, read_charls)
  names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
  # Assign names for each wave's list
  assign(paste0("wave", which(folder_index == i)), list)
}

rm(list)

gc()

##---- life history datasets ----
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("residence", "demographic_background", "education_history", 
              "family_information")
folder_index <- c(1:length(folders))[5] # life history only
files <- list.files(path = folders[5], full.names = TRUE)
files <- files[str_detect(files, regex(glue_collapse(datasets, sep="|"), 
                                       ignore_case = T))]

read_charls_lh <- function(paths){
  if (str_detect(paths, "Residence")) {
    haven::read_dta(paths) %>% select(-rgender)
  } else {
    haven::read_dta(paths)
  }
}
# Read in as list
list <- lapply(files, read_charls_lh)
names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
# Assign names for each wave's list
assign("life_history", list)
rm(list)

## ---- join the datasets ----
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
                      by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w3_", x)})
# colnames(charls_w3)

charls_w4 <- join_all(wave4, 
                      by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w4_", x)})
# colnames(charls_w4)

wave5_ID <- wave5$Demographic_Background %>% select(ID, householdID)
wave5$Family_Information <- wave5_ID %>%
  left_join(wave5$Family_Information, by = "householdID", copy = T)
charls_w5 <- join_all(wave5, 
                      by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w5_", x)})

charls_lh <- join_all(life_history, 
                      by = c("ID", "householdID", "communityID", "versionID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID", "versionID"), 
              function(x){paste0("lh_", x)})

rm(life_history)
rm(list = paste0("wave", 1:5))
gc()

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

##----  join to a single dataset (wave 1 baseline) ----
# Merge all the waves (All the participants!)
charls <- join_all(c(list(charls_w1, charls_w2, charls_w3, charls_w4, charls_w5,
                          charls_lh)),
                   by = c("ID"), # "ID_w234" = "ID", # somehow it didn't work anymore. Rename the ID_w234 in w1 above
                   type = "full") %>% 
  as_tibble()
colnames(charls %>% select(contains(".x")))

charls %<>%
  left_join(harmonized %>% select(-householdID, -communityID, 
                                  -ID_w1), by = "ID")
colnames(charls %>% select(contains(".x")))

charls %<>%
  left_join(harmonized_lh %>% 
              dplyr::rename_with(.cols = starts_with("ra"), 
                                 function(x){paste0("lh_", x)}) %>%
              select(-c("householdID", "communityID")),
            by = "ID")
colnames(charls %>% select(contains(".x")))

rm(list = paste0("charls_", c(paste0("w", 1:5), "lh")))

charls %<>% remove_labels()
gc()
# proc_freq(harmonized, raeduc_c)

# # Sanity check
# diffdf::diffdf(harmonized_lh %>% filter(ID %in% harmonized$ID) %>% 
#                  select(ID, "rabmonth", "rabyear", "ragender", 
#                         "ramomoccup_c", "radadoccup_c"),
#                harmonized %>% filter(ID %in% harmonized_lh$ID) %>% 
#                  select(ID, "rabmonth", "rabyear", "ragender", 
#                         "ramomoccup_c", "radadoccup_c"))
# 
# with(charls, table(rabmonth == lh_rabmonth, is.na(rabmonth), useNA = "ifany"))
# with(charls, table(rabyear == lh_rabyear, is.na(rabyear), useNA = "ifany"))
# with(charls, table(ragender == lh_ragender, useNA = "ifany"))
# with(charls, table(ramomoccup_c == lh_ramomoccup_c, useNA = "ifany"))
# with(charls, table(radadoccup_c == lh_radadoccup_c, useNA = "ifany"))

# Filling NAs for birth month, birth year, gender in the harmonized dataset
# with values from life history dataset and wave 5 dataset (if available)
charls %<>%
  mutate(rabmonth_v2 = case_when(!is.na(rabmonth) ~ rabmonth,
                                 !is.na(lh_rabmonth) ~ lh_rabmonth,
                                 w5_ba003_2 != -1 ~ w5_ba003_2),
         rabyear_v2 = case_when(!is.na(rabyear) ~ rabyear,
                                !is.na(lh_rabyear) ~ lh_rabyear,
                                !is.na(w5_ba003_1) ~ w5_ba003_1),
         ragender_v2 = case_when(!is.na(ragender) ~ ragender,
                                 !is.na(lh_ragender) ~ lh_ragender,
                                 w5_ba001 == w5_ba002|is.na(w5_ba002) ~ 
                                   as.numeric(w5_ba001)),
         ramomoccup_c_v2 = case_when(!is.na(ramomoccup_c) ~ ramomoccup_c,
                                     !is.na(lh_ramomoccup_c) ~ lh_ramomoccup_c),
         radadoccup_c_v2 = case_when(!is.na(radadoccup_c) ~ radadoccup_c,
                                     !is.na(lh_radadoccup_c) ~ lh_radadoccup_c))

# # Sanity check
# with(charls, table(rabyear_v2 == lh_raidbyear, useNA = "ifany"))
# with(charls, table(is.na(rabmonth_v2) & !is.na(lh_raidbmonth),useNA = "ifany"))
# with(charls, table(is.na(rabyear_v2) & !is.na(lh_raidbyear),useNA = "ifany"))
# charls %>% filter(is.na(rabyear_v2) & !is.na(lh_raidbyear)) %>%
#   select(ID, rabmonth, rabyear, rabmonth_v2, rabyear_v2, lh_raidbmonth, lh_raidbyear,
#          contains("w5_ba003")) %>%
#   view()

#---- Age ----
# Wave 5 age
age_w5 <- charls %>% select(ID, contains("w5_ba003"), contains("xrage"),
                            w5_imonth, w5_iyear) %>%
  dplyr::mutate(w5_iwd = as.Date(paste0(w5_imonth, "/1/", w5_iyear), "%m/%d/%Y"),
                w5_rabdate = case_when(
                  w5_ba003_2 == -1 ~
                    as.Date(paste0("1/1/", w5_ba003_1), "%m/%d/%Y"),
                  w5_ba003_3 == -1 ~
                    as.Date(paste0(w5_ba003_2, "/1/", w5_ba003_1), "%m/%d/%Y"),
                  !is.na(w5_ba003_3) ~
                    as.Date(paste0(w5_ba003_2, "/", w5_ba003_3, "/", w5_ba003_1), 
                            "%m/%d/%Y")),
                r5agey = round(interval(start = w5_rabdate, 
                                        end = w5_iwd) / 
                                 duration(num = 1, units = "years")))
charls %<>%
  left_join(age_w5 %>% select(ID, r5agey), by = "ID")

charls %>% select(contains("agey")) %>%
  apply(., 2, proc_sum)

# Using birth year, birth month and interview dates to calculate age
charls %<>%
  # birth dates (assuming at the first day of the month)
  mutate(rabdate_v2 = as.Date(paste0(rabmonth_v2, "/1/", rabyear_v2),
                              "%m/%d/%Y"))
charls %<>%
  dplyr::rename(r5iwm = w5_imonth,
                r5iwy = w5_iyear)

for (i in 1:5){
  charls %<>%
    dplyr::mutate(
      # Interview dates (assuming at the first day of the month)
      !!paste0("r", i, "iwd_v2") := as.Date(
        paste0(!!sym(paste0("r", i, "iwm")), "/1/", 
               !!sym(paste0("r", i, "iwy"))), "%m/%d/%Y"),
      !!paste0("r", i, "agey_v2") := 
        case_when(!is.na(!!sym(paste0("r", i, "agey"))) ~ 
                    !!sym(paste0("r", i, "agey")),
                  is.na(!!sym(paste0("r", i, "agey"))) ~ 
                    round(interval(start = rabdate_v2, 
                                   end = !!sym(paste0("r", i, "iwd_v2"))) / 
                            duration(num = 1, units = "years"))))
}

# Sanity check
# Check for those with age missing, do they have interview dates and birth dates data
colSums(is.na(charls %>% filter(is.na(r2agey)) %>% 
                select(contains(c("iwd_v2", "rabdate_v2")))))

charls %>% select(contains("agey_v2")) %>%
  apply(., 2, proc_sum, more = T)

# For those who are still missing ages, since interview dates were missing, 
# impute them using the closest nonmissing values of age +/- 2 or +/-3 
# depending on waves: 2011, 2013, 2015, 2018, 2020
age_y <- charls %>% dplyr::select(contains("agey_v2"), -contains("LHagey")) %>% 
  apply(., 1, impute_ages, survey_years = c(2011, 2013, 2015, 2018, 2020)) %>% t()

charls[, paste0("r", 1:5, "agey_v3")] <- age_y
rm(age_y)

charls %>% select(contains("agey_v3")) %>%
  apply(., 2, proc_sum, more = T)

# check the birth date and the interview dates
with(charls %>% filter(is.na(r1agey_v3)),
     table(!is.na(r1iwd_v2), !is.na(rabdate_v2)))
with(charls %>% filter(is.na(r2agey_v3)),
     table(!is.na(r1iwd_v2), !is.na(rabdate_v2)))

# Check if interview dates were available for both core waves and lh waves
# for those still missing core wave age but has lh wave age
with(charls %>% filter(is.na(r4agey_v3) & !is.na(rLHagey)),
     table(!is.na(r4iwd_v2), !is.na(rLHiwm)))

missingage_id <- charls%>%
  filter(is.na(r1agey_v3) & is.na(r2agey_v3) & is.na(r3agey_v3) & 
           is.na(r4agey_v3)) %>%
  pull(ID)

charls_missingage <- charls %>%
  filter(ID %in% missingage_id) %>%
  select("ID", paste0("r", 1:4, "iwd_v2"), rabdate_v2, 
         # Weights
         paste0("r", 1:4, "wtrespb"),
         "ragender",
         "rabplace_c",
         paste0("h", 1:4, "rural"),
         paste0("r", 1:4, "hukou"),
         "raeduc_c",
         paste0("r", 1:4, "imrc"), # immediate word recall
         paste0("r", 1:4, "dlrc"), # delayed word recall
         # parent's edu & occupation
         "rameduc_c", "rafeduc_c", "ramomoccup_c", 
         "radadoccup_c")
view(charls_missingage)
# Will have to drop these people

#---- Gender ----
charls %<>% 
  mutate(female = case_when(ragender_v2 == 1 ~ 0,
                            ragender_v2 == 2 ~ 1),
         lh_female = case_when(lh_ragender == 1 ~ 0,
                               lh_ragender == 2 ~ 1),
         female_v2 = case_when(!is.na(female) ~ female,
                               is.na(female) ~ lh_female))
# sanity check
with(charls, table(female, ragender, useNA = "ifany"))
with(charls, table(female, lh_ragender, useNA = "ifany"))
with(charls, table(female, female_v2, useNA = "ifany"))
with(charls, table(female_v2, useNA = "ifany")) # 5 missing gender
# with(charls, table(female, w1_rgender, useNA = "ifany"))
# with(charls, table(female, w2_ba000_w2_3, useNA = "ifany"))
# with(charls, table(female, w2_xrgender, useNA = "ifany"))
# with(charls, table(female, w3_ba000_w2_3, useNA = "ifany"))
# with(charls, table(female, w3_xrgender, useNA = "ifany"))
# with(charls, table(female, w4_ba000_w2_3, useNA = "ifany"))
# with(charls, table(female, w4_zfrgender, useNA = "ifany"))

#Drop CHARLS harmonized gender variable
# charls %<>% dplyr::select(-ragender)

#---- Education variables ----
##---- Early life Education ----
charls %<>%
  mutate(r5educ_c = case_when(w5_ba010 %in% c(10, 11) ~ 10, # master + phd
                              TRUE ~ w5_ba010)) %>%
  mutate(raeduc_c_v2 = case_when(!is.na(raeduc_c) ~ raeduc_c,
                                 is.na(raeduc_c) ~ r5educ_c))
# Sanity check
with(charls, table(raeduc_c_v2, r5educ_c, useNA = "ifany"))

##---- Adult Education ----
# No adult education info available for wave 5 participants (it's ok, cuz for
# those who joined the cohort in wave 5, we'll have to exclude them anyway
# due to only one occurrence of cognitive test measures)
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
  mutate(adult_edu = case_when(
    # bd007's = 1 none adult education
    w4_bd007_w2_1 != 1|w3_bd007_w2_1 != 1|w2_bd007_w2_1 != 1|w2_bd007 != 1|
      w1_bd007 != 1 ~ 1,
    is.na(w4_bd007_w2_1) & is.na(w3_bd007_w2_1) & 
      is.na(w2_bd007_w2_1) & is.na(w2_bd007) & 
      is.na(w1_bd007) ~ NA_real_,
    TRUE ~ 0),
    # This needs to be before wave 1 (2011)
    adult_edu_dplm = case_when(
      # bd009's = 1: no diploma for adult education program
      (w4_bd009_w2_1 == 1|w3_bd009_w2_1 == 1|w2_bd009_w2_1 == 1|w2_bd009 == 1|
         w1_bd009 == 1) ~ 1, 
      is.na(w4_bd009_w2_1) & is.na(w3_bd009_w2_1) & 
        is.na(w2_bd009_w2_1) & is.na(w2_bd009) & 
        is.na(w1_bd009) ~ NA_real_,
      TRUE ~ 0))
# 
# # Check the time of getting the diploma
# charls %>%
#   filter(!is.na(adult_edu_dplm)) %>%
#   select(contains("bd010")) %>%
#   apply(., 2, proc_sum, more = T)
# 
# table(charls$adult_edu_dplm, useNA = "ifany")
# 
# with(charls %>%
#        filter(!is.na(adult_edu_dplm)), 
#      table(w4_bd010 <= 2011, useNA = "ifany"))
# 
# with(charls, table(w3_bd009_w2_1, w3_bd011, useNA = "ifany"))
# 
# proc_freq(charls, adult_edu)
# proc_freq(charls, adult_edu_dplm)
# with(charls, table(adult_edu, adult_edu_dplm, useNA = "ifany"))

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

##---- Edu + adult edu ----
charls %<>%
  mutate(
    childhood_edu = case_when(raeduc_c_v2 == 1 ~ 0,
                              raeduc_c_v2 %in% c(2, 3, 4) ~ 1,
                              raeduc_c_v2 %in% c(5, 6, 7) ~ 2,
                              raeduc_c_v2 %in% c(8, 9, 10) ~ 3),
    continuing_edu = case_when(adult_edu == 0 ~ 0,
                               adult_edu == 1 & adult_edu_dplm == 0 ~ 1,
                               adult_edu == 1 & adult_edu_dplm == 1 ~ 2),
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
      childhood_edu == 3 & continuing_edu == 2 ~ 11)) %>%
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

# Sanity check
with(charls, table(edu_cat, raeduc_c_v2, useNA = "ifany"))
with(charls, table(continuing_edu, adult_edu_dplm, useNA = "ifany"))
with(charls %>% filter(is.na(edu_cat) & raeduc_c_v2 == 1), 
     table(adult_edu, adult_edu_dplm, useNA = "ifany"))

##---- Education interruption 1967 - 1977 ----
# For sensitivity analysis
# Pending
with(charls %>% filter(r1agey_v3 >= 45), 
     table(rabyear_v2, lh_e002, useNA = "ifany"))
proc_freq(charls, lh_e002)
with(charls, table(lh_e003a, lh_e002, useNA = "ifany"))
with(charls, table(lh_e004, lh_e002, useNA = "ifany"))
length(charls %>% filter(rabyear <= 1962 & rabyear >= 1946))
# charls %>% filter(rabyear <= 1962 & rabyear >= 1946)

# Since the life history interviewees all have non missing rLHiwm and rLHiwy
# these serve as indicators
with(charls %>% filter(!is.na(rLHiwm)),
     table(lh_e004, lh_e002, useNA = "ifany"))

#---- Outcome ----
##---- Wave 5 ----
with(charls, table(w5_dc012_s11, w5_dc012_s12, useNA = "ifany"))
# There are 1244 participants with no responses (can't recall any of the words)
# but there are 427 participants refused to recall

w5rc_mat <- charls %>% select(
  contains(c("w5_dc012_","w5_dc013_", "w5_dc014_", # Immediate recall score
             "w5_dc028_"))) # Delayed recall score
colnames(w5rc_mat)
# put every non missing and non zero value to be 1 besides s12
w5rc_mat[w5rc_mat != 0 & !is.na(w5rc_mat)] <- 1
w5rc_mat %<>%
  mutate(across(ends_with("s11"), ~ case_when(.x == 0 ~ NA_real_,
                                              .x == 11 ~ 0)))

charls %<>%
  mutate(r5imrc_cleaned = case_when(
    w5_dc012_s11 == 11 ~ 0,
    w5_dc012_s12 == 12 ~ NA_real_,
    is.na(w5_dc012_s11) & is.na(w5_dc012_s12) ~ NA_real_,
    TRUE ~ rowSums(w5rc_mat %>% select(
      contains("w5_dc012_s"), -contains(c("s12", "s11"))), na.rm = T)),
    r5imrc_trial2 = case_when(
      w5_dc013_s11 == 11 ~ 0,
      w5_dc013_s12 == 12 ~ NA_real_,
      is.na(w5_dc013_s11) & is.na(w5_dc013_s12) ~ NA_real_,
      TRUE ~ rowSums(w5rc_mat %>% select(
        contains("w5_dc013_s"), -contains(c("s12", "s11"))), na.rm = T)),
    r5imrc_trial3 = case_when(
      w5_dc014_s11 == 11 ~ 0,
      w5_dc014_s12 == 12 ~ NA_real_,
      is.na(w5_dc014_s11) & is.na(w5_dc014_s12) ~ NA_real_,
      TRUE ~ rowSums(w5rc_mat %>% select(
        contains("w5_dc014_s"), -contains(c("s12", "s11"))), na.rm = T)),
    r5dlrc_cleaned = case_when(
      w5_dc028_s11 == 11 ~ 0,
      w5_dc028_s12 == 12 ~ NA_real_,
      is.na(w5_dc028_s11) & is.na(w5_dc028_s12) ~ NA_real_,
      TRUE ~ rowSums(
        w5rc_mat %>% 
          select(contains("dc028_s"), -contains(c("s12", "s11"))), na.rm = T))
  )

with(charls, table(r5imrc_cleaned, useNA = "ifany"))
charls %>% select(contains(c("imrc", "dlrc"))) %>% summary()
with(charls, table(r5dlrc_cleaned, useNA = "ifany"))

##---- Cleaning Immediate word recall ----
# Clean the 0s and NAs (separated refuse to answer variable not available in wave 1 and 2)
with(charls %>% filter(r3imrc == 0),
     table(w3_dc006s11, w3_dc006s12, useNA = "ifany"))
with(charls %>% filter(r4imrc == 0),
     table(w4_dc028_w4_s11, w4_dc028_w4_s12, useNA = "ifany"))
# There are 1877 participants with non responses (can't recall any of the words)
# But the rest 653 participants refused to recall (needs to be NAed)

charls %<>% 
  mutate(
    r3imrc_cleaned = case_when(
      r3imrc == 0 & w3_dc006s12 == 12 ~ NA_real_,
      # Note: if using read.dta13, the value will be "12 Refuse to Recall" for dc006s12s
      TRUE ~ as.numeric(r3imrc)),
    r4imrc_cleaned = case_when(
      r4imrc == 0 & w4_dc028_w4_s12 == 12 ~ NA_real_,
      TRUE ~ as.numeric(r4imrc)))
# Sanity check
with(charls, table(r3imrc_cleaned, r3imrc, useNA = "ifany"))
with(charls, table(r4imrc_cleaned, w4_dc028_w4_s12, useNA = "ifany"))    

##---- Cleaning Delayed word recall ----
with(charls %>% filter(r3dlrc == 0),
     table(w3_dc027s11, w3_dc027s12, useNA = "ifany"))
with(charls %>% filter(r4dlrc == 0),
     table(w4_dc047_w4_s11, w4_dc047_w4_s12, useNA = "ifany"))
# There are 2334 participants with non responses (can't recall any of the words)
# But the rest 505 participants seem to be NAed

charls %<>% 
  mutate(r3dlrc_cleaned = case_when(r3dlrc == 0 & w3_dc027s12 == 12 ~ NA_real_,
                                    TRUE ~ r3dlrc),
         r4dlrc_cleaned = case_when(r4dlrc == 0 & w4_dc047_w4_s12 == 12 ~ NA_real_,
                                    TRUE ~ r4dlrc))
# Sanity check
with(charls, table(r3dlrc_cleaned, r3dlrc, useNA = "ifany"))
with(charls, table(r4dlrc_cleaned, r4dlrc, useNA = "ifany"))
with(charls, table(r5dlrc_cleaned, useNA = "ifany"))

##---- Equated scores ----
# Same algorithm for equating for r4 and r5
eqt_table <- readxl::read_xlsx(here::here("output", "tables", 
                                          "table3_lumping_equating.xlsx")) %>%
  set_colnames(c("test", "original_score", "eqt_score")) %>%
  # set_colnames(c("test", "original_score", "r4", "r5")) %>%
  mutate(test = case_when(test == "Immediate" ~ "imrc",
                          test == "Delayed" ~ "dlrc")) %>%
  pivot_wider(id_cols = "original_score", names_from = "test", 
              values_from = "eqt_score", names_sep = "") %>%
  dplyr::rename_at(vars(ends_with("rc")), ~ str_c("r4", ., "_eqt")) %>%
  mutate(r5imrc_eqt = r4imrc_eqt,
         r5dlrc_eqt = r4dlrc_eqt)

charls %<>%
  left_join(eqt_table %>% select(original_score, r4imrc_eqt), 
            by = c("r4imrc_cleaned" = "original_score")) %>%
  left_join(eqt_table %>% select(original_score, r4dlrc_eqt), 
            by = c("r4dlrc_cleaned" = "original_score")) %>%
  left_join(eqt_table %>% select(original_score, r5imrc_eqt), 
            by = c("r5imrc_cleaned" = "original_score")) %>%
  left_join(eqt_table %>% select(original_score, r5dlrc_eqt), 
            by = c("r5dlrc_cleaned" = "original_score"))

# Can also use the eqted score dataset saved from equipercentile project
# The outcome was cleaned in another script (equipercentile equating method)
# Checked that the distribution of equated scores are the same
# load(here::here("data", "analysis_data", "eqt_rc.RData"))
# charls %<>% 
#   left_join(eqt_tib, by = "ID")

#---- Childhood Region of residence ----
# psu2011_nbs <- readxl::read_xlsx(here::here("data", "analysis_data", 
#                                             "psu2011_nbs.xlsx"))
# colnames(psu2011_nbs)
# 
# p_load("deeplr")
# psu2011_nbs %<>%
#   mutate(
#     province_eng = toEnglish2(
#       province_CN, get_detect = T,
#       auth_key = "a152826f-19ff-78ea-32a5-1b8a9b13a291:fx")$translation)
# 
# save(psu2011_nbs, file = here::here("data", "analysis_data", "psu2011_nbs.RData"))

load(here::here("data", "analysis_data", "psu2011_nbs.RData"))
psu2011_short <- psu2011_nbs %>%
  mutate(PSU_code_short = substr(as.character(PSU_code), 1, 2)) %>%
  select(-PSU_code, -city_CN) %>%
  unique()

proc_freq(charls, lh_t002_psu_1_) %>% print(n = Inf)
# These code corresponds to the 2011 NBS area code
charls %>% select(contains("lh_t002_psu_")) %>%
  apply(., 2, function(x) table(x, useNA = "ifany"))

childhood_resi_short <- charls %>% select(paste0("lh_t002_psu_", 1:25, "_")) %>%
  mutate(across(all_of(paste0("lh_t002_psu_", 1:25, "_")), 
                function(x) substr(as.character(x), 1, 2)))
charls[, paste0("lh_t002_psu_", 1:25, "_short")] <- childhood_resi_short

charls %<>%
  left_join(psu2011_short %>% select(PSU_code_short, province_eng) %>%
              dplyr::rename(lh_t002_province_1_ = province_eng),
            by = c("lh_t002_psu_1_short" = "PSU_code_short"))

# Sanity check
# colnames(charls %>% select(contains(".x")))
# with(charls, table(is.na(lh_t002_psu_1_), is.na(lh_t002_province_1_)))
# with(charls %>%
#   select("lh_t002_province_1_", lh_t002_psu_1_) %>%
#   filter(!is.na(lh_t002_psu_1_) & is.na(lh_t002_province_1_)),
#   table(lh_t002_psu_1_))
# proc_freq(charls, lh_t002_province_1_) %>% print(n = Inf)
# with(charls, table(is.na(lh_t002_psu_1_short), is.na(lh_t002_psu_5_short)))

charls %<>%
  mutate(childhood_1st_resi = lh_t002_province_1_,
         childhood_1st_region = case_when(
           str_detect(childhood_1st_resi, "Beijing|Tianjin|Hebei|Shanghai|Jiangsu|Zhejiang|
               Fujian|Shandong|Guangdong|Hainan") ~ 1,
           str_detect(childhood_1st_resi, "Shanxi|Anhui|Jiangxi|Henan|Hubei|Hunan") ~ 2,
           str_detect(childhood_1st_resi, "Mongolia|Guangxi|Chongqing|Sichuan|Guizhou|Yunnan|
               Tibet|Shaanxi|Gansu|Qinghai|Ningxia|Xinjiang") ~ 3,
           str_detect(childhood_1st_resi,"Liaoning|Jilin|Heilongjiang") ~ 4))

#---- Childhood Rural/urban residence ----
colnames(charls %>% select(contains("bb006")))
proc_freq(charls, w1_bb006)
proc_freq(charls, w2_bb006)
with(charls, table(w1_bb006, w2_bb006, useNA = "ifany"))
with(charls %>% filter(is.na(w1_bb006) & !is.na(w2_bb006)),
     table(inw1, inw2, useNA = "ifany"))

charls %<>%
  mutate(rarural_child = case_when(!is.na(w1_bb006) ~ w1_bb006,
                                   !is.na(w2_bb006) ~ w2_bb006)) %>%
  mutate(rarural_child = case_when(rarural_child == 2 ~ 0,
                                   TRUE ~ rarural_child))

# Sanity check
proc_freq(charls, rarural_child)
# with(charls, table(rarural_child, childhood_1st_resi, useNA = "ifany"))

#---- Parental socioeconomic status ----
charls %<>% mutate(rameduc_c = case_when(rameduc_c == 1 ~ 0,
                                         rameduc_c %in% c(2, 3, 4) ~ 1,
                                         rameduc_c %in% c(5, 6, 7) ~ 2,
                                         rameduc_c %in% c(8, 9, 10, 11) ~ 3),
                   rafeduc_c = case_when(rafeduc_c == 1 ~ 0,
                                         rafeduc_c %in% c(2, 3, 4) ~ 1,
                                         rafeduc_c %in% c(5, 6, 7) ~ 2,
                                         rafeduc_c %in% c(8, 9, 10, 11) ~ 3),
                   ramoccup_c = ramomoccup_c - 1,
                   rafoccup_c = radadoccup_c - 1)
# Sanity check
with(charls, table(ramoccup_c, ramomoccup_c, useNA = "ifany"))
with(charls, table(rafoccup_c, radadoccup_c, useNA = "ifany"))
with(charls, table(rameduc_c, useNA = "ifany"))

#---- birth order ----
# From the core dataset
# No detailed sibling information from wave 5    T^T
# But it's fine! (same reasons for childhood rural/urban residence)
# missingness not bad! MI can still save us.
charls %<>%
  mutate(
    first_child = case_when(
      w1_cc002_1 > 0 |w1_cc002_3 > 0|w1_cc004_1 > 0|w1_cc004_3 > 0|
        w2_cc002_1_1_ > 0|w2_cc002_3_1_ > 0|w2_cc004_1_1_ > 0|
        w2_cc004_3_1_ > 0 ~ 0,
      ((w1_cc002_1 == 0 & w1_cc002_3 == 0 & w1_cc004_1 == 0 & w1_cc004_3 == 0)|
         (w2_cc002_1_1_ == 0 & w2_cc002_3_1_ == 0 & w2_cc004_1_1_ == 0 &
            w2_cc004_3_1_ == 0)) ~ 1,
      # lh_b2_1_1_ %in% c(2, 4) ~ 1,
      w3_xsiblingnum == 0 ~ 1,
      w4_cc000_w4_1 == 0 ~ 1,
      is.na(w1_cc002_1) & is.na(w1_cc002_3) & is.na(w1_cc004_1) &
        is.na(w1_cc004_3) & is.na(w2_cc002_1_1_) & is.na(w2_cc002_3_1_) &
        is.na(w2_cc004_1_1_) & is.na(w2_cc004_3_1_) ~ NA_real_
      # TRUE ~ 1
    ))

# # Sanity check
# proc_freq(charls, first_child)
# proc_freq(charls, lh_b1)
# charls %>% select(contains("lh_b2_1_")) %>%
#   apply(., 2, function(x) table(x, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_2_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_3_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_4_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_5_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_6_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_7_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_8_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_9_, useNA = "ifany"))
# with(charls, table(lh_b2_1_1_, lh_b2_1_10_, useNA = "ifany"))
# with(charls, table(first_child, w3_xsiblingnum, useNA = "ifany"))
# sib_mat_long <- charls %>% select(ID, contains("lh_b4_3_")) %>%
#   pivot_longer(cols = !ID, 
#                names_to = "sibling_num",
#                values_to = "rsibbyear",
#                names_pattern = "lh_b4_3_(.)_") %>%
#   filter(!is.na(sibling_num) & !is.na(rsibbyear)) %>%
#   arrange(ID, rsibbyear)
# Sanity check
# colSums(is.na(sib_mat_long))
# sum(is.na(sib_mat_liv6 %>% select(-rabyear_v2, -first_child)))
# Equal if we didn't filter on nonmissing when creating long dataset

sib_mat_liv6 <- charls %>% select(ID, rabyear_v2, contains("lh_b4_3_")) %>%
  mutate(nonNA_1st_sibbyear = coalesce(
    lh_b4_3_1_, lh_b4_3_2_, lh_b4_3_3_, lh_b4_3_4_, lh_b4_3_5_, lh_b4_3_6_, 
    lh_b4_3_7_, lh_b4_3_8_, lh_b4_3_9_, lh_b4_3_10_, lh_b4_3_11_, lh_b4_3_12_,
    lh_b4_3_13_, lh_b4_3_14_, lh_b4_3_15_), 
    first_child = case_when(rabyear_v2 <= nonNA_1st_sibbyear ~ 1,
                            rabyear_v2 > nonNA_1st_sibbyear ~ 0))

# with(sib_mat_liv6, table(nonNA_1st_sibbyear, useNA = "ifany"))
# with(sib_mat_liv6, table(lh_b4_3_1_, useNA = "ifany"))
# view(sib_mat_liv6 %>% filter(is.na(first_child) & !is.na(rabyear_v2)))
# colSums(is.na(sib_mat_liv6 %>% filter(is.na(first_child) & !is.na(rabyear_v2)) %>%
#                 select(contains("lh_b4_3_"))))

charls$first_child_lh <- sib_mat_liv6$first_child

# # Sanity check
# with(charls, table(first_child_lh, first_child, useNA = "ifany"))
# with(charls, table(first_child, useNA = "ifany"))
# with(charls, table(first_child_lh, useNA = "ifany"))
# # For those who got 0,1 and 1,0
# view(charls %>% filter(
#   # (first_child == 0 & first_child_lh == 1)|
#   (first_child == 1 & first_child_lh == 0)) %>%
#     select(ID, rabyear_v2, contains("lh_b4_3_"), 
#            w1_cc002_1, w1_cc002_3, w1_cc004_1, w1_cc004_3, 
#            w2_cc002_1_1_, w2_cc002_3_1_, w2_cc004_1_1_, w2_cc004_3_1_,
#            w3_xsiblingnum,
#            w4_cc000_w4_1))

# Stick with the lh version for first_child, if it's missing, supplement the
# values with the version derived from core datasets
charls %<>%
  mutate(first_child_v2 = case_when(!is.na(first_child_lh) ~ first_child_lh,
                                    is.na(first_child_lh) ~ first_child))

with(charls, table(first_child_lh, first_child_v2, useNA = "ifany"))
proc_freq(charls, first_child_v2)

# # There are some weird people
# # e.g. The person below: Every year after she was born, there was a sibling born.
# view(charls %>% filter(ID == "056346102001") %>%
#        select(ID, female, rabyear, edu_cat,first_child_v2, contains("lh_b4_3_")))
# 
# view(charls %>% filter(first_child_v2 == 1) %>%
#        select(ID, female, rabyear_v2, edu_cat, first_child_v2, 
#               contains("lh_b4_3_")))

#---- Working history from harmonized LH ----
work_history_long_mat <- charls %>%
  select(ID, rLHagey, contains("c_wrkstate")) %>%
  pivot_longer(!c(ID, rLHagey), 
               names_to = "Age",
               values_to = "wrkstate",
               names_pattern = "c_wrkstate(.*)") %>%
  mutate(Age = as.numeric(Age)) %>%
  filter(Age <= rLHagey | is.na(rLHagey)) # exclude the records of later age which participants
# have not reached (exclude the assigned NA records) but keep those who has
# NAed age at LH questionnaire

# Sanity check
with(work_history_long_mat, table(Age, useNA = "ifany"))

# A random sample of 100 people
set.seed("62283")
random_id <- sample(unique(harmonized_lh$ID), 100, replace = F)

work_history_long_mat %>%
  filter(ID %in% random_id) %>%
  ggplot(aes(x = Age, y = as.factor(wrkstate), group = ID, color = factor(ID))) +
  geom_line() +
  theme_classic() +
  labs(x = "Age (year)",
       y = "Work status",
       title = paste0("Profile Plot of Work status across different age",
                      "\nA random sample of 100 people")) +
  theme(legend.position = "none")

##---- First occupation type ----
# # First occupation mat
# work_history_wide_mat <- charls %>%
#   select(ID, rLHagey, contains("c_wrkstate"))
# with(work_history_wide_mat %>% filter(c_wrkstate15 == 9 & c_wrkstate16 == 9 & 
#                                         c_wrkstate17 == 9 & c_wrkstate18 == 9 &
#                                         c_wrkstate19 == 9 & c_wrkstate20 == 9 &
#                                         c_wrkstate21 == 9 & c_wrkstate22 == 9 &
#                                         c_wrkstate23 == 9 & c_wrkstate24 == 9), 
#      table(c_wrkstate25, c_wrkstate26, useNA = "ifany"))

first_work_mat <- work_history_long_mat %>%
  filter(wrkstate != 9 & wrkstate != 7 & !is.na(wrkstate)) %>%
  arrange(ID, Age) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::rename(first_occupation = wrkstate,
                first_occupation_age = Age)

# charls %<>% select(-c("first_occupation_age", "first_occupation"))
charls %<>%
  left_join(first_work_mat %>% select(ID, contains("first_occupation")),
            by = "ID")

proc_freq(first_work_mat, first_occupation)
proc_freq(charls, first_occupation)

# OLD
# proc_freq(charls, w1_fb002)
# proc_freq(charls, w2_fb002)
# with(charls, table(w2_fb002, w1_fb002, useNA = "ifany"))
# with(charls %>% filter(is.na(w1_fb002) & !is.na(w2_fb002)), 
#      table(inw1, inw2, useNA = "ifany"))
# 
# charls %<>%
#   mutate(first_occupation = case_when(!is.na(w1_fb002) ~ w1_fb002,
#                                       !is.na(w2_fb002) ~ w2_fb002))
# proc_freq(charls, first_occupation)
# colnames(charls %>% select(contains("fb002")))

# 2011 wave 1 labor force
# proc_freq(charls, r1lbrf_c)

##---- occupation at 30 (25 - 35 nonmissing closest to 30) -----
nonmissing_30_work_mat <- work_history_long_mat %>%
  filter(!is.na(wrkstate)) %>%
  mutate(age_30_abs = abs(Age - 30)) %>%
  filter(age_30_abs <= 5) %>%
  arrange(ID, age_30_abs) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::rename(occupation_30 = wrkstate,
                occupation_30_age = Age)

# Sanity check
# proc_freq(nonmissing_30_work_mat, occupation_30)
# proc_freq(nonmissing_30_work_mat, occupation_30_age)
# with(charls, table(is.na(c_wrkstate30)))
# OLD
# test <- nonmissing_30_work_mat %>% filter(occupation_30_age == "30") %>%
#   select(ID, occupation_30) %>% na.omit()
# test2 <-  charls %>% select(ID, c_wrkstate30) %>% na.omit()
# ID_set <- test2$ID[which(!test2$ID %in% test$ID)]
# view(work_history_long_mat %>% filter(ID %in% ID_set))
# view(test2 %>% filter(ID %in% ID_set))
# view(charls %>% filter(ID %in% ID_set) %>%
#        select(ID, rLHagey, contains("c_wrkstate")))

charls %<>% select(-contains("occupation_30"))
charls %<>%
  left_join(nonmissing_30_work_mat %>% select(ID, contains("occupation_30")),
            by = "ID")

proc_freq(charls, occupation_30)
proc_freq(charls, c_wrkstate30)

charls %<>%
  mutate(occupation_30 = case_when(occupation_30 %in% c(4, 5) ~ 4,
                                   occupation_30 %in% c(6, 7, 9) ~ 7,
                                   occupation_30 == 10 ~ 6,
                                   TRUE ~ occupation_30))
proc_freq(charls, occupation_30)
# 1.Agri employed |
# 2.Agri self-employed |
# 3.Non-agri employed |
# 4.Non-agri self-employed/family business  |
# 6. Other |
# 7.Unemployed/Retired/Education
# 8. Home |

#---- Marital history from harmonized LH ----
marital_long_mat <- charls %>%
  select(ID, rLHagey, contains("prtnstate")) %>%
  pivot_longer(!c(ID, rLHagey), 
               names_to = "Age",
               values_to = "prtnstate",
               names_pattern = "prtnstate(.*)") %>%
  mutate(Age = as.numeric(Age)) %>%
  filter(Age <= rLHagey | is.na(rLHagey)) 
# # exclude the records of later age which participants
# have not reached (remove assigned NA records)

# Sanity check
with(marital_long_mat, table(Age, useNA = "ifany"))

marital_long_mat %>%
  ggplot(aes(x = Age, y = as.factor(prtnstate), group = ID, color = factor(ID))) +
  geom_line() +
  theme_classic() +
  labs(x = "Age (year)",
       y = "Marital status",
       title = paste0("Profile Plot of Marital status across different age")) +
  theme(legend.position = "none")

##---- First non missing marital status ----
first_marital_mat <- marital_long_mat %>%
  filter(!is.na(prtnstate)) %>%
  arrange(ID, Age) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::rename(first_maritalsts = prtnstate,
                first_maritalsts_age = Age)

charls %<>% select(-contains("first_maritalsts"))
charls %<>%
  left_join(first_marital_mat %>% select(ID, contains("first_maritalsts")),
            by = "ID")
# Sanity check
proc_freq(charls, first_maritalsts)

##---- marital status 30 (25 - 35 nonmissing closest to 30) -----
nonmissing_30_marital_mat <- marital_long_mat %>%
  filter(!is.na(prtnstate)) %>%
  mutate(age_30_abs = abs(Age - 30)) %>%
  filter(age_30_abs <= 5) %>%
  arrange(ID, age_30_abs) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::rename(maritalsts_30 = prtnstate,
                maritalsts_30_age = Age)
# Sanity check
# proc_freq(nonmissing_30_marital_mat, maritalsts_30)
# proc_freq(nonmissing_30_marital_mat, maritalsts_30_age)
# with(charls, table(is.na(prtnstate30)))

charls %<>%
  left_join(nonmissing_30_marital_mat %>% select(ID, contains("maritalsts_30")),
            by = "ID")

charls %<>%
  mutate(marital_30 = maritalsts_30  - 1)
with(charls, table(marital_30, maritalsts_30, useNA = "ifany"))
proc_freq(charls, marital_30)

# OLD
# with(charls, table(first_maritalsts, prtnstate30, useNA = "ifany"))
# with(charls %>% filter(is.na(prtnstate30) & !is.na(first_maritalsts)),
#      table(first_maritalsts_age, useNA = "ifany"))

##---- wave 1 Marital status ----
charls %<>%
  mutate(r1mstat_c = case_when(r1mstat %in% c(1, 2) ~ 1,
                               r1mstat %in% c(4, 5) ~ 2,
                               r1mstat == 7 ~ 3,
                               r1mstat == 8 ~ 4),
         r1mstat_c_v2 = case_when(r1mstat %in% c(1, 2) ~ 1,
                                  r1mstat %in% c(4, 5, 8) ~ 2,
                                  r1mstat == 7 ~ 3))

proc_freq(charls, r1mstat_c)
proc_freq(charls, r1mstat_c_v2)

#---- Residence in adulthood ----
##---- Wave 1 Rural v.s.urban ----
proc_freq(charls, h1rural)
with(charls, table(h1rural, h2rural, useNA = "ifany"))
with(charls, table(h1rural, h3rural, useNA = "ifany"))

charls %<>%
  mutate(h5rural = case_when(w5_ba008 %in% c(3, 4) ~ 1,
                             !is.na(w5_ba008) ~ 0),
         hrural_v2 = case_when(
           !is.na(h1rural) ~ h1rural,
           is.na(h1rural) & !is.na(h2rural) ~ h2rural,
           is.na(h1rural) & is.na(h2rural) & !is.na(h3rural) ~ h3rural,
           is.na(h1rural) & is.na(h2rural) & is.na(h3rural) & !is.na(h4rural) ~ h4rural,
           is.na(h1rural) & is.na(h2rural) & is.na(h3rural) & is.na(h4rural) &
             !is.na(h5rural) ~ h5rural))

# Sanity check
proc_freq(charls, hrural_v2)
with(charls %>% filter(is.na(h1rural) & !is.na(h2rural)), 
     table(inw1, inw2, useNA = "ifany"))
with(charls %>% filter(is.na(h1rural) & !is.na(h3rural)), 
     table(inw1, inw3, useNA = "ifany"))
with(charls %>% filter(is.na(h1rural) & !is.na(h4rural)), 
     table(inw1, inw4, useNA = "ifany"))

##---- Province、region ----
proc_freq(charls, province_eng) %>% print(n = Inf)
charls %<>% mutate(
  region_eng = case_when(
    str_detect(province_eng, "Beijing|Tianjin|Hebei|Shanghai|Jiangsu|Zhejiang|
               Fujian|Shandong|Guangdong|Hainan") ~ 1, # East
    str_detect(province_eng, "Shanxi|Anhui|Jiangxi|Henan|Hubei|Hunan") ~ 2, # Central
    str_detect(province_eng, "Mongolia|Guangxi|Chongqing|Sichuan|Guizhou|Yunnan|
               Tibet|Shaanxi|Gansu|Qinghai|Ningxia|Xinjiang") ~ 3, # West
    str_detect(province_eng,"Liaoning|Jilin|Heilongjiang") ~ 4)) # North East

# Sanity check
proc_freq(charls, region_eng)
with(charls, table(region_eng, province_eng, useNA = "ifany"))

##---- Rural/urban residence 30 ----
residence_mat <- charls %>% select(contains(c("t001", "t002","t004")), 
                                   rabyear_v2, contains("rural")) %>%
  mutate(rayear_30 = rabyear_v2 + 30) %>%
  mutate(across(paste0("lh_t004_", 1:25, "_"),
                ~ case_when(. %in% c(1, 3, 5) ~ 1,
                            . %in% c(2, 4) ~ 0)))
# Sanity check
# table(residence_mat$lh_t004_1_, useNA = "ifany")
# table(charls$lh_t004_1_, useNA = "ifany")
# with(residence_mat, table(abs(rabyear_v2 - lh_t001b_1_10_) <= 5, useNA = "ifany"))

residence_mat %<>%
  mutate(rural_30 = case_when(
    # residence at 25 - 35 (2nd reported residence)
    abs(rabyear_v2 - lh_t001b_1_1_) <= 5 ~ lh_t004_1_,
    # residence at 25 - 35 (3nd reported residence)
    abs(rabyear_v2 - lh_t001b_1_2_) <= 5 ~ lh_t004_2_,
    # residence at 25 - 35 (4th reported residence)
    abs(rabyear_v2 - lh_t001b_1_3_) <= 5 ~ lh_t004_3_,
    # residence at 25 - 35 (5th reported residence)
    abs(rabyear_v2 - lh_t001b_1_4_) <= 5 ~ lh_t004_4_,
    # residence at 25 - 35 (6th reported residence)
    abs(rabyear_v2 - lh_t001b_1_5_) <= 5 ~ lh_t004_5_,
    # residence at 25 - 35 (7th reported residence)
    abs(rabyear_v2 - lh_t001b_1_6_) <= 5 ~ lh_t004_6_,
    lh_t001b_1_ == 2 | lh_t001b_2_ == 2 | lh_t001b_3_ == 2 | lh_t001b_4_ == 2 |
      lh_t001b_5_ == 2 | lh_t001b_6_ == 2 ~ hrural_v2))

# Sanity check
# proc_freq(residence_mat, rural_30)
# with(residence_mat, table(rural_30, hrural_v2, useNA = "ifany"))
# with(residence_mat, table(rural_30, h1rural, useNA = "ifany"))
# with(residence_mat, table(rural_30, lh_t004_1_, useNA = "ifany"))

# residence_mat %<>%
#   mutate(lh_t001_date = case_when(
#     !is.na(lh_t001b_2_1_) ~ 
#       as.Date(paste0(lh_t001b_2_1_, "/1/", lh_t001b_1_1_), "%m/%d/%Y"),
#     # Season
#     is.na(lh_t001b_2_1_) & lh_t001b_3_1_ == 1 ~ # Spring
#       as.Date(paste0(3, "/1/", lh_t001b_1_1_), "%m/%d/%Y"),
#     is.na(lh_t001b_2_1_) & lh_t001b_3_1_ == 2 ~ # Summer
#       as.Date(paste0(6, "/1/", lh_t001b_1_1_), "%m/%d/%Y"),
#     is.na(lh_t001b_2_1_) & lh_t001b_3_1_ == 3 ~ # Fall
#       as.Date(paste0(9, "/1/", lh_t001b_1_1_), "%m/%d/%Y"),
#     is.na(lh_t001b_2_1_) & lh_t001b_3_1_ == 4 ~ # Winter
#       as.Date(paste0(12, "/1/", lh_t001b_1_1_), "%m/%d/%Y"),
#     # first half or second half of a year
#     is.na(lh_t001b_2_1_) & is.na(lh_t001b_3_1_) & lh_t001b_4_1_ == 1 ~ # 1st half
#       as.Date(paste0(4, "/1/", lh_t001b_1_1_), "%m/%d/%Y"),
#     is.na(lh_t001b_2_1_) & is.na(lh_t001b_3_1_) & lh_t001b_4_1_ == 2 ~ # 2nd half
#       as.Date(paste0(9, "/1/", lh_t001b_1_1_), "%m/%d/%Y")))

# with(residence_mat %>% filter(is.na(lh_t001_date)), 
#      table(lh_t001b_1_, useNA = "ifany")) # 13425 participants didn't move
# with(residence_mat %>% filter(lh_t001b_1_ == 2),
#      table(hrural_v2, useNA = "ifany")) # 13244 

# with(residence_mat, table(lh_t001b_1_, useNA = "ifany"))

# with(residence_mat %>% filter(is.na(lh_t001_date)),
#      table(lh_t002e_1_1_, useNA = "ifany"))
# with(residence_mat %>% filter(!is.na(lh_t001_date)), 
#      table(lh_t001b_1_, useNA = "ifany"))

charls$rural_30 <- residence_mat$rural_30

#---- Hukou history from harmonized LH ----
hukou_long_mat <- charls %>%
  select(ID, rLHagey, contains("hkstate")) %>%
  pivot_longer(!c(ID, rLHagey), 
               names_to = "Age",
               values_to = "hkstate",
               names_pattern = "hkstate(.*)") %>%
  mutate(Age = as.numeric(Age),
         # Set those reported to not having hukou to be NA
         hkstate = case_when(hkstate == 4 ~ NA,
                             TRUE ~ hkstate)) %>%
  filter(Age <= rLHagey |is.na(rLHagey)) # exclude the records of later age which participants
# have not reached (assigned NA records)

# Sanity check
with(hukou_long_mat, table(Age, useNA = "ifany"))

hukou_long_mat %>%
  ggplot(aes(x = Age, y = as.factor(hkstate), group = ID, color = factor(ID))) +
  geom_line() +
  theme_classic() +
  labs(x = "Age (year)",
       y = "Hukou status",
       title = paste0("Profile Plot of Hukou status across different age")) +
  theme(legend.position = "none")

##---- First hukou type ----
first_hukou_mat <- hukou_long_mat %>%
  filter(!is.na(hkstate)) %>%
  arrange(ID, Age) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::rename(first_hukou = hkstate,
                first_hukou_age = Age)

# charls %<>% select(-contains("first_hukou"))
charls %<>%
  left_join(first_hukou_mat %>% select(ID, contains("first_hukou")),
            by = "ID")

# Sanity check
proc_freq(first_hukou_mat, first_hukou)
proc_freq(charls, first_hukou)

##---- Hukou status at 30 (25-35 nonmissing closest to 30) -----
nonmissing_30_hukou_mat <- hukou_long_mat %>%
  filter(!is.na(hkstate)) %>%
  mutate(age_30_abs = abs(Age - 30)) %>%
  filter(age_30_abs <= 5) %>%
  arrange(ID, age_30_abs) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::rename(hukou_30 = hkstate,
                hukou_30_age = Age)
# # Sanity check
# proc_freq(nonmissing_30_hukou_mat, hukou_30)
# proc_freq(nonmissing_30_hukou_mat, hukou_30_age)
# with(charls, table(is.na(hkstate30)))

# charls %<>% select(-contains("hukou_30"))
charls %<>%
  left_join(nonmissing_30_hukou_mat %>% select(ID, contains("hukou_30")),
            by = "ID")

charls %<>%
  mutate(hukou_30 = case_when(hukou_30 == 4 ~ NA_real_,
                              TRUE ~ hukou_30))
with(charls, table(hkstate30, hukou_30, useNA = "ifany"))
proc_freq(charls, hukou_30)

#---- Number of children at 30 ----
childnum_long_mat <- charls %>%
  select(ID, rLHagey, contains("chldstate")) %>%
  pivot_longer(!c(ID, rLHagey), 
               names_to = "Age",
               values_to = "chldstate",
               names_pattern = "chldstate(.*)") %>%
  mutate(Age = as.numeric(Age)) %>%
  filter(Age <= rLHagey | is.na(rLHagey)) 

nonmissing_30_child_mat <- childnum_long_mat %>%
  filter(!is.na(chldstate)) %>%
  mutate(age_30_abs = abs(Age - 30)) %>%
  filter(age_30_abs <= 5) %>%
  arrange(ID, age_30_abs) %>%
  group_by(ID) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::rename(chldstate_30 = chldstate,
                chldstate_30_age = Age)

charls %<>%
  left_join(nonmissing_30_child_mat %>% select(ID, contains("chldstate_30")),
            by = "ID")

proc_freq(charls, chldstate30)
with(charls, table(chldstate_30, chldstate30, useNA = "ifany"))

#---- Weight ----
charls %<>% dplyr::rename(r5wtrespb = w5_INDV_weight_ad2)

#---- Dropping people ----
##---- 1. Dropping those who still missing age and gender ----
# test <- charls %>%
#   filter(is.na(female_v2) & is.na(r1agey_v3)) %>%
#   select(contains("edu"), contains("imrc"), contains("dlrc"))
# table(colSums(is.na(test)))

# These 5 people did not have the outcome measures and the eductaion variables
charls %>%
  filter(is.na(female_v2) & is.na(r1agey_v3)) %>% nrow()
charls_selected <- charls %>%
  filter(!is.na(female_v2) | !is.na(r1agey_v3))

##---- 2. Dropping those who did not have any word recall scores ----
# n = 1979
charls_selected %>% 
  filter((is.na(r1imrc) & is.na(r2imrc) & is.na(r3imrc_cleaned) & is.na(r4imrc_cleaned) &
            is.na(r5imrc_cleaned)) |
           (is.na(r1dlrc) & is.na(r2dlrc) & is.na(r3dlrc_cleaned) & is.na(r4dlrc_cleaned) &
              is.na(r5dlrc_cleaned))) %>% 
  nrow()
charls_selected %<>% 
  filter(!((is.na(r1imrc) & is.na(r2imrc) & is.na(r3imrc_cleaned) & is.na(r4imrc_cleaned) &
              is.na(r5imrc_cleaned)) |
             (is.na(r1dlrc) & is.na(r2dlrc) & is.na(r3dlrc_cleaned) & is.na(r4dlrc_cleaned) &
                is.na(r5dlrc_cleaned))))

##---- Select variables ----
charls_selected %<>%
  select(ID, 
         # Weights
         paste0("r", 1:5, "wtrespb"),
         # Age and gender
         paste0("r", 1:5, "agey_v3"), female_v2, 
         # Education
         childhood_edu, continuing_edu, edu_cat,
         # Word recall scores
         contains("imrc"), contains("dlrc"), 
         # Childhood residence
         childhood_1st_resi, childhood_1st_region, rarural_child, 
         # Parental SES
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c, 
         # Birth order
         first_child_v2, 
         # Occupation
         contains("first_occupation"), contains("occupation_30"), 
         # Marital status
         contains("first_maritalsts"), marital_30, maritalsts_30_age, 
         # Adulthood Rural and hukou
         paste0("h", 1:5, "rural"), region_eng, province_eng, rural_30, 
         contains("first_hukou"), contains("hukou_30"), 
         # Number of children at 30 years old
         contains("chldstate_30"))

#---- Save the data ----
save(charls, 
     file = here::here("data", "analysis_data", "charls_cleaned.RData"))
save(charls_selected, 
     file = here::here("data", "analysis_data", "charls_selected.RData"))

#---- OLD ----
# #---- Hukou status ----
# proc_freq(charls, r1hukou)
# proc_freq(charls, lh_hukoutype) # First hukou status
# 
# with(charls, table(r1hukou, r2hukou, useNA = "ifany"))
# with(charls %>% filter(is.na(r1hukou) & !is.na(r2hukou)), 
#      table(inw1, inw2, useNA = "ifany"))
# with(charls %>% filter(is.na(r1hukou) & !is.na(r3hukou)), 
#      table(inw1, inw3, useNA = "ifany"))
# with(charls %>% filter(is.na(r1hukou) & !is.na(r4hukou)), 
#      table(inw1, inw4, useNA = "ifany"))
# 
# charls %<>%
#   mutate(hukou_flag = case_when(
#     r1hukou == r2hukou & r1hukou == r3hukou &
#       r1hukou == r4hukou ~ 0,
#     is.na(r1hukou) & is.na(r2hukou) & is.na(r3hukou) &
#       is.na(r4hukou) & is.na(lh_hukoutype) ~ NA_real_,
#     TRUE ~ 1))
# 
# proc_freq(charls, hukou_flag)
# hukou_mat <- charls %>% select(contains("hukou")) %>% select(-hukou_flag)
# colSums(is.na(hukou_mat))
# #---- Outcome: Word recall ----
# # immediate word recall
# with(charls %>% filter(r3imrc == 0),
#      table(w3_dc006s11, w3_dc006s12, useNA = "ifany"))
# with(charls %>% filter(r4imrc == 0),
#      table(w4_dc028_w4_s11, w4_dc028_w4_s12, useNA = "ifany"))
# # There are 1877 participants with non responses (can't recall any of the words)
# # But the rest 653 participants seem to be NAed
# 
# charls %<>% 
#   mutate(r3imrc_cleaned = case_when(r3imrc == 0 & w3_dc006s12 == 12 ~ NA_real_,
#                                     TRUE ~ r3imrc),
#          r4imrc_cleaned = case_when(r4imrc == 0 & w4_dc028_w4_s12 == 12 ~ NA_real_,
#                                     TRUE ~ r4imrc))
# # Sanity check
# with(charls, table(r3imrc_cleaned, r3imrc, useNA = "ifany"))
# with(charls, table(r4imrc_cleaned, r4imrc, useNA = "ifany"))
# 
# # Delayed word recall
# with(charls %>% filter(r3dlrc == 0),
#      table(w3_dc027s11, w3_dc027s12, useNA = "ifany"))
# with(charls %>% filter(r4dlrc == 0),
#      table(w4_dc047_w4_s11, w4_dc047_w4_s12, useNA = "ifany"))
# # There are 2334 participants with non responses (can't recall any of the words)
# # But the rest 505 participants seem to be NAed
# 
# charls %<>% 
#   mutate(r3dlrc_cleaned = case_when(r3dlrc == 0 & w3_dc027s12 == 12 ~ NA_real_,
#                                     TRUE ~ r3dlrc),
#          r4dlrc_cleaned = case_when(r4dlrc == 0 & w4_dc047_w4_s12 == 12 ~ NA_real_,
#                                     TRUE ~ r4dlrc))
# # Sanity check
# with(charls, table(r3dlrc_cleaned, r3dlrc, useNA = "ifany"))
# with(charls, table(r4dlrc_cleaned, r4dlrc, useNA = "ifany"))
# ##---- Adult edu time ----
# proc_univariate(charls$w1_bd010)
# with(charls %>% filter(adult_edu == 1), 
#      table(w1_bd011, adult_edu_dplm,useNA = "ifany"))
# with(charls %>% filter(adult_edu == 1), 
#      table(w2_bd011, adult_edu_dplm,useNA = "ifany"))
# with(charls %>% filter(adult_edu == 1), 
#      table(w3_bd011, adult_edu_dplm,useNA = "ifany"))
# with(charls %>% filter(adult_edu == 1), 
#      table(w4_bd011, adult_edu_dplm,useNA = "ifany"))
# 
# charls %>% filter(w3_bd011 == 2 & adult_edu_dplm == 0) %>% 
#   select(contains(c("bd009", "bd010", "edu")))
# 
# # All those indicate adult degree as others, set them to be NA
# charls %<>% 
#   mutate(across(.cols = ends_with("bd011"), 
#                 ~ case_when(. == 4 ~ NA_real_,
#                             TRUE ~ .)))
# # Year of the highest adult education degree obtained
# with(charls, table(adult_edu_dplm, (!is.na(w1_bd010)|!is.na(w2_bd010)|
#                                       !is.na(w3_bd010)|!is.na(w4_bd010)),
#                    useNA = "ifany"))
# 
# # charls %<>% mutate(
# #   adult_edu_dplm_year = case_when(!is.na(w1_bd010) ~ w1_bd010,
# #                                   !is.na(w2_bd010) ~ w2_bd010,
# #                                   !is.na(w3_bd010) ~ w3_bd010,
# #                                   !is.na(w4_bd010) ~ w4_bd010),
# #   adult_edu_start_year = case_when(!is.na(w1_bd010) ~ w1_bd010 - w1_bd008,
# #                                    !is.na(w2_bd010) ~ w2_bd010 - w2_bd008,
# #                                    !is.na(w3_bd010) ~ w3_bd010 - w3_bd008,
# #                                    !is.na(w4_bd010) ~ w4_bd010 - w4_bd008_w2_1)) %>%
# #   mutate(adult_edu_dplm_year = case_when(adult_edu_dplm_year > 2010 ~ NA_real_,
# #                                          TRUE ~ adult_edu_dplm_year),
# #          age_adult_edu_dplm = adult_edu_dplm_year - rabyear)
# # 
# # proc_univariate(charls$adult_edu_dplm_year)
# # proc_univariate(charls$age_adult_edu_dplm)
# # 
# # view(charls %>% filter(age_adult_edu_dplm <= 23) %>%
# #        select(contains(c("bd008", "bd009", "bd010", "edu"))))
# 
# # view(charls %>%
# #   select(ID, adult_edu, adult_edu_dplm, adult_edu_dplm_year,
# #          contains(c("bd011", "bd010", "bd009"))) %>%
# #   filter(adult_edu_dplm == 1 & adult_edu_dplm_year > 2011))
##----  Edu level cleaning ----
# # Build the final version of highest education
# with(charls, table(w2_bd001_w2_4, w1_bd001, useNA = "ifany"))

# ###---- Adult education exclusion ----
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


###----  build the variable ----
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
