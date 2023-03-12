# CHARLS_data_construction
# Created by: Yingyan Wu
# Some code adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# Dec.25.2022

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
                          paste0("inw", 1:4),
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

#---- **harmonized life history ----
harmonized_lh <- haven::read_dta(here::here("data", "raw_data", "harmonized",
                                            "H_CHARLS_LH_a.dta"))

#---- **psu ----
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
#---- **main datasets ----
# Adapted from Mingfei Dong's data reading script, 
# Crystal's reading result snippet from make_figures.R in exposure_trajectories
# and the IDRE website
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("demographic_background", "health_status_and_functioning", 
              "work_retirement_and_pension", "work_retirement",
              "family_information", "weights", "Sample_infor"
              )
folder_index <- c(1:length(folders))[-c(1, 3, 5, 8)] # main waves only

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
  } else if (str_detect(paths, "Family_Information|Weights") & 
             str_detect(paths, "2013|2015")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2015|2018") &
             str_detect(paths, "Sample_Infor")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2013|2015")){
    haven::read_dta(paths) %>%
      select(-c("xrtype", "versionID"))
  } else if (str_detect(paths, "Family_Information") & 
             str_detect(paths, "2018")){
    haven::read_dta(paths) %>%
      select(-c("versionID"))
  } else if (str_detect(paths, "2018") &
             str_detect(paths, "Weights")){
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
  # list <- lapply(files, read_dta)
  list <- lapply(files, read_charls)
  names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
  # Assign names for each wave's list
  assign(paste0("wave", which(folder_index == i)), list)
}

rm(list)

gc()

#---- **life history datasets ----
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

charls_lh <- join_all(life_history, 
                      by = c("ID", "householdID", "communityID", "versionID"),
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

# colnames(charls %>% select(contains(".x")))
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

rm(list = paste0("charls_", c(paste0("w", 1:4), "lh")))

charls %<>% remove_labels()
gc()
# proc_freq(harmonized, raeduc_c)

diffdf::diffdf(harmonized_lh %>% filter(ID %in% harmonized$ID) %>% 
                 select(ID, "rabmonth", "rabyear", "ragender", 
                        "ramomoccup_c", "radadoccup_c"),
               harmonized %>% filter(ID %in% harmonized_lh$ID) %>% 
                 select(ID, "rabmonth", "rabyear", "ragender", 
                        "ramomoccup_c", "radadoccup_c"))

#---- Age ----
charls %>% select(contains("agey")) %>%
  apply(., 2, proc_sum)

age_y <- charls %>% dplyr::select(contains("agey"), -contains("LHagey")) %>% 
  apply(., 1, impute_ages) %>% t()

charls[, paste0("r", 1:4, "age_y")] <- age_y
rm(age_y)

charls %>% select(contains("age_y")) %>%
  apply(., 2, proc_sum)

# Check those missing the ages
view(charls %>% filter(is.na(r1age_y)) %>%
       select(paste0("w", 1:4, "_ba002_1"),
              paste0("w", 1:4, "_ba002_2"),
              paste0("w", 1:4, "_ba003"),
              paste0("w", 1:4, "_iyear"),
              paste0("w", 1:4, "_imonth"),
              "rabyear", "rabmonth", "rabday",
              paste0("r", 1:4, "agey"),
              "radyear", "radmonth", contains("lh_r")))

colSums(is.na(harmonized %>% filter(is.na(r1agey) & is.na(r2agey) & is.na(r3agey) &
                             is.na(r4agey)) %>%
       select(rabyear, rabmonth, rabday))) # 305 missing

missingage_id <- harmonized %>% 
  filter(is.na(r1agey) & is.na(r2agey) & is.na(r3agey) & is.na(r4agey)) %>% 
  pull(ID)

harmonized_lh %>% filter(ID %in% missingage_id) %>% nrow()

colSums(is.na(harmonized_lh %>% filter(ID %in% missingage_id) %>%
                select(rabyear, rabmonth)))
charls %>% filter(ID %in% missingage_id) %>% nrow()
colSums(is.na(harmonized_lh %>% filter(ID %in% missingage_id) %>%
                select(rabyear, rabmonth)))

charls_missingage <- charls %>% filter(ID %in% missingage_id) %>%
       select(paste0("w", 1:4, "_ba002_1"),
              paste0("w", 1:4, "_ba002_2"),
              paste0("w", 1:4, "_ba003"),
              paste0("w", 1:4, "_iyear"),
              paste0("w", 1:4, "_imonth"),
              "rabyear", "rabmonth", "rabday",
              paste0("r", 1:4, "agey"),
              "radyear", "radmonth", contains("lh_r"))
table(charls_missingage$w1_ba002_2, useNA = "ifany") # 72 of them can't remember their birth month

view(charls_missingage %>% filter(w1_ba002_2 == 0) %>%
  select(paste0("w", 1:4, "_ba002_1"),
         paste0("w", 1:4, "_ba002_2"),
         paste0("w", 1:4, "_ba003"),
         paste0("w", 1:4, "_iyear"),
         paste0("w", 1:4, "_imonth"),
         "rabyear", "rabmonth", "rabday",
         paste0("r", 1:4, "agey"),
         "radyear", "radmonth", contains("lh_r")))

#---- Gender ----
charls %<>% 
  mutate(female = case_when(ragender == 1 ~ 0,
                            ragender == 2 ~ 1))
# sanity check
with(charls, table(female, ragender, useNA = "ifany"))
with(charls, table(female, w1_rgender, useNA = "ifany"))
with(charls, table(female, w2_ba000_w2_3, useNA = "ifany"))
with(charls, table(female, w2_xrgender, useNA = "ifany"))
with(charls, table(female, w3_ba000_w2_3, useNA = "ifany"))
with(charls, table(female, w3_xrgender, useNA = "ifany"))
with(charls, table(female, w4_ba000_w2_3, useNA = "ifany"))
with(charls, table(female, w4_zfrgender, useNA = "ifany"))
with(charls, table(female, lh_rgender, useNA = "ifany"))
# This 298 participants didn't get any variables related to birthdate as well!
colSums(is.na(charls %>% filter(is.na(r1age_y) & is.na(female)) %>%
                select(paste0("w", 1:4, "_ba002_1"),
                       paste0("w", 1:4, "_ba003"),
                       "rabyear", "rabmonth", "rabday",
                       paste0("r", 1:4, "agey"),
                       "radyear", "radmonth")))
id_298 <- charls %>% filter(is.na(r1age_y) & is.na(female)) %>%
  select(ID) %>% pull()

view(harmonized_lh %>% filter(ID %in% id_298))

#Drop CHARLS harmonized gender variable
# charls %<>% dplyr::select(-ragender)

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

#---- **Edu + adult edu ----
charls %<>%
  mutate(
    childhood_edu = case_when(raeduc_c == 1 ~ 0,
                              raeduc_c %in% c(2, 3, 4) ~ 1,
                              raeduc_c %in% c(5, 6, 7) ~ 2,
                              raeduc_c %in% c(8, 9, 10) ~ 3),
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

proc_freq(charls, edu_cat)
with(charls, table(edu_cat, raeduc_c, useNA = "ifany"))
with(charls %>% filter(is.na(edu_cat) & raeduc_c == 1), 
     table(adult_edu, adult_edu_dplm, useNA = "ifany"))

#---- **Adult edu time ----
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

#---- **Education interruption 1967 - 1977 ----
proc_freq(charls, lh_e002)
with(charls, table(rabyear, lh_e002, useNA = "ifany"))
with(charls, table(lh_e003a, lh_e002, useNA = "ifany"))
with(charls, table(lh_e004, lh_e002, useNA = "ifany"))
length(charls %>% filter(rabyear <= 1962 & rabyear >= 1946))
# charls %>% filter(rabyear <= 1962 & rabyear >= 1946)

# Pending

#---- Region of residence Childhood ----
proc_freq(charls, lh_t002_psu_1_) %>% print(n = Inf)
# These code corresponds to the 2011 NBS area code
charls %>% select(contains("lh_t002_psu_")) %>%
  apply(., 2, function(x) table(x, useNA = "ifany"))
# Pending

#---- Rural/urban residence Childhood ----
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
proc_freq(charls, rarural_child)

#---- Parental socioeconomic status (latent variable to be built) ----
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

# #---- birth order ----
# # Pending 
# # Come back after SER abstract
# proc_freq(charls, w3_cc003_w3_1_)
# proc_freq(charls, w4_cc000_w4_1)
# with(charls, table(lh_b2_1_1_, lh_b2_1_5_, useNA = "ifany"))
# 
# charls %>% select(contains("lh_b4_2_")) %>%
#   apply(., 2, table, useNA = "ifany")
# 
# charls %<>%
#   mutate(
#     first_child = case_when(
#       w1_cc002_1 > 0 |w1_cc002_3 > 0|w1_cc004_1 > 0|w1_cc004_3 > 0|
#         w2_cc002_1_1_ > 0|w2_cc002_3_1_ > 0|w2_cc004_1_1_ > 0|
#         w2_cc004_3_1_ > 0 ~ 0,
#       ((w1_cc002_1 == 0 & w1_cc002_3 == 0 & w1_cc004_1 == 0 & w1_cc004_3 == 0)|
#         (w2_cc002_1_1_ == 0 & w2_cc002_3_1_ == 0 & w2_cc004_1_1_ == 0 & 
#            w2_cc004_3_1_ == 0)) ~ 1,
#       # lh_b2_1_1_ %in% c(2, 4) ~ 1,
#       w3_xsiblingnum == 0 ~ 1,
#       w4_cc000_w4_1 == 0 ~ 1,
#       is.na(w1_cc002_1) & is.na(w1_cc002_3) & is.na(w1_cc004_1) & 
#         is.na(w1_cc004_3) & is.na(w2_cc002_1_1_) & is.na(w2_cc002_3_1_) & 
#         is.na(w2_cc004_1_1_) & is.na(w2_cc004_3_1_) ~ NA_real_
#       # TRUE ~ 1
#     ))
# 
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
# 
# sib_mat_liv6 <- charls %>% select(ID, contains("lh_b4_3_")) %>%
#   mutate(sib1_sib2 = lh_b4_3_1_ - lh_b4_3_2_)
# 
# view(charls %>% filter(ID == "056346102001") %>%
#        select(ID, female, rabyear, edu_cat, contains("lh_b4_3_")))

#---- working history from harmonized LH ----
work_history_long_mat <- charls %>%
  select(ID, rLHagey, contains("c_wrkstate")) %>%
  pivot_longer(!c(ID, rLHagey), 
               names_to = "Age",
               values_to = "wrkstate",
               names_pattern = "c_wrkstate(.*)") %>%
  filter(Age <= rLHagey) # exclude the records of later age which participants
# have not reached (assigned NA records)

# Sanity check
with(work_history_long_mat, table(Age, useNA = "ifany"))

# A random sample of 5 people
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

#---- First occupation type ----
# # Pending after SER
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
proc_freq(charls, r1lbrf_c)

#---- Marital history from harmonized LH ----
marital_long_mat <- charls %>%
  select(ID, rLHagey, contains("prtnstate")) %>%
  pivot_longer(!c(ID, rLHagey), 
               names_to = "Age",
               values_to = "prtnstate",
               names_pattern = "prtnstate(.*)") %>%
  filter(Age <= rLHagey) # exclude the records of later age which participants
# have not reached (assigned NA records)

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

#---- Marital status ----
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
#---- **Rural v.s.urban ----
proc_freq(charls, h1rural)
with(charls, table(h1rural, h2rural, useNA = "ifany"))
with(charls, table(h1rural, h3rural, useNA = "ifany"))

# Sanity check
with(charls %>% filter(is.na(h1rural) & !is.na(h2rural)), 
     table(inw1, inw2, useNA = "ifany"))
with(charls %>% filter(is.na(h1rural) & !is.na(h3rural)), 
     table(inw1, inw3, useNA = "ifany"))
with(charls %>% filter(is.na(h1rural) & !is.na(h4rural)), 
     table(inw1, inw4, useNA = "ifany"))

#---- **Province、region ----
proc_freq(charls, province_eng) %>% print(n = Inf)
charls %<>% mutate(
  region_eng = case_when(
    str_detect(province_eng, "Beijing|Tianjin|Hebei|Shanghai|Jiangsu|Zhejiang|
               Fujian|Shandong|Guangdong|Hainan") ~ 1,
    str_detect(province_eng, "Shanxi|Anhui|Jiangxi|Henan|Hubei|Hunan") ~ 2,
    str_detect(province_eng, "Mongolia|Guangxi|Chongqing|Sichuan|Guizhou|Yunnan|
               Tibet|Shaanxi|Gansu|Qinghai|Ningxia|Xinjiang") ~ 3,
    str_detect(province_eng,"Liaoning|Jilin|Heilongjiang") ~ 4))

# Sanity check
proc_freq(charls, region_eng)
with(charls, table(region_eng, province_eng, useNA = "ifany"))

#---- Pending on birth place ----
# Pending after SER

#---- Hukou history from harmonized LH ----
hukou_long_mat <- charls %>%
  select(ID, rLHagey, contains("hkstate")) %>%
  pivot_longer(!c(ID, rLHagey), 
               names_to = "Age",
               values_to = "hkstate",
               names_pattern = "hkstate(.*)") %>%
  filter(Age <= rLHagey) # exclude the records of later age which participants
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

#---- Hukou status ----
proc_freq(charls, r1hukou)
proc_freq(charls, lh_hukoutype) # First hukou status

with(charls, table(r1hukou, r2hukou, useNA = "ifany"))
with(charls %>% filter(is.na(r1hukou) & !is.na(r2hukou)), 
     table(inw1, inw2, useNA = "ifany"))
with(charls %>% filter(is.na(r1hukou) & !is.na(r3hukou)), 
     table(inw1, inw3, useNA = "ifany"))
with(charls %>% filter(is.na(r1hukou) & !is.na(r4hukou)), 
     table(inw1, inw4, useNA = "ifany"))

charls %<>%
  mutate(hukou_flag = case_when(
    r1hukou == r2hukou & r1hukou == r3hukou &
      r1hukou == r4hukou ~ 0,
    is.na(r1hukou) & is.na(r2hukou) & is.na(r3hukou) &
      is.na(r4hukou) & is.na(lh_hukoutype) ~ NA_real_,
    TRUE ~ 1))

# Pending after SER

proc_freq(charls, hukou_flag)
hukou_mat <- charls %>% select(contains("hukou")) %>% select(-hukou_flag)
colSums(is.na(hukou_mat))

#---- Save the data ----
save(charls, 
     file = here::here("data", "analysis_data", "charls_ser_abstract.RData"))

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
