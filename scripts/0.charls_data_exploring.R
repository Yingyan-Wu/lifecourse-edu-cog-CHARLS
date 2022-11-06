# CHARLS_data_exploring
# Created by: Yingyan Wu
# Some code adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# May.08.2022

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

harmonized_long <- harmonized_selected %>%
  select(ID, contains("agey"), contains("imrc"),
         contains("dlrc")) %>%
  pivot_longer(cols = -ID,
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

harmonized_empirical <- harmonized_long %>%
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

harmonized_empirical %>%
  ggplot(aes(x = wave, y = mean, group = variable, color = variable)) + 
  geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se), width=.1) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x = "Wave",
       y = "Word recall",
       title = "Average word recall score +/- 2 SE at different wave")

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

#---- pending reading core data ----
#---- **main datasets ----
# Adapted from Mingfei Dong's data reading script, 
# Crystal's reading result snippet from make_figures.R in exposure_trajectories
# and the IDRE website
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("demographic_background", "health_status_and_functioning", 
              "work_retirement_and_pension")
folder_index <- c(1:length(folders))[-c(1, 3, 5, 8)] # main waves only

# Read in function
read_charls <- function(paths){
  if (str_detect(paths, "2011")) {
    haven::read_dta(paths) %>%
      select(-"proxy") # pending: can't figure out how to rename proxy automatically
  } else if (str_detect(paths, "2013|2015")){
    haven::read_dta(paths) %>%
      select(-c("xrtype", "versionID")) 
  } else if (str_detect(paths, "2018")){
    haven::read_dta(paths) %>%
      select(-c("xrtype", "versionID",
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
  names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
  # Assign names for each wave's list
  assign(paste0("wave", which(folder_index == i)), list)
}

rm(list)

# ---- **join the datasets ----
# Waves
charls_w1 <- join_all(wave1, by = c("ID", "householdID", "communityID"), 
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w1_", x)})
# colnames(charls_w1)

charls_w2 <- join_all(wave2, by = c("ID", "householdID", "communityID"), 
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w2_", x)})
# colnames(charls_w2)

charls_w3 <- join_all(wave3, by = c("ID", "householdID", "communityID"), 
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w3_", x)})
# colnames(charls_w3)

charls_w4 <- join_all(wave4, by = c("ID", "householdID", "communityID"), 
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w4_", x)})
# colnames(charls_w4)

rm(list = paste0("wave", 1:4))


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

#---- ** join to a single dataset (wave 1 baseline) ----
# Merge all the waves (All the participants!)
charls <- join_all(c(list(charls_w1, charls_w2, charls_w3, charls_w4)),
                   by = c("ID"), # "ID_w234" = "ID", # somehow it didn't work anymore. Rename the ID_w234 in w1 above
                   # "householdID", "communityID"),# Pending: needs to figure out this later
                   type = "full") %>% 
  as_tibble() %>% print(n = 10)

charls %<>% remove_labels()

# charls_w234 <- join_all(c(list(charls_w2, charls_w3, charls_w4)),
#                    by = c("ID", # "ID_w234" = "ID", # somehow it didn't work anymore. Rename the ID_w234 in w1 above
#                           "householdID", "communityID"),
#                    type = "full") %>% 
#   as_tibble() %>% print(n = 10)

#---- Education variables ----
#--- **Highest level of education attained ----
# From harmonized dataset
summarytools::freq(harmonized$raeduc_c)
# From the core dataset
summarytools::freq(charls$w1_bd001)
summarytools::freq(charls$w2_bd001)
# Is the record of highest education level right
summarytools::freq(charls$w2_bd001_w2_2)
# w1 edu level contains adult education
with(charls, table(w1_bd001, w2_bd001_w2_3, useNA = "ifany"))

# w2_bd001 should be asked when new interview == 1. 
# Several participants didn't follow this type
with(charls, table(w1_bd001, w2_bd001, useNA = "ifany"))
with(charls, table(w2_bd001, w2_bd001_w2_4, useNA = "ifany"))
# Wave 1 edu correct or not
with(charls, table(w2_bd001_w2_1, w2_bd001_w2_3, useNA = "ifany"))
with(charls, table(w2_bd001_w2_2, w2_bd001_w2_3, useNA = "ifany"))
# Some of the participants' education going backwards (probably because of the adult education)
with(charls, table(w2_bd001_w2_3, w2_bd001_w2_4, useNA = "ifany"))
with(charls, table(w1_bd001, w2_bd001_w2_3, useNA = "ifany"))

# For those who reported got wave 1 edu right, double check
with(charls %>% filter(w2_bd001_w2_1 == 1),
     table(w1_bd001, w2_bd001_w2_4, useNA = "ifany"))
with(charls %>% filter(w2_bd001_w2_1 == 1),
     table(w2_bd001_w2_3, w2_bd001_w2_4, useNA = "ifany"))
# For those who got wave 1 edu wrong, cross tab of wave 1 and wave 2 edu 
with(charls %>% filter(w2_bd001_w2_1 == 2),
     table(w1_bd001, w2_bd001_w2_3, useNA = "ifany"))

summarytools::freq(charls$w2_bd001)
summarytools::freq(charls$w2_bd001_w2_4)

summarytools::freq(charls$w3_bd001_w2_4)
summarytools::freq(charls$w4_bd001_w2_4)


# Build the final version of highest education
charls %<>%
  mutate(edu_highest = case_when(
    # 12. No change
    !is.na(w4_bd001_w2_4) & w4_bd001_w2_4 != 12 ~ w4_bd001_w2_4,
    w4_bd001_w2_4 == 12 & !is.na(w3_bd001_w2_4) & w3_bd001_w2_4 != 12 ~ 
      w3_bd001_w2_4,
    !is.na(w3_bd001_w2_4) & w3_bd001_w2_4 != 12 ~ w3_bd001_w2_4,
    w3_bd001_w2_4 == 12 & !is.na(w2_bd001_w2_4) & w2_bd001_w2_4 != 12 ~ 
      w2_bd001_w2_4,
    !is.na(w2_bd001_w2_4) & w2_bd001_w2_4 != 12 ~ w2_bd001_w2_4,
    w2_bd001_w2_4 == 12 & !is.na(w1_bd001) ~ w1_bd001,
    !is.na(w2_bd001) & is.na(w1_bd001) ~ w2_bd001,
    !is.na(w1_bd001) ~ w1_bd001)) %>%
  labelled::set_value_labels(
    edu_highest = c("No Formal Education (Illiterate)" = 1,
                    "Did not Finish Primary School" = 2,
                    "Sishu/Home School" = 3,
                    "Elementary School" = 4,
                    "Middle School" = 5,
                    "High School" = 6,
                    "Vocational School" = 7,
                    "Two-/Three-Year College/Associate Degree" = 8,
                    "Four-Year College/Bachelor’s Degree" = 9,
                    "Master’s Degree" = 10,
                    "Doctoral Degree/Ph.D." = 11))

proc_freq(charls, edu_highest)

with(charls %>% filter(edu_highest == 11), table(w4_bd001_w2_4))

with(charls %>% filter(is.na(edu_highest)), 
     table(w2_bd001_w2_3, w2_bd001_w2_4, useNA = "ifany"))
with(charls %>% filter(is.na(edu_highest)), 
     table(w3_bd001_w2_4, useNA = "ifany"))
view(charls %>% filter(is.na(edu_highest) & w3_bd001_w2_4 == 12) %>%
       select(dplyr::contains("bd001")))

#---- ** How many years of schooling after wx_bd001_w2_4----
summarytools::freq(charls$w3_bd002_w3)
summarytools::freq(charls$w4_bd002_w3)
view(with(charls, table(w3_bd002_w3, w4_bd002_w3)) %>% as.matrix())

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
         w4_bd007_w4_1 = coalesce(w4_bd007_w4_1_s1, w4_bd007_w4_1_s2, 
                                  w4_bd007_w4_1_s3, w4_bd007_w4_1_s4, 
                                  w4_bd007_w4_1_s5, w4_bd007_w4_1_s6, 
                                  w4_bd007_w4_1_s7, w4_bd007_w4_1_s8))



with(charls, table(w1_bd007, w2_bd007, useNA = "ifany"))
with(charls, table(w1_bd007, w2_bd007_w2_1, useNA = "ifany"))
# No overlap between these 2 variables
with(charls, table(w2_bd007, w2_bd007_w2_1, useNA = "ifany"))
with(charls, table(w1_bd007s1, w1_bd007s1))
with(charls, table(w3_bd007_w2_1, useNA = "ifany"))

charls %<>%
  mutate(adult_edu = case_when(w4_bd007_w4_1 != 1|w3_bd007_w2_1 != 1|
                                 w2_bd007_w2_1 != 1|w2_bd007 != 1|
                                 w1_bd007 != 1 ~ 1,
                               is.na(w4_bd007_w4_1) & is.na(w3_bd007_w2_1) & 
                                 is.na(w2_bd007_w2_1) & is.na(w2_bd007) & 
                                 is.na(w1_bd007) ~ NA_real_,
                               TRUE ~ 0))

proc_freq(charls, adult_edu)

#---- Education trajectories for some of the participants ----
view(charls %>% filter(adult_edu == 1) %>% 
       select("ID", contains(c("bd001", "bd007"))))
view(charls %>% filter(ID == "010104313002") %>% 
       select("ID", edu_highest, adult_edu, contains(c("bd001", "bd007", 
                                                       "bd005", "bd008"))))
view(charls %>% filter(ID == "010104313002") %>% 
       select("ID", edu_highest, adult_edu, contains(c("bd008", "bd009", "bd010"))))
view(charls %>% filter(ID == "010104313002") %>% select(contains("ba002")))
charls %>% filter(ID == "010104313002") %>% select(contains("bd011"))
charls %>% filter(ID == "010104313002") %>% select(contains("bd010"))
view(charls %>% filter(ID == "010104313002") %>% select(contains("bd007")))

view(charls %>% filter(ID == "034043110002") %>% select(contains("ba002")))
charls %>% filter(ID == "034043110002") %>% select(contains("bd005"))
charls %>% filter(ID == "034043110002") %>% select(contains("bd006"))
charls %>% filter(ID == "034043110002") %>% select(contains("bd002"))
view(charls %>% filter(ID == "034043110002") %>% 
       select("ID", edu_highest, adult_edu, contains(c("bd008", "bd009", "bd010"))))

charls %>% filter(ID == "034043110002") %>% select(contains("bd011"))
charls %>% filter(ID == "034043110002") %>% select(contains("bd010"))
charls %>% filter(ID == "034043110002") %>% select(contains("bd009"))
charls %>% filter(ID == "034043110002") %>% select(contains("bd008")) %>% t()
charls %>% filter(ID == "034043110002") %>% select(contains("bd007")) %>% t()

#---- **Highest grade finished in Primary School ----
summarytools::freq(charls$w1_bd002)

# For those didn't finish primary school but illiterate
with(charls %>% filter(w1_bd001 == 2), summarytools::freq(w1_bd002))
# For those didn't have formal education illiterate
with(charls %>% filter(w1_bd001 == 1), summarytools::freq(w1_bd002))
# For those had sishu
with(charls %>% filter(w1_bd001 == 3), summarytools::freq(w1_bd002))
# For those had middle school education
with(charls %>% filter(w1_bd001 == 5), summarytools::freq(w1_bd002))
# There's one person who claimed to have middle school education but also claimed that I've
# For those who had high school education
with(charls %>% filter(w1_bd001 == 6), summarytools::freq(w1_bd002))

#---- **Additional Years of Schooling after bd001 ----
summarytools::freq(charls$w1_bd003)

#---- **Year of college education started ----
summarytools::freq(charls$w1_bd004)

view(charls %>% filter(w1_bd004 == 1932) %>% select(contains("w1_")))

#---- **Age of formal schooling begins ----
summarytools::freq(charls$w1_bd005)

#---- **Age finish schooling (all the education levels) ----
summarytools::freq(charls$w1_bd006)

# Check for those who had age finish school's highest education
with(charls %>% filter(!is.na(w1_bd006)), table(w1_bd001, useNA = "ifany"))
# All good
# 

# 1. No formal education: illiterate
with(charls %>%
       filter(w1_bd001 == 1), summarytools::freq(w1_bd007s1))
# 4538 people never got any education
# check w2, w3, w4 education status
with(charls %>%
       filter(w1_bd001 == 1), summarytools::freq(w2_bd001_w2_2))

# check hukou
with(charls %>%
       filter(w1_bd001 == 1 & w1_bd007s1 == 1), summarytools::freq(w1_bc001))
# most of them have agricutural hukou
# check occupation status
with(charls %>%
       filter(w1_bd001 == 1 & w1_bd007s1 == 1), summarytools::freq(w1_fb002))
# most of them are farmers

# institution
with(charls %>%
       filter(w1_bd001 == 1 & w1_bd007s1 == 1 & w1_fd002 == 1), 
     summarytools::freq(w1_fd006))
# none of them are civil servant
with(charls %>%
       filter(w1_bd001 == 1 & w1_bd007s1 == 1 & w1_fd002 == 1), 
     summarytools::freq(w1_fd007))
# none of them are formal employee of an establishment 
with(charls %>%
       filter(w1_bd001 == 1 & w1_bd007s1 == 1 & w1_fd002 == 2), 
     summarytools::freq(w1_fd009))
# 3 of them are formal employee of an establishment

with(charls %>%
       filter(w1_bd001 == 1 & w1_bd007s1 == 1 & w1_fd002 == 4), 
     summarytools::freq(w1_fd010))
# unfinished

# 2. Primary school
with(charls %>%
       filter(w1_bd001 == 2), summarytools::freq(w1_bd002))
with(charls %>%
       filter(w1_bd001 == 2), summarytools::freq(w1_bd005))
with(charls %>%
       filter(w1_bd001 == 2), summarytools::freq(w1_bd006))

# adult schools
# Age that finishes schooling
summarytools::freq(charls$w1_bd006)
charls %>% filter(w1_bd006 >= 50) %>%
  select(contains("w1_bd00"))

#---- Waves of Edu ----
#---- ** Wave 1 ----
# n = 17705
# Highest edu level
summarytools::freq(charls_w1$w1_bd001)
# Highest grade finished in primary school
summarytools::freq(charls_w1$w1_bd002)
with(charls_w1, table(w1_bd001, w1_bd002, useNA = "ifany"))
# We could use bd002 for those who filled in bd001 as 1-3 (no formal edu to Sishu)
# For 30 people fill in did not finish primary school, they have missing data for highest grade finished in primary school

# Age of formal schooling begins
proc_univariate(charls_w1$w1_bd005)

# Age finished schooling
proc_univariate(charls_w1$w1_bd006)

with(charls_w1 %>% filter(w1_bd006 > 45), table(w1_bd006, w1_bd005, useNA = "ifany"))
view(charls_w1 %>% filter(w1_bd005 < 20 & w1_bd006 > 45) %>% select(ID, w1_ba002_1,
                                                                    contains("bd00")))
#formal schooling ending: contains adult education

# Adult education
with(charls %>% filter(ID %in% charls_w1$ID), summarytools::freq(w1_bd007))
proc_univariate(charls_w1$w1_bd008)
view(charls_w1 %>% filter(w1_bd008 > 4) %>% select(ID, w1_ba002_1,
                                                   contains("bd00"), 
                                                   contains("bd01"))) 

#---- ** Wave 2 ----
# n = 18605
# Highest edu level
charls_w2 %<>%
  mutate(w2_bd001_clean = case_when(w2_bd001_w2_4 == 12 ~ w2_bd001_w2_3,
                                    !is.na(w2_bd001_w2_4) ~ w2_bd001_w2_4,
                                    !is.na(w2_bd001_w2_3) ~ w2_bd001_w2_3,
                                    !is.na(w2_bd001) ~ w2_bd001))
with(charls_w2, table(w2_bd001, w2_bd001_w2_4, useNA = "ifany"))
with(charls_w2, table(w2_bd001_w2_3, w2_bd001_w2_4, useNA = "ifany"))
with(charls_w2, table(w2_bd001_w2_3, w2_bd001_w2_4, useNA = "ifany"))
with(charls_w2 %>% filter(w2_bd001_w2_4 != 12 & is.na(w2_bd001_w2_4)), 
     table(w2_bd001, w2_bd001_w2_3, useNA = "ifany"))

with(charls_w2, table(w2_bd001, w2_bd001_clean, useNA = "ifany"))

summarytools::freq(charls_w2$w2_bd001_clean)
# nmissing = 9118

# Highest grade finished in primary school
summarytools::freq(charls_w2$w2_bd002)
with(charls_w2, table(w2_bd001_clean, w2_bd002, useNA = "ifany"))
# We could use bd002 for those who filled in bd001 as 1-3 (no formal edu to Sishu)
# For 30 people fill in did not finish primary school, they have missing data for highest grade finished in primary school

# Age of formal schooling begins
proc_univariate(charls_w2$w2_bd005)

# Age finished schooling
proc_univariate(charls_w2$w2_bd006)

with(charls_w2 %>% filter(w2_bd006 > 45), table(w2_bd006, w2_bd005, useNA = "ifany"))
view(charls_w2 %>% filter(w2_bd005 < 20 & w2_bd006 > 45) %>% select(ID, w2_ba002_1,
                                                                    contains("bd00")))
#formal schooling ending: contains adult education

#---- Adult education age distribution ----
summarytools::freq(charls$adult_edu)

#---- ** wave 1 ----
proc_univariate(charls_w1$w1_bd008)
proc_univariate(charls_w1$w1_bd010)

with(charls_w1, table(w1_bd011, useNA = "ifany"))

charls_w1 %<>% mutate(w1_age_finish_adltedu = w1_bd010 - w1_ba002_1)

proc_univariate(charls_w1$w1_age_finish_adltedu)

view(charls_w1 %>% filter(w1_age_finish_adltedu < 20 & 
                            w1_age_finish_adltedu != w1_bd006) %>%
       select(ID, w1_ba002_1, contains("bd00"), w1_age_finish_adltedu,
              contains("bd01")))

#---- **working ----
charls_w1 %<>% mutate(w1_age_work = 
                        case_when(w1_fb001 == 1 ~ w1_fb001_1,
                                  w1_fb001 == 2 ~ w1_fb001_2 - w1_ba002_1))
# The reason of stop working
proc_freq(charls_w1, w1_fb010)
  # 24 of them stopped 
# Age start working * work unit at first job
with(charls_w1, table(w1_age_work, w1_fb002, useNA = "ifany"))

with(charls_w1 %>% filter(w1_age_work < 20), 
     table(w1_age_work, w1_fb004, useNA = "ifany"))

#---- ** wave 2 ----
proc_univariate(charls_w2$w2_bd008)
proc_univariate(charls_w2$w2_bd010)

#---- Education + work trajectories ----
charls %<>%
  mutate(age_edu_start = case_when(!is.na(w2_bd005) ~ w2_bd005,
                                   !is.na(w1_bd005) ~ w1_bd005),
         age_edu_finish = case_when(!is.na(w4_bd006) ~ w4_bd006,
                                   !is.na(w3_bd006) ~ w3_bd006,
                                   !is.na(w2_bd006) ~ w2_bd006,
                                   !is.na(w1_bd006) ~ w1_bd006))

with(charls, table(age_edu_start, w1_bd005, useNA = "ifany"))
summarytools::freq(charls$age_edu_start) # missing 9999 people

with(charls, table(age_edu_start, edu_highest, useNA = "ifany"))
with(charls, table(age_edu_finish, edu_highest, useNA = "ifany"))
with(charls, proc_sum(age_edu_finish - age_edu_start, more = T))
