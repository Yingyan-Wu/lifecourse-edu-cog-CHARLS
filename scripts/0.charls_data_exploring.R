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
       "survey")

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
harmonized_long <- harmonized_selected %>%
  select(ID, contains("agey"), contains("imrc"),
         contains("dlrc")) %>%
  pivot_longer(cols = -ID,
               names_to = c("wave", ".value"),
               names_pattern = "r(\\d+)(.*)") %>%
  print()

random_id <- sample(harmonized_selected$ID, size = 25)

harmonized_long %>%
  filter(ID %in% random_id) %>%
  ggplot(aes(x = agey, y = imrc, group = ID, color = factor(ID))) +
  geom_line() +
  theme(legend.position = "none") +
  theme_classic() +
  theme(x = "Age(years)",
        y = "Immediate word recall")


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
datasets <- c("demographic_background", "health_status_and_functioning")
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


#---- ** join to a single dataset (wave 1 baseline) ----
# Merge all the waves (left join based on those who participated in wave 1)
charls <- join_all(c(list(charls_w1, charls_w2, charls_w3, charls_w4)),
                   by = c("ID", "householdID", "communityID"),
                   type = "left") %>% 
  as_tibble() %>% print(n = 10)

#---- check age that finished schooling ----
charls %>%
  filter(w1_bd006 > w1)