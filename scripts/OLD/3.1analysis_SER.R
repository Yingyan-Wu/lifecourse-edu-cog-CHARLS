# 3.1 SER analysis
# Created by Yingyan Wu
# Fixed g comp analysis for SER abstract
# Date: Jan.08.2023

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "tidyverse", "magrittr", "plyr", "haven", "glue", 
       "survey", "labelled")

#---- Load data ----
load(here::here("data", "analysis_data", "charls_ser_abstract.RData"))

charls_selected <- charls %>%
  filter(r1age_y >= 45) %>%
  select(ID, edu_cat, childhood_edu, continuing_edu, r1imrc, r1dlrc,
         r1age_y, female, province_eng, region_eng, rarural_child, lh_hukoutype, 
         r1lbrf_c,
         h1rural,
         r1mstat_c_v2,
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c,
         r1wtrespb) %>%
  na.omit()

#---- function ----
source(here::here("scripts", "functions.R"))
source(here::here("scripts", "3.0time_fixed_g_comp_func.R"))

#---- Table 1 ----
p_load("gtsummary")
charls_selected_svy <- charls_selected %>%
  select(r1age_y, female, edu_cat, h1rural,
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c,
         r1imrc, r1dlrc,
         r1wtrespb) %>%
  labelled::set_variable_labels(
    r1age_y = "Baseline age in years",
    female = "Female (%)",
    edu_cat = "Education level (%)",
    h1rural = "Rural residence (%)",
    rameduc_c = "Mother's education level (%)", 
    rafeduc_c = "Father's education level (%)", 
    ramoccup_c = "Mother's occupation (%)", 
    rafoccup_c = "Father's occupation (%)",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    h1rural = c("Yes" = 1, "No" = 0),
    rameduc_c = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3), 
    rafeduc_c = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3) , 
    ramoccup_c = c("Farming" = 1,
                   "Non-Agricultural" = 0), 
    rafoccup_c = c("Farming" = 1,
                   "Non-Agricultural" = 0)) %>%
  modify_if(is.labelled, to_factor) %>%
  modify_if(is.factor,
            function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  survey::svydesign(~1, data = ., weights = ~r1wtrespb)

table1 <- tbl_svysummary(
    data = charls_selected_svy,
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)",
                     all_continuous() ~ c("{mean} ({sd})")),
    type = all_continuous() ~ "continuous",
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0, 1))) %>% 
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )  

table1 %>% as_flex_table() %>% flextable::save_as_pptx(
    path = here::here("output", "tables", "table1_poster.pptx"))
  # select(-ID, -province_eng) %>%
  # modify_if(is.labelled, to_factor) %>%
  # modify_if(is.factor,
  #           function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  # tbl_summary(
  #   statistic = list(all_categorical() ~ "{n} ({p}%)",
  #                    all_continuous() ~ c("{mean} ({sd})",
  #                                         "({min}, {max})")),
  #   type = all_continuous() ~ "continuous2",
  #   # by = childhood_edu20,
  #   digits = list(all_continuous() ~ 1,
  #                 all_categorical() ~ c(0, 1)),
  #   missing = "ifany",
  #   missing_text = "Missing") %>% 
  # # modify_spanning_header(starts_with("stat_") ~ "**Baseline CES-D**") %>%
  # bold_labels() %>%
  # modify_footnote(
  #   update = list(
  #     starts_with("stat_") ~
  #       "Mean (SD) for continuous variables; n (%) for categorical variables")
  # ) %>% as_flex_table()

tbl_strata_ex1 <-
  charls_selected %>%
  select(r1age_y, female, edu_cat, h1rural,
         rameduc_c, rafeduc_c, ramoccup_c, rafoccup_c,
         r1imrc, r1dlrc,
         r1wtrespb) %>%
  na.omit() %>%
  labelled::set_variable_labels(
    r1age_y = "Baseline age in years",
    female = "Female (%)",
    edu_cat = "Education level (%)",
    h1rural = "Rural residence (%)",
    rameduc_c = "Mother's education level (%)", 
    rafeduc_c = "Father's education level (%)", 
    ramoccup_c = "Mother's occupation (%)", 
    rafoccup_c = "Father's occupation (%)",
    r1imrc = "Baseline Immediate word recall", 
    r1dlrc = "Baseline delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Female" = 1, "Male" = 0),
    h1rural = c("Rural" = 1, "Urban" = 0),
    rameduc_c = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3), 
    rafeduc_c = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3) , 
    ramoccup_c = c("Farming" = 1,
                   "Non-Agricultural" = 0), 
    rafoccup_c = c("Farming" = 1,
                   "Non-Agricultural" = 0)) %>%
  modify_if(is.labelled, to_factor) %>%
  modify_if(is.factor,
            function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  tbl_strata(
    strata = h1rural,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = female, missing = "no") %>%
      add_n(),
    .header = "**{strata}**, N = {n}"
  )

tbl_strata_ex1 %>% as_flex_table()

#---- Prepare the dataset ----
covariates <- c("r1age_y", "female", "region_eng", "rarural_child", 
                "lh_hukoutype", "r1lbrf_c", "h1rural", "r1mstat_c_v2", 
                "rameduc_c", "rafeduc_c", "ramoccup_c", "rafoccup_c")
charls_selected %>% select(all_of(covariates)) %>%
  apply(., 2, unique)

# charls_selected %<>%
#   filter(lh_hukoutype != 4 & rameduc_c != 3)

charls_selected %<>%
  mutate(childhood_edu10 = case_when(childhood_edu == 1 ~ 1,
                                     childhood_edu == 0 ~ 0),
         childhood_edu20 = case_when(childhood_edu == 2 ~ 1,
                                     childhood_edu == 0 ~ 0),
         childhood_edu21 = case_when(childhood_edu == 2 ~ 1,
                                     childhood_edu == 1 ~ 0),
         childhood_edu30 = case_when(childhood_edu == 3 ~ 1,
                                     childhood_edu == 0 ~ 0),
         childhood_edu31 = case_when(childhood_edu == 3 ~ 1,
                                     childhood_edu == 1 ~ 0),
         childhood_edu32 = case_when(childhood_edu == 3 ~ 1,
                                     childhood_edu == 2 ~ 0),
         continuing_edu10 = case_when(continuing_edu == 1 ~ 1,
                                      continuing_edu == 0 ~ 0),
         continuing_edu20 = case_when(continuing_edu == 2 ~ 1,
                                      continuing_edu == 0 ~ 0),
         continuing_edu21 = case_when(continuing_edu == 2 ~ 1,
                                      continuing_edu == 1 ~ 0),
         continuing_edu210 = case_when(continuing_edu %in% c(1, 2) ~ 1,
                                       continuing_edu == 0 ~ 0),
         r1lbrf_c_v2 = case_when(r1lbrf_c %in% c(1, 2) ~ 1, # agriculture related
                                 r1lbrf_c %in% c(3, 4, 5) ~ 2,
                                 r1lbrf_c %in% c(6, 7, 8) ~ 3)) %>%
  mutate_at(c("female", "region_eng", "rarural_child", 
              "lh_hukoutype", "r1lbrf_c", "h1rural", "r1mstat_c_v2", 
              "rameduc_c", "rafeduc_c", "ramoccup_c", "rafoccup_c",
              "r1lbrf_c_v2"),
            as.factor)
# Sanity check
with(charls_selected, table(childhood_edu, childhood_edu30, useNA = "ifany")) 
with(charls_selected, table(continuing_edu, continuing_edu20, useNA = "ifany")) 
charls_selected %>% select(all_of(covariates)) %>%
  map_dfr(class) %>% t()

#---- E(Y_edu1) ----
cov_ch_total <- c("r1age_y", "female", "region_eng", "rarural_child", 
                  "lh_hukoutype", "h1rural",  
                  "rameduc_c", "rafeduc_c", "ramoccup_c", "rafoccup_c")
#---- **Point estimate ----
set.seed("62283")
point_est_Y_x <- tibble()
for (i in c(10, 20, 21, 30, 31, 32)){
  # if (i %in% c(10, 20, 21)){
  #   dataset_forpe <- charls_selected %>% 
  #     filter(lh_hukoutype != 4 & rameduc_c != 3)
  # } else if (i %in% c(30, 31, 32)){
  dataset_forpe <- charls_selected %>% 
    filter(!lh_hukoutype %in% c(3, 4) & rameduc_c != 3)
  # }
  
  for (outcome in c("r1imrc", "r1dlrc")){
    point_est_Y_x %<>%
      rbind(tibble(Point_est = g_comp_x(
        dataset_forpe,
        x = paste0("childhood_edu", i), c = cov_ch_total, 
        y = outcome, wt = "r1wtrespb", 
        bootstrap = F),
        Exposure = paste0("childhood_edu", i),
        Outcome = outcome))
  }
}
point_est_Y_x

#---- **CI ----
bootsize = 1000
tic()
set.seed("62283")
CI_Y_x <- tibble()
for (i in c(10, 20, 21, 30, 31, 32)){
  # if (i %in% c(10, 20, 21)){
  #   dataset_forCI <- charls_selected %>% 
  #     filter(lh_hukoutype != 4 & rameduc_c != 3)
  # } else if (i %in% c(30, 31, 32)){
  dataset_forCI <- charls_selected %>% 
    filter(!lh_hukoutype %in% c(3, 4) & rameduc_c != 3)
  # }
  
  for (outcome in c("r1imrc", "r1dlrc")){
    CI_Y_x %<>%
      rbind(map_dfr(
        .x = 1:bootsize, ~ g_comp_x(
          dataset_forCI,
          x = paste0("childhood_edu", i), c = cov_ch_total, 
          y = outcome, wt = "r1wtrespb", 
          bootstrap = T), .id = "replication") %>% 
          estimate_df() %>% 
          mutate(Exposure = paste0("childhood_edu", i),
                 Outcome = outcome))
  }
}
toc()

#---- **result ----
results_Y_x <- CI_Y_x %>% 
  left_join(point_est_Y_x) %>%
  arrange(Outcome, Exposure) %>%
  select(Exposure, Outcome, Point_est, p2.5th, p97.5th, everything())
view(results_Y_x)

#---- E(Y_edu2|edu1) ----
cov_cont_cond <- c("r1age_y", "female", "region_eng", "rarural_child", 
                   "childhood_edu", "lh_hukoutype", "r1lbrf_c", "h1rural", 
                   "r1mstat_c_v2", "rameduc_c", "rafeduc_c", "ramoccup_c", 
                   "rafoccup_c")
#---- **Point estimate ----
set.seed("62283")
point_est_Y_x2_cond <- tibble()
for (outcome in c("r1imrc", "r1dlrc")){
  point_est_Y_x2_cond %<>%
    rbind(tibble(Exposure = paste0("continuing_edu", c(10, 20, 210)),
                 Outcome = rep(outcome, 3),
                 Point_est = c(
                   # Continuing w/o diploma vs. no|early life edu
                   g_comp_x(charls_selected %>% filter(rameduc_c != 3), 
                            # due to the small cells/positivity issue
                            x = "continuing_edu10", 
                            c = c(cov_cont_cond, "childhood_edu*continuing_edu10"), 
                            y = outcome, wt = "r1wtrespb", 
                            bootstrap = F),
                   # Continuing w/ diploma vs. no|early life edu
                   g_comp_x(charls_selected %>% filter(rameduc_c != 3),
                            # due to the small cells/positivity issue 
                            x = "continuing_edu20", 
                            c = c(cov_cont_cond, "childhood_edu*continuing_edu20"), 
                            y = outcome, wt = "r1wtrespb", 
                            bootstrap = F),
                   g_comp_x(charls_selected %>% filter(rameduc_c != 3),
                            # due to the small cells/positivity issue 
                            x = "continuing_edu210", 
                            c = c(cov_cont_cond, "childhood_edu*continuing_edu20"), 
                            y = outcome, wt = "r1wtrespb", 
                            bootstrap = F))))
}
# Continuing w/ diploma vs. Continuing w/o diploma 
# due to the small cells/positivity issue  -- this won't work 
# g_comp_x(charls_selected %>% filter(rameduc_c != 3 & lh_hukoutype != 4 &
#                                       r1lbrf_c != 6 & r1mstat_c_v2 != 2 &
#                                       # due to the small cells/positivity issue 
#                                       childhood_edu == 0), 
#          x = "continuing_edu21", 
#          c = cov_cont_cond, y = "r1imrc", 
#          bootstrap = F)

#---- **CI ----
bootsize = 1000
set.seed("62283")
tic()
CI_Y_x2_cond <- tibble()
for (outcome in c("r1imrc", "r1dlrc")){
  CI_Y_x2_cond %<>%
    rbind(
      # Continuing w/o diploma vs. no|early life edu
      map_dfr(.x = 1:bootsize, ~ 
                g_comp_x(charls_selected %>% filter(rameduc_c != 3), 
                         # due to the small cells/positivity issue
                         x = "continuing_edu10", 
                         c = c(cov_cont_cond, "childhood_edu*continuing_edu10"), 
                         y = outcome, wt = "r1wtrespb", 
                         bootstrap = T), .id = "replication") %>% 
        estimate_df() %>% mutate(Exposure = "continuing_edu10",
                                 Outcome = outcome), 
      # Continuing w/ diploma vs. no|early life edu
      map_dfr(.x = 1:bootsize, ~ 
                g_comp_x(charls_selected %>% filter(rameduc_c != 3),
                         # due to the small cells/positivity issue 
                         x = "continuing_edu20", 
                         c = c(cov_cont_cond, "childhood_edu*continuing_edu20"), 
                         y = outcome, wt = "r1wtrespb", 
                         bootstrap = T), .id = "replication") %>% 
        estimate_df() %>% mutate(Exposure = "continuing_edu20",
                                 Outcome = outcome),
      # Continuing w/o diploma vs. no|early life edu
      map_dfr(.x = 1:bootsize, ~ 
                g_comp_x(charls_selected %>% filter(rameduc_c != 3),
                         # due to the small cells/positivity issue 
                         x = "continuing_edu210", 
                         c = c(cov_cont_cond, "childhood_edu*continuing_edu20"), 
                         y = outcome, wt = "r1wtrespb", 
                         bootstrap = T), .id = "replication") %>% 
        estimate_df() %>% mutate(Exposure = "continuing_edu210",
                                 Outcome = outcome))
}
toc()

#---- **Results ----
results_Y_x2_cond <- CI_Y_x2_cond %>% 
  left_join(point_est_Y_x2_cond) %>%
  arrange(Outcome, Exposure) %>%
  select(Exposure, Outcome, Point_est, p2.5th, p97.5th, everything())

save(results_Y_x, results_Y_x2_cond, 
     file = here::here("data", "analysis_data", "SER_abstract_results.RData"))
# #---- Coditional effect: Pending after SER ----
# # Error: no valid set of coefficients has been found: please supply starting values
# cov_cont_cond <- c("r1age_y", "female", "region_eng", "rarural_child",
#                    "lh_hukoutype", "r1lbrf_c", "h1rural",
#                    "r1mstat_c_v2", "rameduc_c", "rafeduc_c", "ramoccup_c",
#                    "rafoccup_c")
# 
# point_est_Y_x2_condx1 <- tibble()
# set.seed("62283")
# # Point estimate
# for (i in 0:3){
#   point_est_Y_x2_condx1 %<>%
#     rbind(
#       tibble(
#         condx1 = rep(paste0("childhood_edu", i), 2),
#         Exposure = paste0("childhood_edu", c(10, 20)),
#         Point_est = c(
#           # Continuing w/o diploma vs. no|early life edu
#           g_comp_x(charls_selected %>%
#                      filter(rameduc_c != 3 & # due to the small cells/positivity issue
#                               childhood_edu == i),
#                    x = "continuing_edu10",
#                    c = cov_cont_cond,
#                    y = "r1imrc",
#                    bootstrap = F),
#           # Continuing w/ diploma vs. no|early life edu
#           g_comp_x(charls_selected %>%
#                      filter(rameduc_c != 3 & # due to the small cells/positivity issue
#                               childhood_edu == i),
#                    x = "continuing_edu20",
#                    c = cov_cont_cond, y = "r1imrc",
#                    bootstrap = F))))
#   # Continuing w/ diploma vs. Continuing w/o diploma
#   # due to the small cells/positivity issue  -- this won't work
#   # g_comp_x(charls_selected %>% filter(rameduc_c != 3 & lh_hukoutype != 4 &
#   #                                       r1lbrf_c != 6 & r1mstat_c_v2 != 2 &
#   #                                       # due to the small cells/positivity issue
#   #                                       childhood_edu == 0),
#   #          x = "continuing_edu21",
#   #          c = cov_cont_cond, y = "r1imrc",
#   #          bootstrap = F)
# }
# 
# 
# # CI
# bootsize = 1000
# 
# CI_Y_x2_condx1 <- tibble()
# for (i in 0:1){
#   CI_Y_x2_condx1 %<>%
#     rbind(
#       # Continuing w/o diploma vs. no|early life edu
#       map_dfr(.x = 1:bootsize, ~
#                 g_comp_x(charls_selected %>% filter(rameduc_c != 3 &
#                                                       childhood_edu == i),
#                          x = "continuing_edu10",
#                          c = cov_cont_cond,
#                          y = "r1imrc",
#                          bootstrap = T), .id = "replication") %>%
#         estimate_df() %>% mutate(Exposure = "continuing_edu10",
#                                  condx1 = paste0("childhood_edu", i)),
# 
#       # Continuing w/ diploma vs. no|early life edu
#       map_dfr(.x = 1:bootsize, ~
#                 g_comp_x(charls_selected %>% filter(rameduc_c != 3 &
#                                                       childhood_edu == i &
#                                                       lh_hukoutype != 3 &
#                                                       rameduc_c != 2),
#                          x = "continuing_edu20",
#                          c = cov_cont_cond,
#                          y = "r1imrc",
#                          bootstrap = T), .id = "replication") %>%
#         estimate_df() %>% mutate(Exposure = "continuing_edu20",
#                                  condx1 = paste0("childhood_edu", i)))
# }
# 
# results_Y_x2_condx1 <- CI_Y_x2_condx1 %>%
#   left_join(point_est_Y_x) %>%
#   select(Exposure, condx1, Point_est, everything())

#---- Figure ----
load(here::here("data", "analysis_data", "SER_abstract_results.RData"))
results_for_figure <- rbind(results_Y_x, results_Y_x2_cond) %>%
  filter(Exposure != "continuing_edu210") %>%
  mutate(
    Outcome = case_when(Outcome == "r1imrc" ~ "Immediate word recall score",
                        Outcome == "r1dlrc" ~ "Delayed word recall score",
                        TRUE ~ Outcome),
    contrast = case_when(
      Exposure == "childhood_edu10" ~ 
        "Elementary school or less vs.\nNo Formal Education",
      Exposure == "childhood_edu20" ~ 
        "Middle school level vs.\nNo Formal Education",
      Exposure == "childhood_edu21" ~ 
        "Middle school level vs.\nElementary school or less",
      Exposure == "childhood_edu30" ~ 
        "College & above vs.\nNo Formal Education",
      Exposure == "childhood_edu31" ~ 
        "College & above vs.\nElementary school or less",
      Exposure == "childhood_edu32" ~ 
        "College & above vs.\nMiddle school level",
      Exposure == "continuing_edu10" ~
        "Continuing education without a diploma vs.\nno continuing education\n(conditioning on early life education level)",
      Exposure == "continuing_edu20" ~
        "Continuing education with a diploma vs.\nno continuing education\n(conditioning on early life education level)"),
    comparison = case_when(str_detect(Exposure, "childhood_edu") ~ "Early life edu",
                           str_detect(Exposure, "continuing_edu") ~ "Continuing edu")) %>%
      # Exposure == "continuing_edu210" ~
      #   "Continuing education with/without a diploma vs.\nno continuing education\n(conditioning on early life education level)")) %>%
  mutate(contrast = factor(
    contrast, 
    levels = 
      rev(c("Elementary school or less vs.\nNo Formal Education",
        "Middle school level vs.\nNo Formal Education",
        "College & above vs.\nNo Formal Education",
        "Middle school level vs.\nElementary school or less",
        "College & above vs.\nMiddle school level",
        "College & above vs.\nElementary school or less",
        "Continuing education without a diploma vs.\nno continuing education\n(conditioning on early life education level)",
        # "Continuing education with/without a diploma vs.\nno continuing education\n(conditioning on early life education level)",
        "Continuing education with a diploma vs.\nno continuing education\n(conditioning on early life education level)"))))

results_for_figure %>%
  ggplot(aes(x = Mean,
             # x = Point_est, 
             y = contrast)) +
  geom_point(position = position_dodge((width = 0.3))) +
  geom_errorbarh(aes(xmin = p2.5th, xmax = p97.5th), height = 0.1,
                 position = position_dodge()) +
  geom_vline(xintercept = 0) +
  ylab("Comparison") +
  xlab("Mean Difference") +
  scale_x_continuous(limits = c(-0.7, 3.3)) +
  # scale_x_continuous(limits=c(-3, 2), breaks = seq(0, 2.5, 0.5)) +
  # scale_y_discrete(limits = rev(levels(results_for_figure$contrast))) +
  facet_grid(cols = vars(Outcome), scales = "free") +
  # cols = vars(contrast)) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggtitle("The Estimated Effect of Education Level on Cognition")

ggsave(here::here("output", "figures", "figure_SER.jpg"),
       device = "jpg", dpi = 300, width = 7, height = 5, units = "in")

#---- ** another version of the figure ----
results_for_figure <- rbind(results_Y_x, results_Y_x2_cond) %>%
  filter(Exposure != "continuing_edu210") %>%
  mutate(
    Outcome = case_when(Outcome == "r1imrc" ~ "Immediate word recall score",
                        Outcome == "r1dlrc" ~ "Delayed word recall score",
                        TRUE ~ Outcome),
    contrast = case_when(
      Exposure == "childhood_edu10" ~ 
        "Elementary school or less (41.0%) vs.\nNo Formal Education (25.1%)",
      Exposure == "childhood_edu20" ~ 
        "Middle school level (32.3%) vs.\nNo Formal Education",
      Exposure == "childhood_edu21" ~ 
        "Middle school level vs.\nElementary school or less",
      Exposure == "childhood_edu30" ~ 
        "College & above (1.6%) vs.\nNo Formal Education",
      Exposure == "childhood_edu31" ~ 
        "College & above vs.\nElementary school or less",
      Exposure == "childhood_edu32" ~ 
        "College & above vs.\nMiddle school level",
      Exposure == "continuing_edu10" ~
        "Continuing education w/o a diploma (5.4%) vs.\nno continuing education (93.0%)",
      Exposure == "continuing_edu20" ~
        "Continuing education w/ a diploma (2.6%) vs.\nno continuing education"),
    comparison = case_when(str_detect(Exposure, "childhood_edu") ~ "Early life edu",
                           str_detect(Exposure, "continuing_edu") ~ "Continuing edu")) %>%
  # Exposure == "continuing_edu210" ~
  #   "Continuing education with/without a diploma vs.\nno continuing education\n(conditioning on early life education level)")) %>%
  mutate(contrast = factor(
    contrast, 
    levels = 
      rev(c("Elementary school or less (41.0%) vs.\nNo Formal Education (25.1%)",
            "Middle school level (32.3%) vs.\nNo Formal Education",
            "College & above (1.6%) vs.\nNo Formal Education",
            "Middle school level vs.\nElementary school or less",
            "College & above vs.\nMiddle school level",
            "College & above vs.\nElementary school or less",
            "Continuing education w/o a diploma (5.4%) vs.\nno continuing education (93.0%)",
            # "Continuing education with/w/o a diploma vs.\nno continuing education\n(conditioning on early life education level)",
            "Continuing education w/ a diploma (2.6%) vs.\nno continuing education"))))

results_for_figure %>%
  ggplot(aes(x = Mean,
             y = contrast)) +
  geom_point(position = position_dodge((width = 0.3)), 
             color = "#0083C3", size = 2.5) +
  geom_errorbarh(aes(xmin = p2.5th, xmax = p97.5th), color = "#0083C3",
                 height = 0.1, position = position_dodge(), size = 0.8) +
  geom_vline(xintercept = 0) +
  xlab("Mean Difference") +
  scale_x_continuous(limits = c(-0.7, 3.3), breaks = seq(-0.7, 3.3, 0.5)) +
  # scale_x_continuous(limits=c(-3, 2), breaks = seq(0, 2.5, 0.5)) +
  facet_grid(fct_rev(comparison) ~ Outcome, scales = "free", space = "free") +
  # scale_y_discrete(limits = rev(levels(results_for_figure$contrast))) +
  # cols = vars(contrast)) +
  theme_bw() +
  theme(axis.text.y = element_text(color = "black", size = 11),
        axis.title.y = element_blank(),
        strip.text.y = element_text(size = 10.5),
        plot.background = element_rect(fill='transparent', color=NA))
  # ggtitle("The Estimated Effect of Education Level on Cognition")

ggsave(here::here("output", "figures", "figure_poster_06042023.png"),
       bg='transparent',
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

results_for_figure %>%
  filter(Outcome == "Delayed word recall score") %>%
  ggplot(aes(x = Mean,
             y = contrast)) +
  geom_point(position = position_dodge((width = 0.3)), 
             color = "#0083C3", size = 2.5) +
  geom_errorbarh(aes(xmin = p2.5th, xmax = p97.5th), color = "#0083C3",
                 height = 0.1, position = position_dodge(), size = 0.8) +
  geom_vline(xintercept = 0) +
  xlab("Mean Difference") +
  scale_x_continuous(limits = c(-0.7, 3.3), breaks = seq(-0.7, 3.3, 0.5)) +
  # scale_x_continuous(limits=c(-3, 2), breaks = seq(0, 2.5, 0.5)) +
  facet_grid(fct_rev(comparison) ~ ., scales = "free", space = "free") +
  # scale_y_discrete(limits = rev(levels(results_for_figure$contrast))) +
  # cols = vars(contrast)) +
  theme_bw(base_size = 16) +
  theme(axis.text.y = element_text(color = "black", size = 12),
        axis.title.y = element_blank(),
        strip.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        plot.background = element_rect(fill='transparent', color=NA))
# ggtitle("The Estimated Effect of Education Level on Cognition")

ggsave(here::here("output", "figures", "figure_poster_dlrc_06052023_larger_font.png"),
       bg='transparent',
       device = "png", dpi = 300, width = 7, height = 5, units = "in")
