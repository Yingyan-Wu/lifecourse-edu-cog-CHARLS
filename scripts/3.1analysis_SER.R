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

#---- Prepare the dataset ----
covariates <- c("r1age_y", "female", "region_eng", "rarural_child", 
                "lh_hukoutype", "r1lbrf_c", "h1rural", "r1mstat_c_v2", 
                "rameduc_c", "rafeduc_c", "ramoccup_c", "rafoccup_c")
charls_selected %>% select(all_of(covariates)) %>%
  apply(., 2, unique)

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


p_load("gtsummary")
charls_selected %>%
  select(-ID, -province_eng) %>%
  modify_if(is.labelled, to_factor) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ c("{mean} ({sd})",
                                          "({min}, {max})")),
    type = all_continuous() ~ "continuous2",
    # by = childhood_edu20,
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing") %>% 
  # modify_spanning_header(starts_with("stat_") ~ "**Baseline CES-D**") %>%
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  ) %>% as_flex_table()

# charls_selected %<>%
#   filter(lh_hukoutype != 4 & rameduc_c != 3)

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
        "Continuing education with a diploma vs.\nno continuing education\n(conditioning on early life education level)")) %>%
      # Exposure == "continuing_edu210" ~
      #   "Continuing education with/without a diploma vs.\nno continuing education\n(conditioning on early life education level)")) %>%
  mutate(contrast = factor(
    contrast, 
    levels = 
      c("Elementary school or less vs.\nNo Formal Education",
        "Middle school level vs.\nNo Formal Education",
        "College & above vs.\nNo Formal Education",
        "Middle school level vs.\nElementary school or less",
        "College & above vs.\nMiddle school level",
        "College & above vs.\nElementary school or less",
        "Continuing education without a diploma vs.\nno continuing education\n(conditioning on early life education level)",
        # "Continuing education with/without a diploma vs.\nno continuing education\n(conditioning on early life education level)",
        "Continuing education with a diploma vs.\nno continuing education\n(conditioning on early life education level)")))

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
  scale_y_discrete(limits = rev(levels(results_for_figure$contrast))) +
  facet_grid(cols = vars(Outcome), scales = "free") +
  # cols = vars(contrast)) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggtitle("The Estimated Effect of Education Level on Cognition")

ggsave(here::here("output", "figures", "figure_SER.jpg"),
       device = "jpg", dpi = 300, width = 7, height = 5, units = "in")
