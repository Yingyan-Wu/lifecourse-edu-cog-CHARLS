# Bootstrap results (from cluster)
# Created by: Yingyan Wu
# Oct.29.2024

#---- Package loading + options ----
rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman", repos = 'http://cran.us.r-project.org')
}
p_load("here", "tidyverse", "broom", "magrittr")

# No scientific notation
options(scipen = 999)

#---- Load the results ----
# # For some reason, the hoffman cluster output did not include the column names
# temp <- readRDS(here::here("data", "hoffman2", "model_results",
#                            paste0("MSM_coef_imrc_0.RDS")))
# 
# imrc_MSM_coef <- tibble()
# for (scenario_num in 1:40){
#   imrc_MSM_coef <- bind_rows(
#     imrc_MSM_coef, read_csv(
#       here::here("data", "hoffman2", "appended_model_results",
#                  paste0("MSM_coef_imrc_scenario_", scenario_num, ".csv")),
#     col_names = FALSE))
# }
# imrc_MSM_coef %<>% set_colnames(colnames(temp))
# 
# dlrc_MSM_coef <- tibble()
# for (scenario_num in 41:80){
#   dlrc_MSM_coef <- bind_rows(
#     dlrc_MSM_coef, read_csv(
#       here::here("data", "hoffman2", "appended_model_results",
#                  paste0("MSM_coef_dlrc_scenario_", scenario_num, ".csv")),
#       col_names = F))
# }
# dlrc_MSM_coef %<>% set_colnames(colnames(temp))
# 
# # Check if there's missing run
# table(imrc_MSM_coef$n_imp)
# table(imrc_MSM_coef$i_boot)
# table(dlrc_MSM_coef$n_imp)
# table(dlrc_MSM_coef$i_boot)
# 
# 
# save(imrc_MSM_coef, dlrc_MSM_coef, 
#      file = here::here("data", "model_results", "MSM_coef_imrc_dlrc.RData"))

load(here::here("data", "model_results", "MSM_coef_imrc_dlrc.RData"))

# Check warnings: no warnings at all for all models
# # For some reason, the hoffman cluster output did not include the column names
# colnames_warning <- c("i_boot", "imp", "scenario", "duration", "w_qmodel",
#                       "w_msm", "w_qmodel_msg", "w_msm_msg", "w_chldstate_30",
#                       "w_marital_30", "w_rural_30", "w_occupation_30_2cat",
#                       "w_hukou_30_2cat")
# 
# imrc_warnings <- tibble()
# for (scenario_num in 1:40){
#   imrc_warnings <- bind_rows(
#     imrc_warnings, read_csv(
#       here::here("data", "hoffman2", "appended_warnings",
#                  paste0("warnings_imrc_scenario_", scenario_num, ".csv")),
#     col_names = FALSE))
# }
# imrc_warnings %<>% set_colnames(colnames_warning)
# 
# dlrc_warnings <- tibble()
# for (scenario_num in 41:80){
#   dlrc_warnings <- bind_rows(
#     dlrc_warnings, read_csv(
#       here::here("data", "hoffman2", "appended_warnings",
#                  paste0("warnings_dlrc_scenario_", scenario_num, ".csv")),
#       col_names = FALSE))
# }
# dlrc_warnings %<>% set_colnames(colnames_warning)
# 
# # Check warnings
# imrc_warnings %>%
#   select(starts_with("w_"), -ends_with("msg")) %>%
#   apply(., 2, table, useNA = "ifany")
# 
# dlrc_warnings %>%
#   select(starts_with("w_"), -ends_with("msg")) %>%
#   apply(., 2, table, useNA = "ifany")

#---- PE, 95% CI ----
for (y in c("imrc", "dlrc")){
  temp <- get(paste0(y, "_MSM_coef")) %>%
    mutate(
      # Both education vs. No formal education at all
      slope_11_00 = doX0_1 + doX1_1 + `doX0_1:doX1_1` + `fu_time:doX0_1` + `fu_time:doX1_1` + `fu_time:doX0_1:doX1_1`,
      slope_21_00 = doX0_2 + doX1_1 + `doX0_2:doX1_1` + `fu_time:doX0_2` + `fu_time:doX1_1` + `fu_time:doX0_2:doX1_1`,
      slope_31_00 = doX0_3 + doX1_1 + `doX0_3:doX1_1` + `fu_time:doX0_3` + `fu_time:doX1_1` + `fu_time:doX0_3:doX1_1`,
      # Only continuing education vs. No formal education at all
      slope_01_00 = doX1_1 + `fu_time:doX1_1`,
      # Both education vs. No continuing education with the same early life edu intervention
      slope_11_10 = doX1_1 + `doX0_1:doX1_1` + `fu_time:doX1_1` + `fu_time:doX0_1:doX1_1`,
      slope_21_20 = doX1_1 + `doX0_2:doX1_1` + `fu_time:doX1_1` + `fu_time:doX0_2:doX1_1`,
      slope_31_30 = doX1_1 + `doX0_3:doX1_1` + `fu_time:doX1_1` + `fu_time:doX0_3:doX1_1`,
      # Only early life education (no continuing education) vs. No formal education at all
      slope_10_00 = doX0_1 + `fu_time:doX0_1`,
      slope_20_00 = doX0_2 + `fu_time:doX0_2`,
      slope_30_00 = doX0_3 + `fu_time:doX0_3`)
  assign(paste0(y, "_MSM_coef"), temp)
}

imrc_dlrc_CI <- tibble()
for (y in c("imrc", "dlrc")){
  # 2.5th and 97.5th
  temp <- get(paste0(y, "_MSM_coef")) %>%
    filter(i_boot != 0) %>%
    select(-i_boot, -scenario_num) %>%
    reframe(across(everything(), ~quantile(.x, c(0.025, 0.975)))) %>%
    mutate(estimate = c("p2.5th", "p97.5th"),
           outcome = y) %>%
    select(estimate, outcome, everything())
  # Mean PE
  temp2 <- get(paste0(y, "_MSM_coef")) %>%
    filter(i_boot == 0) %>%
    select(-i_boot, -scenario_num) %>%
    mutate(estimate = "pe",
           outcome = y,
           across(-c("estimate", "outcome"), \(x) mean(x, na.rm = TRUE))) %>%
    select(estimate, outcome, everything()) %>%
    unique()
  imrc_dlrc_CI <- bind_rows(imrc_dlrc_CI, temp2, temp)
}

remove(temp, temp2)

#---- Estimates ----
imrc_dlrc_CI %>%
  select(estimate, outcome, contains("slope"))

#---- Whisker plot ----
CI_forplot <- imrc_dlrc_CI %>%
  select(estimate, outcome, contains("slope")) %>%
  pivot_longer(cols = -c(estimate, outcome),
               names_to = "term") %>%
  pivot_wider(names_from = estimate, values_from = value) %>%
  mutate(
    label = case_when(
      str_detect(term, "11_00") ~ "Elementary school or less & Adulthood edu vs.\nNo formal education at all",
      str_detect(term, "21_00") ~ "Middle school level & Adulthood edu vs.\nNo formal education at all",
      str_detect(term, "31_00") ~ "College and above & Adulthood edu vs.\nNo formal education at all",
      str_detect(term, "01_00") ~ "Only adulthood edu vs.\nNo formal education at all",
      str_detect(term, "11_10") ~ "Elementary school or less & Adulthood edu vs.\nElementary school or less & No adulthood edu",
      str_detect(term, "21_20") ~ "Middle school level & Adulthood edu vs.\nMiddle school level & No adulthood edu",
      str_detect(term, "31_30") ~ "College and above & Adulthood edu vs.\nCollege and above & No adulthood edu",
      str_detect(term, "10_00") ~ "Elementary school or less & No adulthood edu vs.\nNo formal education at all",
      str_detect(term, "20_00") ~ "Middle school level & No adulthood edu vs.\nNo formal education at all",
      str_detect(term, "30_00") ~ "College and above & No adulthood edu vs.\nNo formal education at all")) 

CI_forplot %<>%
  mutate(term = factor(term, levels = unique(CI_forplot$term)),
         label = factor(label, levels = unique(CI_forplot$label)),
         outcome = factor(outcome, levels = unique(CI_forplot$outcome)))

CI_forplot %>%
  mutate(outcome = factor(case_when(outcome == "imrc" ~ "Immediate word recall",
                                    outcome == "dlrc" ~ "Delayed word recall"),
                          levels = c("Immediate word recall", "Delayed word recall")))%>%
  ggplot(aes(x = pe, y = label)) +
  geom_point(position = position_dodge((width = 0.3)), color = "#0083C3") +
  geom_errorbarh(aes(xmin = p2.5th, xmax = p97.5th), height = 0.1, 
                 color = "#0083C3",
                 position = position_dodge()) +
  geom_vline(xintercept = 0) +
  # scale_x_continuous(limits = c(-0.7, 3.3)) +
  # scale_x_continuous(limits=c(-3, 2), breaks = seq(0, 2.5, 0.5)) +
  scale_y_discrete(limits = rev) +
  facet_wrap(~outcome) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave(file = here::here("output", "figures", "slope_CI_imrc_dlrc_GEE.png"), 
       device = "png", width = 7, height = 5, units = "in", dpi = 300)

#---- OLD ----
# for (y in c("imrc", "dlrc")){
#   temp <- tibble()
#   # for (m in 1:10){
#   # for (b in 0:1000){
#   for (scenario_num in 1:80){
#     temp <- bind_rows(
#       temp, readRDS(here::here("data", "hoffman2", "model_results",
#                              paste0("MSM_coef_", y, "_", b, ".RDS"))))
#     # paste0("MSM_coef_", y, 
#     #        "_imp_", m,"_boot_", b, ".RDS")))
#   }
#   assign(paste0(y, "_MSM_coef"), temp)
#   # }
# }

# warnings
for (y in c("imrc", "dlrc")){
  warnings <- tibble()
  # for (m in 1:10){
  for (b in 0:1000){
    warnings <- bind_rows(
      warnings, readRDS(here::here("data", "hoffman2", "warnings",
                                   paste0("warnings_", y, "_", b, ".RDS"))))
    # paste0("warnings_", y, 
    #        "_imp_", m,"_boot_", b, ".RDS")))
  }
  assign(paste0(y, "_warnings"), warnings)
  # }
}
rm(warnings)