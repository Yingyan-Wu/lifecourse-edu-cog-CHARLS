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
for (y in c("imrc", "dlrc")){
  temp <- tibble()
  # for (m in 1:10){
  for (b in 0:1000){
    temp <- bind_rows(
      temp, readRDS(here::here("data", "hoffman2", "model_results",
                               paste0("MSM_coef_", y, "_", b, ".RDS"))))
    # paste0("MSM_coef_", y, 
    #        "_imp_", m,"_boot_", b, ".RDS")))
  }
  assign(paste0(y, "_MSM_coef"), temp)
  # }
}
rm(temp)

save(imrc_MSM_coef, dlrc_MSM_coef, 
     file = here::here("data", "model_results", "MSM_coef_imrc_dlrc.RData"))

load(here::here("data", "model_results", "MSM_coef_imrc_dlrc.RData"))

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

# Check warnings
imrc_warnings %>%
  select(starts_with("w_")) %>%
  apply(., 2, table, useNA = "ifany")

dlrc_warnings %>%
  select(starts_with("w_")) %>%
  apply(., 2, table, useNA = "ifany")

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
  temp <- get(paste0(y, "_MSM_coef")) %>%
    filter(i_boot != 0) %>%
    select(-i_boot, -scenario_num) %>%
    reframe(across(everything(), ~quantile(.x, c(0.025, 0.975)))) %>%
    mutate(estimate = c("p2.5th", "p97.5th"),
           outcome = y) %>%
    select(estimate, outcome, everything())
  temp2 <- get(paste0(y, "_MSM_coef")) %>%
    filter(i_boot == 0) %>%
    select(-i_boot, -scenario_num) %>%
    mutate(estimate = "pe",
           outcome = y) %>%
    select(estimate, outcome, everything())
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
