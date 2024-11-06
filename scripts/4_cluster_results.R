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

# #---- Load the results ----
# for (anlys in c("main", "sens", "sens2")){
#   temp_imrc <- tibble()
#   for (scenario_num in 1:40){
#     temp_imrc <- bind_rows(
#       temp_imrc, read_csv(
#         here::here("data", "hoffman2", paste0(anlys, "_model_results"),
#                    paste0("MSM_coef_imrc_scenario_", scenario_num, ".csv")),
#         show_col_types = F))
#   }
# 
#   temp_dlrc <- tibble()
#   for (scenario_num in 41:80){
#     temp_dlrc <- bind_rows(
#       temp_dlrc, read_csv(
#         here::here("data", "hoffman2", paste0(anlys, "_model_results"),
#                    paste0("MSM_coef_dlrc_scenario_", scenario_num, ".csv")),
#         show_col_types = F))
#   }
# 
#   # Check if there's missing run
#   print(table(temp_imrc$n_imp))
#   print(table(temp_imrc$i_boot))
#   print(table(temp_dlrc$n_imp))
#   print(table(temp_dlrc$i_boot))
# 
#   assign(paste0("imrc_MSM_coef_", anlys), temp_imrc)
#   assign(paste0("dlrc_MSM_coef_", anlys), temp_dlrc)
# 
#   save(list = c(paste0("imrc_MSM_coef_", anlys),
#                 paste0("dlrc_MSM_coef_", anlys)),
#        file = here::here("data", "model_results",
#                          paste0(anlys, "_MSM_coef_imrc_dlrc.RData")))
# }

load(here::here("data", "model_results", "main_MSM_coef_imrc_dlrc.RData"))
load(here::here("data", "model_results", "sens_MSM_coef_imrc_dlrc.RData"))
load(here::here("data", "model_results", "sens2_MSM_coef_imrc_dlrc.RData"))

# #---- Check the warnings ----
# for (anlys in c("main", "sens", "sens2")){
#   imrc_warnings <- tibble()
#   for (scenario_num in 1:40){
#     imrc_warnings <- bind_rows(
#       imrc_warnings, read_csv(
#         here::here("data", "hoffman2", paste0(anlys, "_warnings"),
#                    paste0("warnings_imrc_scenario_", scenario_num, ".csv")),
#         show_col_types = F))
#   }
# 
#   dlrc_warnings <- tibble()
#   for (scenario_num in 41:80){
#     dlrc_warnings <- bind_rows(
#       dlrc_warnings, read_csv(
#         here::here("data", "hoffman2", paste0(anlys, "_warnings"),
#                    paste0("warnings_dlrc_scenario_", scenario_num, ".csv")),
#         show_col_types = F))
#   }
# 
#   assign(paste0("imrc_warnings_", anlys), imrc_warnings)
#   assign(paste0("dlrc_warnings_", anlys), dlrc_warnings)
# }
# 
# # Check warnings
# for (anlys in c("main", "sens", "sens2")){
#   print(get(paste0("imrc_warnings_", anlys)) %>%
#           select(starts_with("w_"), -ends_with("msg")) %>%
#           apply(., 2, table, useNA = "ifany"))
# 
#   print(get(paste0("dlrc_warnings_", anlys)) %>%
#           select(starts_with("w_"), -ends_with("msg")) %>%
#           apply(., 2, table, useNA = "ifany"))
# }
# 
# rm(list = ls()[str_detect(ls(), "warnings")])

#---- Calculate estimands ----
for (anlys in c("main", "sens", "sens2")){
  for (y in c("imrc", "dlrc")){
    temp <- get(paste0(y, "_MSM_coef_", anlys)) %>%
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
    
    temp_pred_fu <- expand_grid(n_imp = 1:20,
                                i_boot = 0:1000,
                                fu_time_yrs = c(0, 2, 4, 7, 9)) %>%
      left_join(get(paste0(y, "_MSM_coef_", anlys)), by = c("n_imp", "i_boot")) %>%
      mutate(rc_00 = `(Intercept)` + fu_time*fu_time_yrs,
             rc_10 = `(Intercept)` + doX0_1 + fu_time*fu_time_yrs + `fu_time:doX0_1`*fu_time_yrs,
             rc_20 = `(Intercept)` + doX0_2 + fu_time*fu_time_yrs + `fu_time:doX0_2`*fu_time_yrs,
             rc_30 = `(Intercept)` + doX0_3 + fu_time*fu_time_yrs + `fu_time:doX0_3`*fu_time_yrs,
             rc_01 = `(Intercept)` + doX1_1 + fu_time*fu_time_yrs + `fu_time:doX1_1`*fu_time_yrs,
             rc_11 = `(Intercept)` + doX0_1 + doX1_1 + `doX0_1:doX1_1` + 
               fu_time*fu_time_yrs + `fu_time:doX0_1`*fu_time_yrs + 
               `fu_time:doX1_1`*fu_time_yrs + `fu_time:doX0_1:doX1_1`*fu_time_yrs,
             rc_21 = `(Intercept)` + doX0_2 + doX1_1 + `doX0_2:doX1_1` + 
               fu_time*fu_time_yrs + `fu_time:doX0_2`*fu_time_yrs + 
               `fu_time:doX1_1`*fu_time_yrs + `fu_time:doX0_2:doX1_1`*fu_time_yrs,
             rc_31 = `(Intercept)` + doX0_3 + doX1_1 + `doX0_3:doX1_1` + 
               fu_time*fu_time_yrs + `fu_time:doX0_3`*fu_time_yrs + 
               `fu_time:doX1_1`*fu_time_yrs + `fu_time:doX0_3:doX1_1`*fu_time_yrs) %>%
      select(n_imp, i_boot, fu_time_yrs, starts_with("rc_"))
    
    temp_mean_PO <- get(paste0(y, "_MSM_coef_", anlys)) %>%
      select(n_imp, i_boot, starts_with("mean_PO"))
    
    assign(paste0(y, "_MSM_coef_", anlys), temp)
    assign(paste0(y, "_pred_fu_", anlys), temp_pred_fu)
    assign(paste0(y, "_mean_PO_", anlys), temp_mean_PO)
  }
}

rm(list = ls()[str_detect(ls(), "temp")])

#---- PE, 95% CI ----
imrc_dlrc_CI <- tibble()
imrc_dlrc_pred_fu_CI <- tibble()
imrc_dlrc_mean_PO_CI <- tibble()
for (anlys in c("main", "sens", "sens2")){
  for (y in c("imrc", "dlrc")){
    # 2.5th and 97.5th
    temp <- get(paste0(y, "_MSM_coef_", anlys)) %>%
      filter(i_boot != 0) %>%
      select(-i_boot, -scenario_num) %>%
      reframe(across(everything(), ~quantile(.x, c(0.025, 0.975)))) %>%
      mutate(estimate = c("p2.5th", "p97.5th"),
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    # PE (mean across imputation)
    temp2 <- get(paste0(y, "_MSM_coef_", anlys)) %>%
      filter(i_boot == 0) %>%
      select(-i_boot, -scenario_num) %>%
      reframe(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
      mutate(estimate = "pe",
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    imrc_dlrc_CI <- bind_rows(imrc_dlrc_CI, temp2, temp)
    
    # 2.5th and 97.5 th
    temp_pred_fu <- get(paste0(y, "_pred_fu_", anlys)) %>%
      filter(i_boot != 0) %>%
      select(-i_boot, -n_imp) %>%
      group_by(fu_time_yrs) %>%
      reframe(across(everything(), ~quantile(.x, c(0.025, 0.975)))) %>%
      mutate(estimate = rep(c("p2.5th", "p97.5th"), n_distinct(fu_time_yrs)),
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    # Mean
    temp_pred_fu2 <- get(paste0(y, "_pred_fu_", anlys)) %>%
      filter(i_boot == 0) %>%
      select(-i_boot, -n_imp) %>%
      group_by(fu_time_yrs) %>%
      reframe(across(everything(), ~mean(.x))) %>%
      mutate(estimate = "pe",
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    imrc_dlrc_pred_fu_CI <- bind_rows(imrc_dlrc_pred_fu_CI, temp_pred_fu2, temp_pred_fu)
    
    # 2.5th and 97.5 th
    temp_mean_PO <- get(paste0(y, "_mean_PO_", anlys)) %>%
      filter(i_boot != 0) %>%
      select(-i_boot, -n_imp) %>%
      reframe(across(everything(), ~quantile(.x, c(0.025, 0.975)))) %>%
      mutate(estimate = c("p2.5th", "p97.5th"),
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    # Mean
    temp_mean_PO2 <- get(paste0(y, "_mean_PO_", anlys)) %>%
      filter(i_boot == 0) %>%
      select(-i_boot, -n_imp) %>%
      reframe(across(everything(), ~mean(.x))) %>%
      mutate(estimate = "pe",
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    imrc_dlrc_mean_PO_CI <- bind_rows(imrc_dlrc_mean_PO_CI, temp_mean_PO2, temp_mean_PO)
  }
}

rm(list = ls()[str_detect(ls(), "temp")])

#---- Estimates ----
imrc_dlrc_CI %>%
  select(analysis, estimate, outcome, contains("slope"))

#---- Whisker plot ----
CI_forplot <- imrc_dlrc_CI %>%
  select(analysis, estimate, outcome, contains("slope")) %>%
  pivot_longer(cols = -c(analysis, estimate, outcome),
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
      str_detect(term, "30_00") ~ "College and above & No adulthood edu vs.\nNo formal education at all")) %>%
  arrange(analysis, outcome, term)

CI_forplot %<>%
  mutate(
    term = factor(term, levels = unique(CI_forplot$term)),
    label = factor(label, levels = unique(CI_forplot$label)), 
    outcome = factor(case_when(outcome == "imrc" ~ "Immediate word recall",
                               outcome == "dlrc" ~ "Delayed word recall"),
                     levels = c("Immediate word recall", "Delayed word recall")))

##---- main analysis ----
CI_forplot %>%
  filter(analysis == "main") %>%
  ggplot(aes(x = pe, y = label)) +
  geom_point(position = position_dodge((width = 0.3)), color = "#0083C3") +
  geom_errorbarh(aes(xmin = p2.5th, xmax = p97.5th), height = 0.1, 
                 color = "#0083C3",
                 position = position_dodge()) +
  geom_vline(xintercept = 0) +
  # scale_x_continuous(limits = c(-0.7, 3.3)) +
  # scale_x_continuous(limits=c(-3, 2), breaks = seq(0, 2.5, 0.5)) +
  # scale_y_discrete(limits = rev) +
  facet_wrap(~outcome) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave(file = here::here("output", "figures", "main_slope_CI_imrc_dlrc_GEE.png"), 
       device = "png", width = 7, height = 5, units = "in", dpi = 300)

##---- sens2: restricting to 2011 sample ----
CI_forplot %>%
  filter(analysis == "sens2") %>%
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

ggsave(file = here::here("output", "figures", "sens2_slope_CI_imrc_dlrc_GEE.png"), 
       device = "png", width = 7, height = 5, units = "in", dpi = 300)

#---- Predicted outcomes trajectory plot ----
PO_CI_forplot <- imrc_dlrc_mean_PO_CI %>%
  pivot_longer(cols = starts_with("mean_PO"), 
               names_to = c("intervention", "fu_time_yrs"), 
               names_pattern = "mean_PO_(.*)_yr(.*)",
               values_to = "rc") %>%
  pivot_wider(names_from = estimate, values_from = rc) %>%
  mutate(
    fu_time_yrs = as.numeric(fu_time_yrs),
    label = case_when(
      intervention == "00" ~ "No formal education at all",
      intervention == "10" ~ "Elementary school or less & No adulthood edu",
      intervention == "20" ~ "Middle school level & No adulthood edu",
      intervention == "30" ~ "College and above & No adulthood edu",
      intervention == "01" ~ "Only adulthood edu",
      intervention == "11" ~ "Elementary school or less & Adulthood edu",
      intervention == "21" ~ "Middle school level & Adulthood edu",
      intervention == "31" ~ "College and above & Adulthood edu"
    ),
    label_adult = case_when(
      intervention %in% c("00", "10", "20", "30") ~ "No adulthood edu",
      intervention %in% c("01", "11", "21", "31") ~ "Adulthood edu"),
    comp_childhood_only = case_when(intervention %in% c(00, 10, 20, 30) ~ 1,
                                    TRUE ~ 0),
    comp_adult_only = case_when(intervention %in% c("00", "01") ~ 1,
                                TRUE ~ 0),
    comp_adult_childhood = case_when(intervention %in% c("00", "01") ~ "No early life formal education",
                                     intervention %in% c(10, 11) ~ "Elementary school or less",
                                     intervention %in% c(20, 21) ~ "Middle school level",
                                     intervention %in% c(30, 31) ~ "College and above"),
    intervention = factor(intervention, 
                          levels = c("00", "01", "10", "11", "20", "21", "30", "31"))) %>%
  arrange(outcome, fu_time_yrs, intervention)

PO_CI_forplot %<>%
  mutate(label = factor(label, levels = unique(pred_CI_forplot$label)),
         label_adult = factor(label_adult, levels = unique(pred_CI_forplot$label_adult)),
         comp_adult_childhood = factor(
           comp_adult_childhood, levels = unique(pred_CI_forplot$comp_adult_childhood))) %>%
  mutate(outcome = factor(case_when(outcome == "imrc" ~ "Immediate word recall",
                                    outcome == "dlrc" ~ "Delayed word recall"),
                          levels = c("Immediate word recall", "Delayed word recall")))

color_palette <- c("#E9C4A6", "#99806c", "#54B663", "#6a8d52",
                   "#5698c3", "#3271ae", "#9B4BDD", "#65318E")
##---- Figures ----
for (anlys in c("main", "sens", "sens2")){
  p <- PO_CI_forplot %>%
    filter(analysis == anlys) %>%
    ggplot(aes(x = fu_time_yrs, y = pe, 
               color = label, group = label)) +
    geom_point(position = position_dodge((width = 1))) +
    geom_errorbar(aes(ymin = p2.5th, ymax = p97.5th), 
                  position = position_dodge((width = 1)), width = 0.8) +
    geom_line(position = position_dodge((width = 1))) + 
    facet_grid(rows = vars(outcome), 
               cols = vars(comp_adult_childhood)) +
    scale_color_manual(name = NULL, values = color_palette) +
    # labels = label_adult) +
    scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
    scale_y_continuous(breaks = 1:6) +
    labs(x = "Follow-up time (years)", y = "Word recall scores") +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size = 10, color = "black"), 
          strip.text.x = element_text(size = 10, color = "black"),
          strip.text.y = element_text(size = 10, color = "black"),
          axis.text.x = element_text(size = 10, color = "black"), 
          axis.text.y = element_text(size = 10, color = "black"),
          legend.text = element_text(size = 10, color = "black"))
  ggsave(p, file = here::here("output", "figures", 
                              paste0(anlys, "_PO_fu_CI_imrc_dlrc_GEE.png")), 
         device = "png", width = 12, height = 7, units = "in", dpi = 300)
  assign(paste0("p_PO_", anlys), p)
}
rm(p)
p_PO_main
p_PO_sens # Unweighted
p_PO_sens2 # 2011 only

#---- Trajectory plot ----
pred_CI_forplot <- imrc_dlrc_pred_fu_CI %>%
  pivot_longer(cols = starts_with("rc_"), 
               names_to = "intervention", names_prefix = "rc_", 
               values_to = "rc") %>%
  pivot_wider(names_from = estimate, values_from = rc) %>%
  mutate(label = case_when(
    intervention == "00" ~ "No formal education at all",
    intervention == "10" ~ "Elementary school or less & No adulthood edu",
    intervention == "20" ~ "Middle school level & No adulthood edu",
    intervention == "30" ~ "College and above & No adulthood edu",
    intervention == "01" ~ "Only adulthood edu",
    intervention == "11" ~ "Elementary school or less & Adulthood edu",
    intervention == "21" ~ "Middle school level & Adulthood edu",
    intervention == "31" ~ "College and above & Adulthood edu"
  ),
  label_adult = case_when(
    intervention %in% c("00", "10", "20", "30") ~ "No adulthood edu",
    intervention %in% c("01", "11", "21", "31") ~ "Adulthood edu"),
  comp_childhood_only = case_when(intervention %in% c(00, 10, 20, 30) ~ 1,
                                  TRUE ~ 0),
  comp_adult_only = case_when(intervention %in% c("00", "01") ~ 1,
                              TRUE ~ 0),
  comp_adult_childhood = case_when(intervention %in% c("00", "01") ~ "No early life formal education",
                                   intervention %in% c(10, 11) ~ "Elementary school or less",
                                   intervention %in% c(20, 21) ~ "Middle school level",
                                   intervention %in% c(30, 31) ~ "College and above"),
  intervention = factor(intervention, 
                        levels = c("00", "01", "10", "11", "20", "21", "30", "31"))) %>%
  arrange(outcome, fu_time_yrs, intervention)

pred_CI_forplot %<>%
  mutate(label = factor(label, levels = unique(pred_CI_forplot$label)),
         label_adult = factor(label_adult, levels = unique(pred_CI_forplot$label_adult)),
         comp_adult_childhood = factor(
           comp_adult_childhood, levels = unique(pred_CI_forplot$comp_adult_childhood))) %>%
  mutate(outcome = factor(case_when(outcome == "imrc" ~ "Immediate word recall",
                                    outcome == "dlrc" ~ "Delayed word recall"),
                          levels = c("Immediate word recall", "Delayed word recall")))

color_palette <- c("#E9C4A6", "#99806c", "#54B663", "#6a8d52",
                   "#5698c3", "#3271ae", "#9B4BDD", "#65318E")
##---- Figures ----
for (anlys in c("main", "sens", "sens2")){
  p <- pred_CI_forplot %>%
    filter(analysis == anlys) %>%
    ggplot(aes(x = fu_time_yrs, y = pe, 
               color = label, group = label)) +
    geom_point(position = position_dodge((width = 1))) +
    geom_errorbar(aes(ymin = p2.5th, ymax = p97.5th), 
                  position = position_dodge((width = 1)), width = 0.8) +
    geom_line(position = position_dodge((width = 1))) + 
    facet_grid(rows = vars(outcome), 
               cols = vars(comp_adult_childhood)) +
    scale_color_manual(name = NULL, values = color_palette) +
    # labels = label_adult) +
    scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
    scale_y_continuous(breaks = 1:6) +
    labs(x = "Follow-up time (years)", y = "Word recall scores") +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size = 10, color = "black"), 
          strip.text.x = element_text(size = 10, color = "black"),
          strip.text.y = element_text(size = 10, color = "black"),
          axis.text.x = element_text(size = 10, color = "black"), 
          axis.text.y = element_text(size = 10, color = "black"),
          legend.text = element_text(size = 10, color = "black"))
  ggsave(p, file = here::here("output", "figures", 
                              paste0(anlys, "_pred_fu_CI_imrc_dlrc_GEE.png")), 
         device = "png", width = 12, height = 7, units = "in", dpi = 300)
  assign(paste0("p_", anlys), p)
}
rm(p)
p_main
p_sens # Unweighted
p_sens2 # 2011 only

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