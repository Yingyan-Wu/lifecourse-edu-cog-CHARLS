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
#   assign(paste0("imrc_results_", anlys), temp_imrc)
#   assign(paste0("dlrc_results_", anlys), temp_dlrc)
# 
#   save(list = c(paste0("imrc_results_", anlys),
#                 paste0("dlrc_results_", anlys)),
#        file = here::here("data", "model_results",
#                          paste0(anlys, "_results_imrc_dlrc.RData")))
# }

load(here::here("data", "model_results", "main_results_imrc_dlrc.RData"))
load(here::here("data", "model_results", "sens_results_imrc_dlrc.RData"))
load(here::here("data", "model_results", "sens2_results_imrc_dlrc.RData"))

#---- Check the warnings ----
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

#---- Reframe the results to PE and CI ----
for (anlys in c("main", "sens", "sens2")){
  for (y in c("imrc", "dlrc")){
    temp <- get(paste0(y, "_results_", anlys)) %>%
      select(-scenario_num, -ends_with("_sec"), -starts_with("mean_")) 
    
    temp_mean_PO <- get(paste0(y, "_results_", anlys)) %>%
      select(n_imp, i_boot, starts_with("mean_PO"))
    
    temp_mean_predMSM <- get(paste0(y, "_results_", anlys)) %>%
      select(n_imp, i_boot, starts_with("mean_predMSM")) 
    
    # Adding change from baseline for predMSM
    for (yr in c(2, 4, 7, 9)){
      for (itvn in c("00", "01", "10", "11", "20", "21", "30", "31")){
        temp_mean_predMSM %<>%
          mutate(!!paste0("mean_predMSM_noPE_", itvn, "_yr", yr, "_0") :=
                   !!sym(paste0("mean_predMSM_noPE_", itvn, "_yr", yr)) -
                   !!sym(paste0("mean_predMSM_noPE_", itvn, "_yr0")))
      }
    }
    
    # Contrasts
    for (yr in c("0", "2_0", "4_0", "7_0", "9_0")){
      temp_mean_predMSM %<>%
        mutate(
          # Only continuing education vs. No formal education
          !!paste0("mean_predMSM_noPE_01_00_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_01_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_00_yr", yr)),
          # Both education vs. No formal education
          !!paste0("mean_predMSM_noPE_11_00_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_11_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_00_yr", yr)),
          !!paste0("mean_predMSM_noPE_21_00_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_21_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_00_yr", yr)),
          !!paste0("mean_predMSM_noPE_31_00_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_31_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_00_yr", yr)),
          # Both education vs. No continuing education with the same early life edu intervention
          !!paste0("mean_predMSM_noPE_11_10_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_11_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_10_yr", yr)),
          !!paste0("mean_predMSM_noPE_21_20_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_21_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_20_yr", yr)),
          !!paste0("mean_predMSM_noPE_31_30_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_31_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_30_yr", yr)),
          # Only early life education (no continuing education) vs. No formal education
          !!paste0("mean_predMSM_noPE_10_00_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_10_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_00_yr", yr)),
          !!paste0("mean_predMSM_noPE_20_00_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_20_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_00_yr", yr)),
          !!paste0("mean_predMSM_noPE_30_00_yr", yr) :=
            !!sym(paste0("mean_predMSM_noPE_30_yr", yr)) -
            !!sym(paste0("mean_predMSM_noPE_00_yr", yr)))
    }
    
    assign(paste0(y, "_MSM_coef_", anlys), temp)
    assign(paste0(y, "_predMSM_", anlys), temp_mean_predMSM)
    assign(paste0(y, "_mean_PO_", anlys), temp_mean_PO)
  }
}

rm(list = ls()[str_detect(ls(), "temp")])

##---- PE, 95% CI ----
imrc_dlrc_coef_CI <- tibble()
imrc_dlrc_predMSM_CI <- tibble()
imrc_dlrc_mean_PO_CI <- tibble()
for (anlys in c("main", "sens", "sens2")){
  for (y in c("imrc", "dlrc")){
    # coefficients
    # 2.5th and 97.5th
    temp <- get(paste0(y, "_MSM_coef_", anlys)) %>%
      filter(i_boot != 0) %>%
      select(-i_boot) %>%
      reframe(across(everything(), ~quantile(.x, c(0.025, 0.975)))) %>%
      mutate(estimate = c("p2.5th", "p97.5th"),
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    # PE (mean across imputation)
    temp2 <- get(paste0(y, "_MSM_coef_", anlys)) %>%
      filter(i_boot == 0) %>%
      select(-i_boot) %>%
      reframe(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
      mutate(estimate = "pe",
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    imrc_dlrc_coef_CI <- bind_rows(imrc_dlrc_coef_CI, temp2, temp)
    
    # Prediction MSM and Change from Prediction MSM
    # 2.5th and 97.5 th
    temp_predMSM <- get(paste0(y, "_predMSM_", anlys)) %>%
      filter(i_boot != 0) %>%
      select(-i_boot, -n_imp) %>%
      reframe(across(everything(), ~quantile(.x, c(0.025, 0.975)))) %>%
      mutate(estimate = rep(c("p2.5th", "p97.5th")),
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    # Mean
    temp_predMSM2 <- get(paste0(y, "_predMSM_", anlys)) %>%
      filter(i_boot == 0) %>%
      select(-i_boot, -n_imp) %>%
      reframe(across(everything(), ~mean(.x))) %>%
      mutate(estimate = "pe",
             outcome = y,
             analysis = anlys) %>%
      select(estimate, outcome, analysis, everything())
    imrc_dlrc_predMSM_CI <- bind_rows(imrc_dlrc_predMSM_CI, temp_predMSM2, temp_predMSM)
    
    # Mean Potential outcomes
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

# # Sanity check
# imrc_dlrc_predMSM_CI %>%
#   filter(outcome == "imrc", analysis == "main") %>%
#   select(estimate, starts_with("mean_predMSM_noPE_")) %>% 
#   select(estimate, ends_with("yr0")) %>%
#   view()

# imrc_dlrc_coef_CI %>% view()
imrc_dlrc_predMSM_CI_long <- imrc_dlrc_predMSM_CI %>% 
  select(-ends_with("_0"), -ends_with("0_00_yr0"), -ends_with("1_00_yr0"),
         -ends_with("0_10_yr0"), -ends_with("1_10_yr0"),
         -ends_with("1_20_yr0"),
         -ends_with("1_30_yr0")) %>%
  pivot_longer(cols = -c(analysis, estimate, outcome),
               names_to = c("PE", "intervention", "fu_time_yrs"),
               names_pattern = "mean_predMSM_(.*)_(.*)_yr(.)",
               values_to = "rc") %>%
  mutate(fu_time_yrs = as.numeric(fu_time_yrs))

imrc_dlrc_predMSM_noPE_contrasts_long <- imrc_dlrc_predMSM_CI %>% 
  select(analysis, estimate, outcome, ends_with("_0")) %>%
  # select(analysis, estimate, outcome, contains("noPE")) %>%
  pivot_longer(cols = -c(analysis, estimate, outcome),
               names_to = c("intervention_contrasts", "fu_time_yrs"),
               names_pattern = "mean_predMSM_noPE_(.*)_yr(.)_0",
               values_to = "rc_change") %>%
  rbind(imrc_dlrc_predMSM_CI %>% 
          select(analysis, estimate, outcome, ends_with("0_yr0"), ends_with("1_yr0")) %>%
          select(analysis, estimate, outcome, contains("noPE")) %>%
          pivot_longer(cols = -c(analysis, estimate, outcome),
                       names_to = c("intervention_contrasts"),
                       names_pattern = "mean_predMSM_noPE_(.*)_yr0",
                       values_to = "rc_change") %>% mutate(fu_time_yrs = 0)) %>%
  mutate(fu_time_yrs = as.numeric(fu_time_yrs))

imrc_dlrc_mean_PO_CI_long <- imrc_dlrc_mean_PO_CI %>% 
  pivot_longer(cols = -c(analysis, estimate, outcome),
               names_to = c("intervention", "fu_time_yrs"),
               names_pattern = "mean_PO_(.*)_yr(.)",
               values_to = "rc") %>%
  mutate(fu_time_yrs = as.numeric(fu_time_yrs))

##---- dataset For trajectory plot ----
for (dat in c("predMSM", "mean_PO")){
  temp <- get(paste0("imrc_dlrc_", dat, "_CI_long")) %>%
    pivot_wider(names_from = estimate, values_from = rc) %>%
    mutate(label = case_when(
      intervention == "00" ~ "No formal education/illiterate",
      intervention == "10" ~ "Primary school & no adult edu",
      intervention == "20" ~ "Secondary school & no adult edu",
      intervention == "30" ~ "College & no adult edu",
      intervention == "01" ~ "Only adult edu",
      intervention == "11" ~ "Primary school & adult edu",
      intervention == "21" ~ "Secondary school & adult edu",
      intervention == "31" ~ "College & adult edu"),
      label_adult = case_when(
        intervention %in% c("00", "10", "20", "30") ~ "no adult edu",
        intervention %in% c("01", "11", "21", "31") ~ "adult edu"),
      comp_childhood_only = case_when(intervention %in% c(00, 10, 20, 30) ~ 1,
                                      TRUE ~ 0),
      comp_adult_only = case_when(intervention %in% c("00", "01") ~ 1,
                                  TRUE ~ 0),
      comp_adult_childhood = case_when(intervention %in% c("00", "01") ~ "No early-life formal education/illiterate",
                                       intervention %in% c(10, 11) ~ "Primary school",
                                       intervention %in% c(20, 21) ~ "Secondary school",
                                       intervention %in% c(30, 31) ~ "College"),
      intervention = factor(intervention, 
                            levels = c("00", "01", "10", "11", "20", "21", "30", "31"))) %>%
    arrange(outcome, fu_time_yrs, intervention)
  
  label_order <- temp %>%
    mutate(intervention = factor(intervention, 
                                 levels = c("01", "00", "11", "10", "21", "20", "31", "30"))) %>%
    arrange(intervention) %>% pull(label) %>% unique() %>% as.vector()
  
  temp %<>%
    mutate(label = factor(label, levels = label_order),
           label_adult = factor(label_adult, levels = unique(temp$label_adult)),
           comp_adult_childhood = factor(
             comp_adult_childhood, levels = unique(temp$comp_adult_childhood))) %>%
    mutate(outcome = factor(case_when(outcome == "imrc" ~ "Immediate word recall",
                                      outcome == "dlrc" ~ "Delayed word recall"),
                            levels = c("Immediate word recall", "Delayed word recall")))
  
  assign(paste0(dat, "_forplot_traj"), temp)
}
rm(temp)

# #---- Predicted outcomes trajectory plot ----
# color_palette <- c("#AA812F", "#F8BC44", "#4B7F3D", "#76CF65",
#                    "#3271ae","#48A4FF", "#65318E", "#B657FF")
# linetype_palette <- rep(c("solid", "dashed"), 4)
# ##---- Mean PO traj plot ----
# for (anlys in c("main", "sens", "sens2")){
#   p <- mean_PO_forplot_traj %>%
#     filter(analysis == anlys) %>%
#     ggplot(aes(x = fu_time_yrs, y = pe, 
#                color = label, group = label, linetype = label)) +
#     geom_point(position = position_dodge((width = 1))) +
#     geom_errorbar(aes(ymin = p2.5th, ymax = p97.5th), 
#                   position = position_dodge((width = 1)), width = 0.8) +
#     geom_line(position = position_dodge((width = 1))) +
#     facet_grid(rows = vars(outcome), 
#                cols = vars(comp_adult_childhood)) +
#     scale_color_manual(name = NULL, values = color_palette) +
#     scale_linetype_manual(name = NULL, values = linetype_palette) +
#     # labels = label_adult) +
#     scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
#     scale_y_continuous(breaks = 1:6) +
#     labs(x = "Follow-up time (years)", y = "Word recall scores") +
#     theme_bw() +
#     theme(legend.position = "bottom",
#           text = element_text(size = 12, color = "black"), 
#           strip.text.x = element_text(size = 12, color = "black"),
#           strip.text.y = element_text(size = 12, color = "black"),
#           axis.text.x = element_text(size = 12, color = "black"), 
#           axis.text.y = element_text(size = 12, color = "black"),
#           legend.text = element_text(size = 11, color = "black"))
#   ggsave(p, file = here::here("output", "figures", 
#                               paste0(anlys, "_PO_fu_CI_imrc_dlrc_GEE.png")), 
#          device = "png", width = 12, height = 7, units = "in", dpi = 300)
#   assign(paste0("p_PO_", anlys), p)
# }
# rm(p)
# p_PO_main
# p_PO_sens # Unweighted
# p_PO_sens2 # 2011 only

##---- MSM GEE pred traj plot ----
color_palette <- c("#AA812F", "#F8BC44", "#4B7F3D", "#76CF65",
                   "#3271ae","#48A4FF", "#65318E", "#B657FF")
linetype_palette <- rep(c("solid", "dashed"), 4)
# linetype_palette <- rep(c("dashed", "solid"), 4)

###---- Figures ----
for (anlys in c("main", "sens", "sens2")){
  p <- predMSM_forplot_traj %>%
    filter(analysis == anlys, PE == "noPE") %>%
    ggplot(aes(x = fu_time_yrs, y = pe, group = label)) +
    geom_line(aes(color = label, linetype = label)) + 
    geom_ribbon(aes(ymin = p2.5th, ymax = p97.5th, fill = label), alpha = 0.4) +
    facet_grid(rows = vars(outcome),
               cols = vars(comp_adult_childhood)) +
    scale_color_manual(name = NULL, values = color_palette) +
    scale_fill_manual(name = NULL, values = color_palette) +
    scale_linetype_manual(name = NULL, values = linetype_palette) +
    # labels = label_adult) +
    scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
    scale_y_continuous(breaks = 1:6) +
    labs(x = "Follow-up time (years)", y = "Word recall scores") +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size = 12, color = "black"), 
          strip.text.x = element_text(size = 12, color = "black"),
          strip.text.y = element_text(size = 12, color = "black"),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          legend.text = element_text(size = 11, color = "black"))
  ggsave(p, file = here::here("output", "figures", 
                              paste0(anlys, "_predMSM_imrc_dlrc_GEE.png")), 
         device = "png", width = 14, height = 7, units = "in", dpi = 600)
  ggsave(p, file = here::here("output", "figures", 
                              paste0(anlys, "_predMSM_imrc_dlrc_GEE.pdf")), 
         device = "pdf", width = 14, height = 7, units = "in", dpi = 600)
  assign(paste0("p_", anlys), p)
}
rm(p)
p_main
p_sens # Unweighted
p_sens2 # 2011 only

#---- Tables ----
# mean change from baseline and contrasts between intervention
temp <- imrc_dlrc_predMSM_noPE_contrasts_long %>%
  pivot_wider(names_from = c(intervention_contrasts, estimate), 
              values_from = rc_change, 
              names_glue = "{estimate}_{intervention_contrasts}") %>%
  mutate(outcome = factor(outcome, levels = c("imrc", "dlrc"))) %>%
  arrange(outcome, fu_time_yrs)
imrc_dlrc_predMSM_noPE_contrasts_fmt <- temp %>%
  # build PE (p2.5th, p97.5th) for each contrast
  mutate(across(starts_with("pe"),
                ~sprintf("%.1f \n(%.1f, %.1f)", .x, 
                         temp[[gsub("pe_", "p2.5th_", cur_column())]], 
                         temp[[gsub("pe_", "p97.5th_", cur_column())]]), 
                .names = "pe_ci_{str_replace(.col, 'pe_', '')}")) %>%
  select(analysis, outcome, fu_time_yrs, starts_with("pe_ci_")) %>%
  set_colnames(c("analysis", "outcome", "fu_time_yrs", 
                 "No formal education",
                 "Only adult education",
                 "Primary school or less & no adult edu",
                 "Primary school or less & adult edu",
                 "Secondary school level & no adult edu",
                 "Secondary school level & adult edu",
                 "College and above & no adult edu",
                 "College and above & adult edu",
                 # Both edu vs. No
                 "Only adult edu vs.\nNo formal education",
                 "Primary school or less & adult edu vs.\nNo formal education",
                 "Secondary school level & adult edu vs.\nNo formal education",
                 "College and above & adult edu vs.\nNo formal education",
                 # adult vs no in the same early life intervention group
                 "Primary school or less & adult edu vs.\nPrimary school or less & no adult edu",
                 "Secondary school level & adult edu vs.\nSecondary school level & no adult edu",
                 "College and above & adult edu vs.\nCollege and above & no adult edu",
                 # Only early life edu vs. No
                 "Primary school or less & no adult edu vs.\nNo formal education",
                 "Secondary school level & no adult edu vs.\nNo formal education",
                 "College and above & no adult edu vs.\nNo formal education"
  ))
writexl::write_xlsx(list(
  "main" = imrc_dlrc_predMSM_noPE_contrasts_fmt %>% filter(analysis == "main") %>% select(-analysis), 
  "sens" = imrc_dlrc_predMSM_noPE_contrasts_fmt %>% filter(analysis == "sens") %>% select(-analysis),
  "sens2" = imrc_dlrc_predMSM_noPE_contrasts_fmt %>% filter(analysis == "sens2") %>% select(-analysis)),
  here::here("output", "tables", "imrc_dlrc_predMSM_noPE_contrasts.xlsx"))

#---- Contrasts figure ----
##---- Contrasts with no formal education ----
contrasts_main_no_formal <- imrc_dlrc_predMSM_noPE_contrasts_long %>% 
  filter(analysis == "main", 
         # fu_time_yrs != 0,
         str_detect(intervention_contrasts, "_00")) %>%
  mutate(label = case_when(
    intervention_contrasts == "01_00" ~ "Only adult education",
    intervention_contrasts == "10_00" ~ "Primary school & no adult education",
    intervention_contrasts == "11_00" ~ "Primary school & adult education",
    intervention_contrasts == "20_00" ~ "Secondary school & no adult education",
    intervention_contrasts == "21_00" ~ "Secondary school & adult education",
    intervention_contrasts == "30_00" ~ "College & no adult education",
    intervention_contrasts == "31_00" ~ "College & adult education")) %>%
  pivot_wider(names_from = "estimate", 
              values_from = "rc_change") %>%
  bind_rows(expand_grid(intervention_contrasts = "00",
                        label = "Ref: No formal education/illiterate",
                        analysis = "main",
                        pe = 0,
                        fu_time_yrs = c(0, 2, 4, 7, 9),
                        outcome = c("dlrc", "imrc"))) %>%
  mutate(label = factor(label, 
                        levels = c("Ref: No formal education/illiterate",
                                   "Only adult education",
                                   "Primary school & no adult education",
                                   "Primary school & adult education",
                                   "Secondary school & no adult education",
                                   "Secondary school & adult education",
                                   "College & no adult education",
                                   "College & adult education")),
         outcome = 
           factor(case_when(outcome == "imrc" ~ "Immediate word recall",
                            outcome == "dlrc" ~ "Delayed word recall"),
                  levels = c("Immediate word recall", "Delayed word recall")),
         type = case_when(fu_time_yrs == 0 ~ "Baseline",
                          fu_time_yrs != 0 ~ "Change over time"))


contrasts_color_palette <- c("#F8BC44", "#AA812F", "#76CF65", "#4B7F3D", 
                             "#48A4FF", "#3271ae", "#B657FF", "#65318E")

contrasts_main_no_formal %>%
  mutate(fu_time_yrs = factor(fu_time_yrs, levels = c(0, 2, 4, 7, 9))) %>%
  ggplot(aes(x = fu_time_yrs, y = pe, color = label)) +
  geom_point(position = position_dodge((width = .8))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_errorbar(aes(ymin = p2.5th, ymax = p97.5th),
                position = position_dodge((width = .8)),
                width = 0.8) +
  facet_grid(rows = vars(outcome), cols = vars(type), 
             scale = "free_x", space = "free_x") +
  scale_color_manual(name = NULL, values = contrasts_color_palette) +
  # scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
  scale_y_continuous(breaks = seq(-1.5, 2.5, 0.5)) +
  labs(x = "Follow-up time (years)", y = "Difference in word recall score") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, color = "black"), 
        strip.text.x = element_text(size = 12, color = "black"),
        strip.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 11, color = "black"))
ggsave(here::here("output", "figures", 
                  "contrasts_main_no_formal_imrc_dlrc_GEE.png"), 
       device = "png", width = 12, height = 7, units = "in", dpi = 600)
ggsave(here::here("output", "figures", 
                  "contrasts_main_no_formal_imrc_dlrc_GEE.pdf"), 
       device = "pdf", width = 12, height = 7, units = "in", dpi = 600)

##---- Contrasts same childhood ----
contrasts_main_same_childhood <- imrc_dlrc_predMSM_noPE_contrasts_long %>% 
  filter(analysis == "main", 
         # fu_time_yrs != 0,
         intervention_contrasts %in% c("01_00", "11_10", "21_20", "31_30")) %>%
  mutate(label = case_when(
    intervention_contrasts == "01_00" ~ "Only adult education vs. \nNo formal education/illiterate",
    intervention_contrasts == "11_10" ~ "Primary school & adult education vs. \nPrimary school & no adult education",
    intervention_contrasts == "21_20" ~ "Secondary school & adult education vs. \nSecondary school & no adult education",
    intervention_contrasts == "31_30" ~ "College & adult education vs. \nCollege & no adult education")) %>%
  mutate(label = factor(label, 
                        levels = c("Only adult education vs. \nNo formal education/illiterate",
                                   "Primary school & adult education vs. \nPrimary school & no adult education",
                                   "Secondary school & adult education vs. \nSecondary school & no adult education",
                                   "College & adult education vs. \nCollege & no adult education")),
         outcome = 
           factor(case_when(outcome == "imrc" ~ "Immediate word recall",
                            outcome == "dlrc" ~ "Delayed word recall")),
         type = case_when(fu_time_yrs == 0 ~ "Baseline",
                          fu_time_yrs != 0 ~ "Change over time")) %>%
  pivot_wider(names_from = "estimate", 
              values_from = "rc_change")

contrasts_main_same_childhood %>%
  mutate(fu_time_yrs = factor(fu_time_yrs, levels = c(0, 2, 4, 7, 9))) %>%
  ggplot(aes(x = fu_time_yrs, y = pe, color = label)) +
  geom_point(position = position_dodge((width = .5))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  # geom_line(position = position_dodge((width = 1))) +
  geom_errorbar(aes(ymin = p2.5th, ymax = p97.5th),
                position = position_dodge((width = .5)),
                width = 0.4) +
  facet_grid(rows = vars(outcome), cols = vars(type), 
             scale = "free_x", space = "free_x") +
  scale_color_manual(name = NULL, values = c("#AA812F", "#4B7F3D", "#3271ae", "#65318E")) +
  # scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
  scale_y_continuous(breaks = seq(-1.5, 2.5, 0.5)) +
  labs(x = "Follow-up time (years)", y = "Difference in word recall score") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, color = "black"), 
        strip.text.x = element_text(size = 12, color = "black"),
        strip.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 11, color = "black"))
ggsave(here::here("output", "figures", 
                  "contrasts_main_same_childhood_imrc_dlrc_GEE.png"),
       device = "png", width = 12, height = 7, units = "in", dpi = 600)
ggsave(here::here("output", "figures", 
                  "contrasts_main_same_childhood_imrc_dlrc_GEE.pdf"),
       device = "pdf", width = 12, height = 7, units = "in", dpi = 600)

# #---- Figures for Dissertation defense ----
# color_palette <- c("#AA812F", "#F8BC44", "#4B7F3D", "#8BED71", 
#                    "#3271ae","#48A4FF", "#65318E", "#B657FF")
# comp_vec <- unique(predMSM_forplot_traj$comp_adult_childhood)
# 
# for (i in 1:length(comp_vec)){
#   new_color_palette <- c(color_palette[1:(2*i)],
#                          # Make the rest of comparison transparent
#                          rep("#00000000", length(color_palette) - 2*i))
# 
#   p <- predMSM_forplot_traj %>%
#     filter(analysis == "main", PE == "noPE",
#            comp_adult_childhood %in% comp_vec[1:i]) %>%
#     ggplot(aes(x = fu_time_yrs, y = pe, group = label)) +
#     geom_line(aes(color = label, linetype = label)) + 
#     geom_ribbon(aes(ymin = p2.5th, ymax = p97.5th, fill = label), alpha = 0.4) +
#     facet_grid(rows = vars(outcome)) +
#                # cols = vars(comp_adult_childhood)) +
#     scale_color_manual(name = NULL, values = new_color_palette) +
#     scale_fill_manual(name = NULL, values = new_color_palette) +
#     scale_linetype_manual(name = NULL, values = linetype_palette) +
#     # labels = label_adult) +
#     scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
#     scale_y_continuous(limits = c(1, 6.3), breaks = 1:6) +
#     labs(x = "Follow-up time (years)", y = "Word recall scores") +
#     theme_bw() +
#     theme(legend.position = "none",
#           text = element_text(size = 12, color = "black"), 
#           strip.text.x = element_text(size = 12, color = "black"),
#           strip.text.y = element_text(size = 12, color = "black"),
#           axis.text.x = element_text(size = 12, color = "black"), 
#           axis.text.y = element_text(size = 12, color = "black"),
#           legend.text = element_text(size = 11, color = "black"))
#   ggsave(file = here::here("output", "figures", "defense",
#                            paste0("MELODEM_pred_fu_CI_imrc_dlrc_GEE_", i, ".png")),
#          device = "png", width = 7, height = 5, units = "in", dpi = 300)
# }
# rm(p)

# #---- Figures for MELODEM pre ----
# color_palette <- c("#FFD5B3", "#99806c", "#76FF8D", "#3B7F46",
#                    "#48A4FF", "#3271ae", "#B657FF", "#65318E")
# comp_vec <- unique(pred_CI_forplot$comp_adult_childhood)
# 
# for (i in 1:length(comp_vec)){
#   new_color_palette <- c(color_palette[1:(2*i)],
#                          # Make the rest of comparison transparent
#                          rep("#00000000", length(color_palette) - 2*i))
#   p <- pred_CI_forplot %>%
#     filter(analysis == "main") %>%
#     ggplot(aes(x = fu_time_yrs, y = pe,
#                color = label, group = label)) +
#     geom_point(position = position_dodge((width = 1))) +
#     geom_errorbar(aes(ymin = p2.5th, ymax = p97.5th), width = 0.8,
#                   position = position_dodge((width = 1))) +
#     geom_line(position = position_dodge((width = 1))) +
#     facet_grid(rows = vars(outcome)) +
#     # cols = vars(comp_adult_childhood)) +
#     scale_color_manual(name = NULL, values = new_color_palette) +
#     # labels = label_adult) +
#     scale_x_continuous(breaks = c(0, 2, 4, 7, 9)) +
#     scale_y_continuous(limits = c(1, 6), breaks = 1:6) +
#     labs(x = "Follow-up time (years)", y = "Word recall scores") +
#     theme_bw() +
#     theme(legend.position = "none",
#           text = element_text(size = 10, color = "black"),
#           strip.text.x = element_text(size = 10, color = "black"),
#           strip.text.y = element_text(size = 10, color = "black"),
#           axis.text.x = element_text(size = 10, color = "black"),
#           axis.text.y = element_text(size = 10, color = "black"),
#           legend.text = element_text(size = 10, color = "black"))
#   ggsave(file = here::here("output", "figures", "MELODEM_pre",
#                            paste0("MELODEM_pred_fu_CI_imrc_dlrc_GEE_", i, ".png")),
#          device = "png", width = 7, height = 5, units = "in", dpi = 300)
# }
# rm(p)

#---- OLD ----
# # Memory scores at different time points. 
# temp <- imrc_dlrc_predMSM_CI_long %>%
#   filter(PE == "noPE", fu_time_yrs %in% c(0, 2, 4, 7, 9)) %>%
#   select(-PE) %>%
#   pivot_wider(names_from = c(intervention, estimate), 
#               values_from = rc, 
#               names_glue = "{estimate}_{intervention}")
# imrc_dlrc_predMSM_noPE_fmt <- temp %>%
#   # build PE (p2.5th, p97.5th) for each contrast
#   mutate(across(starts_with("pe_"),
#                 ~sprintf("%.1f (%.1f, %.1f)", .x, 
#                          temp[[gsub("pe_", "p2.5th_", cur_column())]], 
#                          temp[[gsub("pe_", "p97.5th_", cur_column())]]), 
#                 .names = "pe_ci_{str_replace(.col, 'pe_', '')}")) %>%
#   select(analysis, outcome, fu_time_yrs, starts_with("pe_ci_")) %>%
#   set_colnames(c("analysis", "outcome", "fu_time_yrs", 
#                  "No formal education",
#                  "Only adult education",
#                  "Primary school or less & no adult edu",
#                  "Primary school or less & adult edu",
#                  "Secondary school level & no adult edu",
#                  "Secondary school level & adult edu",
#                  "College and above & no adult edu",
#                  "College and above & adult edu"))
# writexl::write_xlsx(list(
#   "main" = imrc_dlrc_predMSM_noPE_fmt %>% filter(analysis == "main") %>% select(-analysis), 
#   "sens" = imrc_dlrc_predMSM_noPE_fmt %>% filter(analysis == "sens") %>% select(-analysis),
#   "sens2" = imrc_dlrc_predMSM_noPE_fmt %>% filter(analysis == "sens2") %>% select(-analysis)),
#   here::here("output", "tables", "imrc_dlrc_predMSM_noPE.xlsx"))
# #---- Calculate estimands ----
# for (anlys in c("main", "sens", "sens2")){
#   for (y in c("imrc", "dlrc")){
#     temp <- get(paste0(y, "_results_", anlys)) %>%
#       mutate(
#         slope_00 = fu_time,
#         slope_01 = fu_time + `fu_time:doX1_1`,
#         slope_10 = fu_time + `fu_time:doX0_1`,
#         slope_11 = fu_time + `fu_time:doX0_1` + `fu_time:doX1_1` + `fu_time:doX0_1:doX1_1`,
#         slope_20 = fu_time + `fu_time:doX0_2`,
#         slope_21 = fu_time + `fu_time:doX0_2` + `fu_time:doX1_1` + `fu_time:doX0_2:doX1_1`,
#         slope_30 = fu_time + `fu_time:doX0_3`,
#         slope_31 = fu_time + `fu_time:doX0_3` + `fu_time:doX1_1` + `fu_time:doX0_3:doX1_1`,
#         # Both education vs. No formal education
#         slope_11_00 = slope_11 - slope_00,
#         slope_21_00 = slope_21 - slope_00,
#         slope_31_00 = slope_31 - slope_00,
#         # Only continuing education vs. No formal education
#         slope_01_00 = slope_01 - slope_00,
#         # Both education vs. No continuing education with the same early life edu intervention
#         slope_11_10 = slope_11 - slope_10,
#         slope_21_20 = slope_21 - slope_20,
#         slope_31_30 = slope_31 - slope_30,
#         # Only early life education (no continuing education) vs. No formal education
#         slope_10_00 = slope_10 - slope_00,
#         slope_20_00 = slope_20 - slope_00,
#         slope_30_00 = slope_30 - slope_00)
#     
#     temp_pred_fu <- expand_grid(n_imp = 1:20,
#                                 i_boot = 0:1000,
#                                 fu_time_yrs = c(0, 2, 4, 7, 9),
#                                 first_outcome = c(1, 0, 0, 0, 0)) %>%
#       left_join(get(paste0(y, "_results_", anlys)), by = c("n_imp", "i_boot")) %>%
#       mutate(rc_00 = `(Intercept)` + fu_time*fu_time_yrs,
#              rc_10 = `(Intercept)` + doX0_1 + fu_time*fu_time_yrs + `fu_time:doX0_1`*fu_time_yrs,
#              rc_20 = `(Intercept)` + doX0_2 + fu_time*fu_time_yrs + `fu_time:doX0_2`*fu_time_yrs,
#              rc_30 = `(Intercept)` + doX0_3 + fu_time*fu_time_yrs + `fu_time:doX0_3`*fu_time_yrs,
#              rc_01 = `(Intercept)` + doX1_1 + fu_time*fu_time_yrs + `fu_time:doX1_1`*fu_time_yrs,
#              rc_11 = `(Intercept)` + doX0_1 + doX1_1 + `doX0_1:doX1_1` + 
#                fu_time*fu_time_yrs + `fu_time:doX0_1`*fu_time_yrs + 
#                `fu_time:doX1_1`*fu_time_yrs + `fu_time:doX0_1:doX1_1`*fu_time_yrs,
#              rc_21 = `(Intercept)` + doX0_2 + doX1_1 + `doX0_2:doX1_1` + 
#                fu_time*fu_time_yrs + `fu_time:doX0_2`*fu_time_yrs + 
#                `fu_time:doX1_1`*fu_time_yrs + `fu_time:doX0_2:doX1_1`*fu_time_yrs,
#              rc_31 = `(Intercept)` + doX0_3 + doX1_1 + `doX0_3:doX1_1` + 
#                fu_time*fu_time_yrs + `fu_time:doX0_3`*fu_time_yrs + 
#                `fu_time:doX1_1`*fu_time_yrs + `fu_time:doX0_3:doX1_1`*fu_time_yrs) %>%
#       select(n_imp, i_boot, fu_time_yrs, starts_with("rc_"))
#     
#     temp_mean_PO <- get(paste0(y, "_results_", anlys)) %>%
#       select(n_imp, i_boot, starts_with("mean_PO"))
#     
#     assign(paste0(y, "_MSM_coef_", anlys), temp)
#     assign(paste0(y, "_pred_fu_", anlys), temp_pred_fu)
#     assign(paste0(y, "_mean_PO_", anlys), temp_mean_PO)
#   }
# }

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
# #---- Whisker plot ----
# CI_forplot <- imrc_dlrc_CI %>%
#   select(analysis, estimate, outcome, contains("slope")) %>%
#   pivot_longer(cols = -c(analysis, estimate, outcome),
#                names_to = "term") %>%
#   pivot_wider(names_from = estimate, values_from = value) 
# 
# CI_forplot  %>% filter(analysis == "main", !str_detect(term, "0_00|1_00|1_10|1_20|1_30"))
# 
# CI_forplot %<>%
#   mutate(
#     label = case_when(
#       str_detect(term, "11_00") ~ "Primary school or less & adult edu vs.\nNo formal education",
#       str_detect(term, "21_00") ~ "Secondary school level & adult edu vs.\nNo formal education",
#       str_detect(term, "31_00") ~ "College and above & adult edu vs.\nNo formal education",
#       str_detect(term, "01_00") ~ "Only adult edu vs.\nNo formal education",
#       str_detect(term, "11_10") ~ "Primary school or less & adult edu vs.\nPrimary school or less & no adult edu",
#       str_detect(term, "21_20") ~ "Secondary school level & adult edu vs.\nSecondary school level & no adult edu",
#       str_detect(term, "31_30") ~ "College and above & adult edu vs.\nCollege and above & no adult edu",
#       str_detect(term, "10_00") ~ "Primary school or less & no adult edu vs.\nNo formal education",
#       str_detect(term, "20_00") ~ "Secondary school level & no adult edu vs.\nNo formal education",
#       str_detect(term, "30_00") ~ "College and above & no adult edu vs.\nNo formal education")) %>%
#   arrange(analysis, outcome)
# 
# CI_forplot %<>%
#   mutate(
#     term = factor(term, levels = unique(CI_forplot$term)),
#     label = factor(label, levels = unique(CI_forplot$label)), 
#     outcome = factor(case_when(outcome == "imrc" ~ "Immediate word recall",
#                                outcome == "dlrc" ~ "Delayed word recall"),
#                      levels = c("Immediate word recall", "Delayed word recall")))
# 
# ##---- main analysis ----
# CI_forplot %>%
#   filter(analysis == "main", !is.na(label)) %>%
#   ggplot(aes(x = pe, y = label)) +
#   geom_point(position = position_dodge((width = 0.3)), color = "#0083C3") +
#   geom_errorbarh(aes(xmin = p2.5th, xmax = p97.5th), height = 0.1, 
#                  color = "#0083C3",
#                  position = position_dodge()) +
#   geom_vline(xintercept = 0) +
#   # scale_x_continuous(limits = c(-0.7, 3.3)) +
#   # scale_x_continuous(limits=c(-3, 2), breaks = seq(0, 2.5, 0.5)) +
#   # scale_y_discrete(limits = rev) +
#   facet_wrap(~outcome) +
#   theme_bw() +
#   theme(axis.title.y = element_blank(),
#         axis.title.x = element_blank())
# 
# ggsave(file = here::here("output", "figures", "main_slope_CI_imrc_dlrc_GEE.png"), 
#        device = "png", width = 7, height = 5, units = "in", dpi = 300)
# 
# ##---- sens2: restricting to 2011 sample ----
# CI_forplot %>%
#   filter(analysis == "sens2", !is.na(label)) %>%
#   ggplot(aes(x = pe, y = label)) +
#   geom_point(position = position_dodge((width = 0.3)), color = "#0083C3") +
#   geom_errorbarh(aes(xmin = p2.5th, xmax = p97.5th), height = 0.1, 
#                  color = "#0083C3",
#                  position = position_dodge()) +
#   geom_vline(xintercept = 0) +
#   # scale_x_continuous(limits = c(-0.7, 3.3)) +
#   # scale_x_continuous(limits=c(-3, 2), breaks = seq(0, 2.5, 0.5)) +
#   # scale_y_discrete(limits = rev) +
#   facet_wrap(~outcome) +
#   theme_bw() +
#   theme(axis.title.y = element_blank(),
#         axis.title.x = element_blank())
# 
# ggsave(file = here::here("output", "figures", "sens2_slope_CI_imrc_dlrc_GEE.png"), 
#        device = "png", width = 7, height = 5, units = "in", dpi = 300)
# #---- warnings ----
# for (y in c("imrc", "dlrc")){
#   warnings <- tibble()
#   # for (m in 1:10){
#   for (b in 0:1000){
#     warnings <- bind_rows(
#       warnings, readRDS(here::here("data", "hoffman2", "warnings",
#                                    paste0("warnings_", y, "_", b, ".RDS"))))
#     # paste0("warnings_", y, 
#     #        "_imp_", m,"_boot_", b, ".RDS")))
#   }
#   assign(paste0(y, "_warnings"), warnings)
#   # }
# }
# rm(warnings)
# #---- warnings ----
# for (y in c("imrc", "dlrc")){
#   warnings <- tibble()
#   # for (m in 1:10){
#   for (b in 0:1000){
#     warnings <- bind_rows(
#       warnings, readRDS(here::here("data", "hoffman2", "warnings",
#                                    paste0("warnings_", y, "_", b, ".RDS"))))
#     # paste0("warnings_", y, 
#     #        "_imp_", m,"_boot_", b, ".RDS")))
#   }
#   assign(paste0(y, "_warnings"), warnings)
#   # }
# }
# rm(warnings)