# 1.1 equating method for memory scores
# Created by Yingyan Wu
# Mar.14.2023
# Covariate balance and weight development code was adapted from 
# EHL's Khandle weighting code and 
# YW's sample selection code

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "gtsummary", "equate", "twang", "spatstat", "survey")

#---- Load the datasets ----
load(here::here("data", "analysis_data", 
                "charls_equating_samp.RData"))

#---- 1. Restrict to a calibration group ----
# Restrict to a “calibration group” that has a common age range, 
# and common education range across the time points. 
# This is a highly selected, “biased” sample.
summary(harmonized$r3age_y)
summary(harmonized$r4age_y)

summary(charls_selected$childhood_edu)

charls_selected <- harmonized_long_1518 %>%
  na.omit() %>%
  select(-householdID, -communityID, -pnc) %>%
  filter(age_y >= 60 & age_y < 90 & raeduc_c >= 4 & raeduc_c != 10 &
           hukou != 3 & hukou != 4) %>%
  mutate(treat = case_when(wave == "4" ~ 1,
                           wave == "3" ~ 0),
         work_status = case_when(lbrf_c %in% c(1, 2) ~ 1, # Agriculture employed
                                 lbrf_c %in% c(3, 4, 5) ~ 2, # Non-agriculture employed
                                 lbrf_c == 6 ~ 3, # Unemployed
                                 lbrf_c == 7 ~ 4, # retired
                                 lbrf_c == 8 ~ 5 # Never work
         ),
         unweight = 1) %>%
  mutate(across(c("childhood_edu", "hukou", "work_status"), factor))

#---- 2. Generate weights ----
# Generate weights, to ensure the age/education distributions are the same 
# in each study wave. This will remove the aging and education effects from the 
# equating algorithm. You can generate weights using any approach; 
# AG uses IPW but other options work.

#---- IOW ----
#---- **functions ----
checkcovbal <- function(data, weightvar){
  # data = charls_covbal
  # weightvar = "unweight"
  temp <- twang::bal.stat(data = data, 
                          vars = selected_vars,
                          w.all = data[, weightvar],
                          treat.var = "treat", # 1 = 2018
                          sampw = data$unweight, 
                          estimand = "ATT", 
                          na.action = "exclude",
                          multinom = F)
  
  temp[[1]][, "var"] <- rownames(temp[[1]])
  covbal_toplot <- do.call("rbind", temp)
  
  # Label the variables
  covbal_toplot %<>%
    filter(var %in% order_var) %>%
    arrange(desc(std.eff.sz)) %>%
    mutate(label = case_when(
      var == "age_y" ~ "Age (Years)",
      var == "female" ~ "Female",
      var == "childhood_edu:2" ~ "Vocational school/middle school/high school",
      var == "childhood_edu:3" ~ "College & above",
      var == "childhood_edu:1" ~ "Elementary school or less",
      var == "rural" ~ "Rural residence",
      var == "mstat" ~ "Marital status",
      var == "work_status:1" ~ "Agriculture employed",
      var == "work_status:2" ~ "Non-agriculture employed",
      var == "work_status:3" ~ "Unemployed",
      var == "work_status:4" ~ "Retired",
      var == "work_status:5" ~ "Never work"
    ))
  covbal_toplot %<>%
    mutate(var = factor(var, levels = rev(order_var))) %>%
    arrange(desc(var))
  covbal_toplot$label <- factor(covbal_toplot$label, 
                                levels = rev(covbal_toplot$label))
  
  # Generate data to plot covariate balance
  covbal_plot <- covbal_toplot %>%
    ggplot(aes(x = std.eff.sz, y = label)) +
    geom_point(size = 2, color = "#0072B2") +
    geom_vline(xintercept = 0) +
    xlab("Standardized mean diff Mean(Wave 3) - Mean(Wave 4)/SD(Wave 3)") +
    theme_bw() +
    geom_vline(xintercept = c(-0.25, 0.25), linetype = "dashed",
               color = "dark grey") +
    # sclae_x_continuous
    theme(aspect.ratio = 5/3, panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "none")
  
  return(list("covbal_data" = covbal_toplot, "covbal_plot" = covbal_plot))
  
}

covbal_comp_plot <- function(weighted_covbal_data, unweighted_covbal_data){
  covbal_toplot <- weighted_covbal_data %>% select("label", "std.eff.sz") %>%
    mutate(IOW = "1") %>%
    full_join(unweighted_covbal_data %>% select(label, std.eff.sz) %>%
                mutate(IOW = "0"), by = c("label", "IOW", "std.eff.sz"))
  
  plot <- covbal_toplot %>%
    ggplot(aes(x = std.eff.sz, y = label)) +
    geom_point(aes(shape = IOW), size = 2, color = "#0072B2") +
    scale_shape_manual(values = c(21, 16)) +
    geom_vline(xintercept = 0) +
    xlab("Standardized mean diff Mean(Wave 3) - Mean(Wave 4)/SD(Wave 3)") +
    theme_bw() +
    geom_vline(xintercept = c(-0.25, 0.25), linetype = "dashed",
               color = "dark grey") +
    # sclae_x_continuous
    theme(aspect.ratio = 5/3, panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10, face = "bold"),
          legend.position = "none")
  
  return(plot)
}

weight_dev <- function(dataset, pS_Z_formula, index){
  pS_Z_model <- glm(as.formula(pS_Z_formula),
                    family = binomial(link = logit),
                    weights = unweight,
                    data = dataset)
  
  dataset %<>%
    mutate(!!paste0("pS_Z", index) := predict(pS_Z_model, type = "response"),
           # W = (1-PS_Z)/PS_Z
           !!paste0("W", index) := 
             (1 - !!sym(paste0("pS_Z", index)))/!!sym(paste0("pS_Z", index)),
           # SW = W*PS/(1-PS)
           !!paste0("SW", index) := case_when(
             treat == 1 ~ !!sym(paste0("W", index))*pS/(1-pS),
             treat == 0 ~ 1),
           # SW_twang = SW*samplingwt
           !!paste0("SW", index, "_twang") := 
             !!sym(paste0("SW", index))*unweight)
  
  # Truncation pending
  
  return(dataset %>%
           select(paste0("pS_Z", index), paste0("W", index),
                  paste0("SW", index), paste0("SW", index, "_twang")))
  # paste0("SW", index, "_twang_trunc99"),
  # paste0("pS_Z", index, "_trunc99")))
}

#---- **setup ----
charls_covbal <- charls_selected %>%
  remove_labels() %>%
  remove_val_labels() %>%
  as.data.frame()

selected_vars <- colnames(charls_covbal %>% 
                            select(female, childhood_edu, rural, mstat,
                                   work_status, age_y))
order_var <- c("age_y", "rural", "female", "mstat", 
               "childhood_edu:2", "childhood_edu:3", "childhood_edu:1", 
               "work_status:1", "work_status:2", "work_status:3", 
               "work_status:4", "work_status:5")

#---- **Unweighted ----
unw_test <- checkcovbal(charls_covbal, "unweight")
unw_test$covbal_plot

#---- **pS ----
pS <- sum(charls_covbal$treat)/sum(charls_covbal$unweight)
# P(wave 2018): we are not using the survey weight, but still using the unweight
# variable to make it easier to switch to survey weights

#---- **Weight 1: education + age ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, "treat ~ childhood_edu + age_y", "1"))

# Is this okay? two peaks
charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z1, linetype = factor(treat), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2018")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w1_test <- checkcovbal(charls_covbal, "SW1_twang")
covbal_comp_plot(w1_test$covbal_data, unw_test$covbal_data)

#---- **Weight 2: education + age + work ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, "treat ~ childhood_edu + age_y + work_status", "2"))

# Is this okay? two peaks
charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z2, linetype = factor(treat), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2018")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w2_test <- checkcovbal(charls_covbal, "SW2_twang")
covbal_comp_plot(w2_test$covbal_data, unw_test$covbal_data)

#---- **Weight 3: education + age + work + female ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, 
             "treat ~ childhood_edu + age_y + work_status + female", "3"))

# Is this okay? two peaks
charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z3, linetype = factor(treat), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2018")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w3_test <- checkcovbal(charls_covbal, "SW3_twang")
covbal_comp_plot(w3_test$covbal_data, unw_test$covbal_data)

# #---- IPW: remove the covariates' effect ----
# charls_selected %<>%
#   mutate(
#     # Estimate P(A)
#     p_imrc = predict(glm(imrc_cleaned ~ 1, family = gaussian(link = "identity"), 
#                          data = charls_selected), type = "response"),
#     p_dlrc = predict(glm(dlrc_cleaned ~ 1, family = gaussian(link = "identity"), 
#                          data = charls_selected), type = "response"),
#     # Estimate P(A|W1, W2)
#     p_imrc_cov3 = predict(glm(imrc_cleaned ~ 
#                                 childhood_edu + age_y + work_status + female, 
#                               family = gaussian(link = "identity"), 
#                               data = charls_selected), type = "response"),
#     p_dlrc_cov3 = predict(glm(dlrc_cleaned ~ 
#                                 childhood_edu + age_y + work_status + female, 
#                               family = gaussian(link = "identity"), 
#                               data = charls_selected), type = "response"),
#     # Get the IPTW for ATE, ATU and ATT
#     iptw3_imrc = case_when(A == 1 ~ pA/pA_W1W2,
#                          A == 0 ~ (1 - pA)/(1 - pA_W1W2)),
#     iptw3_dlrc = case_when(A == 1 ~ pA/pA_W1W2,
#                           A == 0 ~ (1 - pA)/(1 - pA_W1W2)))


#----2a. Check distribution for weighted 2018 and 2015 ----
# Unweighted 2015
r3freqim_IOW <- freqtab(charls_covbal[charls_covbal$wave == "3", "imrc_cleaned"])
r3freqdl_IOW <- freqtab(charls_covbal[charls_covbal$wave == "3", "dlrc_cleaned"])
# Weighted 2018
r4freqim_IOW <- charls_covbal %>%
  filter(wave == "4") %>%
  dplyr::count(imrc_cleaned, wt = SW3_twang) %>%
  as.freqtab()

r4freqdl_IOW <- charls_covbal %>%
  filter(wave == "4") %>%
  dplyr::count(dlrc_cleaned, wt = SW3_twang) %>%
  as.freqtab()

#---- 3. Perform the equating ----
#---- **Immediate recall ----
eqim_linear_IOW <- equate(r4freqim_IOW, r3freqim_IOW, type = "l")
eqim_mean_IOW <- equate(r4freqim_IOW, r3freqim_IOW, type = "mean")
eqim_eqp_IOW <- equate(r4freqim_IOW, r3freqim_IOW, type = "equipercentile")

eqim_linear_IOW
eqim_mean_IOW
eqim_eqp_IOW$concordance

plot(eqim_eqp_IOW, eqim_mean_IOW, eqim_linear_IOW)

#---- **Delayed recall ----
eqdl_linear_IOW <- equate(r4freqdl_IOW, r3freqdl_IOW, type = "l")
eqdl_mean_IOW <- equate(r4freqdl_IOW, r3freqdl_IOW, type = "mean")
eqdl_eqp_IOW <- equate(r4freqdl_IOW, r3freqdl_IOW, type = "equipercentile")

eqdl_linear_IOW
eqdl_mean_IOW
eqdl_eqp_IOW$concordance

plot(eqdl_eqp_IOW, eqdl_mean_IOW, eqdl_linear_IOW)

#---- 4. Apply the equipercentile equating algorithm to the full sample in the focal group -----
harmonized %<>%
  left_join(eqim_eqp_IOW$concordance %>% 
              dplyr::rename(r4imrc_cleaned = scale, r4imrc_eqt = yx),
            by = "r4imrc_cleaned") %>%
  left_join(eqdl_eqp_IOW$concordance %>% 
              dplyr::rename(r4dlrc_cleaned = scale, r4dlrc_eqt = yx),
            by = "r4dlrc_cleaned")

with(harmonized, table(r4imrc_cleaned, r4imrc_eqt, useNA = "ifany"))
with(harmonized, table(r4dlrc_cleaned, r4dlrc_eqt, useNA = "ifany"))

#---- ** Empirical plot ----
harmonized %<>%
  mutate(imrc_balanced = case_when(!is.na(r1imrc) & !is.na(r2imrc) & 
                                     !is.na(r3imrc_cleaned) & 
                                     !is.na(r4imrc_cleaned) ~ 1,
                                   TRUE ~ 0),
         dlrc_balanced = case_when(!is.na(r1dlrc) & !is.na(r2dlrc) & 
                                     !is.na(r3dlrc_cleaned) & 
                                     !is.na(r4dlrc_cleaned) ~ 1,
                                   TRUE ~ 0))
with(harmonized, table(imrc_balanced, useNA = "ifany"))
with(harmonized, table(dlrc_balanced, useNA = "ifany"))

harmonized_long <- harmonized %>%
  select(ID, contains("imrc"), contains("dlrc"), 
         -c("r3dlrc", "r4dlrc", "r3imrc", "r4imrc"),
         imrc_balanced, dlrc_balanced) %>%
  dplyr::rename_with(.cols = c(paste0("r", c(1, 2), "dlrc"),
                               paste0("r", c(1, 2), "imrc")), 
                     function(x){paste0(x, "_cleaned")}) %>%
  pivot_longer(cols = -c(ID, imrc_balanced, dlrc_balanced),
               names_to = c("wave", ".value", "type"),
               names_pattern = "r(\\d+)(.*)_(.*)") %>%
  print()

harmonized_empirical_balanced <- harmonized_long %>%
  filter(imrc_balanced == 1) %>%
  group_by(wave, type) %>%
  summarise_at(vars(imrc), 
               funs(n(), mean, sd)) %>%
  mutate(se = sd/sqrt(n),
         variable = paste0("Immediate word recall\n(n = ", n, ")")) %>%
  rbind(harmonized_long %>%
          filter(dlrc_balanced == 1) %>%
          group_by(wave, type) %>%
          summarise_at(vars(dlrc), 
                       funs(n(), mean, sd)) %>%
          mutate(se = sd/sqrt(n),
                 variable = paste0("Delayed word recall\n(n = ", n, ")"))) 

harmonized_empirical_balanced %<>%
  rbind(harmonized_empirical_balanced %>% 
          filter(type == "cleaned" & wave != 4) %>%
          mutate(type = "eqt")) %>%
  mutate(type = factor(type, levels = c("eqt", "cleaned"))) %>%
  arrange(type, wave) %>%
  print()

harmonized_empirical_balanced %>%
  ggplot(aes(x = wave, y = mean,
             group = interaction(variable, type),
             color = interaction(variable, type))) +
  geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se), width=.1) +
  geom_line(aes(linetype = interaction(variable, type))) +
  geom_point() +
  scale_color_manual(values = c("salmon", "cyan3", "salmon", "cyan3"),
                     labels=c("Equated w4 dlrc", "Equated w4 imrc",
                              "Original scores","Original scores")) +
  scale_linetype_manual(values = c(2, 2, 1, 1)) +
  theme_classic() +
  guides(linetype = FALSE) +
  labs(x = "Wave",
       y = "Word recall",
       title = "Average word recall score +/- 2 SE at different wave
       balanced data",
       color = "Variable")

#---- Save the dataset ----
r4rc_eqt_tib <- harmonized %>%
  select(ID, r4imrc_eqt, r4dlrc_eqt)
save(r4rc_eqt_tib, file = here::here("data", "analysis_data", "eqt_r4_rc.RData"))

#---- OLD ----
#---- **Table 1 ----
table1_1518 <- charls_selected %>%
  select(age_y, female, raeduc_c, childhood_edu, rural,
         hukou, imrc_cleaned, dlrc_cleaned, wave) %>%
  labelled::set_variable_labels(
    age_y = "Age at waves",
    female = "Female (%)",
    raeduc_c = "Early life education level (%)",
    childhood_edu = "Early life education level (simplified) (%)",
    rural = "Rural residence (%)",
    hukou = "Hukou status (%)",
    imrc_cleaned = "Immediate word recall", 
    dlrc_cleaned = "Delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    rural = c("Yes" = 1, "No" = 0),
    childhood_edu = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3),
    wave = c("2015" = "3", "2018" = "4")) %>%
  modify_if(is.labelled, to_factor) %>%
  modify_if(is.factor,
            function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ c("{mean} ({sd})",
                                          "({min}, {max})")),
    type = all_continuous() ~ "continuous2",
    by = wave,
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing") %>%
  add_p() %>%
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )

table1_1518 %>%  as_flex_table()
# flextable::save_as_docx(path = here::here("output", "tables",
#                                           "table1_equating_calibrate_sample_15_18.docx"))

