# Test weights arugument in geepack package
# Yingyan Wu

#---- Package loading + preparation ----
#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("tidyverse", "magrittr", "survey", "geepack", "WeMix")

select <- dplyr::select
#---- fake data ----
test_data <- readRDS(here::here("data", "analysis_data", "imp_pmm_10_stacked.RDS")) %>%
  filter(imp == 1) %>%
  select(ID, contains("agey_v3"), female, childhood_edu, 
         paste0("r", 1:2, "imrc"),
         paste0("r", 3, "imrc_cleaned"), paste0("r", 4:5, "imrc_eqt")) %>%
  dplyr::rename_at(vars(contains(c("imrc", "dlrc"))),
                   function(x) str_replace(x, "_eqt|_cleaned", "")) %>%
  slice_head(n = 100) %>%
  mutate(weight = rep(1:10, 10),
         childhood_edu = case_when(childhood_edu == 0 ~ 0,
                                   childhood_edu %in% c(1, 2, 3) ~ 1),
         ID = as.numeric(ID))

table(test_data$childhood_edu, useNA = "ifany")
class(test_data$ID)

test_data_long <- test_data %>%
  pivot_longer(cols = !c(ID, female, weight, childhood_edu), 
               names_to = c("wave", ".value"), 
               names_pattern = "r(.)(.*)") %>%
  mutate(wave = as.numeric(wave)) %>%
  na.omit()

length(unique(test_data_long$ID))

stacked_test_data <- 
  test_data %>%
  slice(rep(seq_len(n()), weight))

stacked_test_data_long <-
  test_data_long %>%
  slice(rep(seq_len(n()), weight)) %>%
  arrange(ID, female, weight, childhood_edu, agey_v3, imrc)

# stacked_long_2 <- stacked_test_data %>%
#   pivot_longer(cols = !c(ID, weight, female),
#                names_to = c("wave", ".value"),
#                names_pattern = "r(.)(.*)") %>%
#   mutate(wave = as.numeric(wave)) %>%
#   na.omit() %>%
#   arrange(ID, female, weight, agey_v3, imrc)
# 
# diffdf::diffdf(stacked_test_data_long, stacked_long_2) # the same

#---- GEE model ----
GEE_stacked <- geepack::geese(imrc ~ agey_v3 + female + childhood_edu, 
                              id = ID, 
                              data = stacked_test_data_long, 
                              waves = agey_v3, 
                              family = gaussian)

wtd_GEE <- geepack::geese(imrc ~ agey_v3 + female + childhood_edu, 
                          id = ID, 
                          data = test_data_long, 
                          waves = agey_v3, 
                          weights = weight,
                          family = gaussian)

unwtd_GEE <- geepack::geese(imrc ~ agey_v3 + female + childhood_edu, 
                            id = ID, 
                            data = test_data_long, 
                            waves = agey_v3, 
                            family = gaussian)

GEE_stacked$beta
wtd_GEE$beta
unwtd_GEE$beta

GEEglm_stacked <- geepack::geeglm(imrc ~ agey_v3 + female + childhood_edu, 
                                  id = ID, 
                                  data = stacked_test_data_long, 
                                  waves = agey_v3, 
                                  family = gaussian)

wtd_GEEglm <- geepack::geeglm(imrc ~ agey_v3 + female + childhood_edu, 
                              id = ID, 
                              data = test_data_long, 
                              waves = agey_v3, 
                              weights = weight,
                              family = gaussian)
GEEglm_stacked$coefficients
wtd_GEEglm$coefficients

# The weight argument is doing what we want it to do. -- as survey weight

#---- Test MSM coefficients GEE vs. wtd LMM ----
# 1. get proportion of childhood_edu
test_data_svy <- svydesign(ids = ~ID, weights = ~weight, 
                           data = test_data)
px0_model <- svyglm(childhood_edu ~ 1, 
                    design = test_data_svy, 
                    family = quasibinomial("identity"))

# 2. Q model
GEEglm_qmodel <- geepack::geeglm(imrc ~ wave + female + childhood_edu * wave, 
                                 id = ID, 
                                 data = test_data_long, 
                                 waves = wave, 
                                 weights = weight,
                                 family = gaussian)
GEEglm_qmodel$coefficients

test_data_long %<>%
  mutate(w_obs = 1,
         weight = as.numeric(weight))
Wemix_qmodel <- WeMix::mix(imrc ~ wave + (1 + wave |ID) + female + 
                             childhood_edu * wave,
                           data = test_data_long, 
                           weights = c("w_obs", "weight"),
                           cWeights = TRUE)
Wemix_qmodel$coef

# 3. intervention variables and potential outcomes
test_data %<>%
  mutate(doX0 = rbinom(100, size = 1, prob = 
                         predict(px0_model, newdata = test_data, type = "response")))

test_data_long %<>%
  left_join(test_data %>% select(ID, doX0), by = "ID")

test_data_long %<>%
  mutate(Y_GEE = predict(GEEglm_qmodel, newdata = test_data_long %>% 
                           select(-childhood_edu) %>%
                           dplyr::rename(childhood_edu = doX0), type = "response"))
# # Not working for some reason
# Y_LMM = predict(Wemix_qmodel,
#                 newdata = test_data_long %>%
#                   select(-childhood_edu) %>%
#                   dplyr::rename(childhood_edu = doX0), type = "response")

pred_mat <- test_data_long %>% 
  select(-childhood_edu) %>%
  dplyr::rename(childhood_edu = doX0) %>%
  model.matrix(as.formula("imrc ~ wave + (1 + wave |ID) + female + 
                 childhood_edu * wave"),
               .) %>%
  as.data.frame() %>%
  select(-contains("IDTRUE"))

test_data_long$Y_LMM_fixed <- as.matrix(pred_mat) %*% as.matrix(coef(Wemix_qmodel)) %>%
  as.numeric()

test_data_long %<>%
  left_join(as.data.frame(Wemix_qmodel$ranefMat) %>% 
              mutate(ID = as.numeric(rownames(.))),
            by = "ID") 

test_data_long %<>%
  mutate(Y_LMM = Y_LMM_fixed + `ID..Intercept.` + `ID.wave`*wave)

test_data_long %<>%
   mutate(Y_GEE_rd = round(Y_GEE, 2),
          Y_LMM_rd = round(Y_LMM, 2))

# 4. MSM
GEE_MSM <- geepack::geeglm(Y_GEE_rd ~ wave * doX0, 
                          id = ID, 
                          data = test_data_long, 
                          waves = wave, 
                          weights = weight,
                          family = gaussian)
summary(GEE_MSM)
GEE_MSM$coefficients

LMM_MSM <- WeMix::mix(Y_LMM_rd ~ wave + (1 + wave |ID) + doX0*wave,
                      data = test_data_long, 
                      weights = c("w_obs", "weight"),
                      cWeights = TRUE)
LMM_MSM$coef
summary(LMM_MSM)
