# Test weights arugument in geepack package
# Yingyan Wu

#---- Package loading + preparation ----
#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("tidyverse", "magrittr", "survey", "geepack")

select <- dplyr::select
#---- fake data ----
test_data <- readRDS(here::here("data", "analysis_data", "imp_pmm_10_stacked.RDS")) %>%
  filter(imp == 1) %>%
  select(ID, contains("agey_v3"), female,
         paste0("r", 1:2, "imrc"),
         paste0("r", 3, "imrc_cleaned"), paste0("r", 4:5, "imrc_eqt")) %>%
  dplyr::rename_at(vars(contains(c("imrc", "dlrc"))),
                   function(x) str_replace(x, "_eqt|_cleaned", "")) %>%
  slice_head(n = 100) %>%
  mutate(weight = rep(1:4, 25))

test_data_long <- test_data %>%
  pivot_longer(cols = !c(ID, female, weight), 
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
  arrange(ID, female, weight, agey_v3, imrc)
 
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
GEE_stacked <- geepack::geese(imrc ~ agey_v3 + female, 
                          id = ID, 
                          data = stacked_test_data_long, 
                          waves = agey_v3, 
                          family = gaussian)

wtd_GEE <- geepack::geese(imrc ~ agey_v3 + female, 
                          id = ID, 
                          data = test_data_long, 
                          waves = agey_v3, 
                          weights = weight,
                          family = gaussian)

unwtd_GEE <- geepack::geese(imrc ~ agey_v3 + female, 
                            id = ID, 
                            data = test_data_long, 
                            waves = agey_v3, 
                            family = gaussian)

GEE_stacked$beta
wtd_GEE$beta
unwtd_GEE$beta


GEEglm_stacked <- geepack::geeglm(imrc ~ agey_v3 + female, 
                              id = ID, 
                              data = stacked_test_data_long, 
                              waves = agey_v3, 
                              family = gaussian)

wtd_GEEglm <- geepack::geeglm(imrc ~ agey_v3 + female, 
                          id = ID, 
                          data = test_data_long, 
                          waves = agey_v3, 
                          weights = weight,
                          family = gaussian)
GEEglm_stacked$coefficients
wtdglm_GEE$coefficients

# The weight argument is doing what we want it to do. -- as survey weight

