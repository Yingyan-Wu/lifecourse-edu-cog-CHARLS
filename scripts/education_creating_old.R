#---- Waves of Edu ----
#---- ** Wave 1 ----
# n = 17705
# Highest edu level
summarytools::freq(charls_w1$w1_bd001)
# Highest grade finished in primary school
summarytools::freq(charls_w1$w1_bd002)
with(charls_w1, table(w1_bd001, w1_bd002, useNA = "ifany"))
# We could use bd002 for those who filled in bd001 as 1-3 (no formal edu to Sishu)
# For 30 people fill in did not finish primary school, they have missing data for highest grade finished in primary school

# Age of formal schooling begins
proc_univariate(charls_w1$w1_bd005)

# Age finished schooling
proc_univariate(charls_w1$w1_bd006)

with(charls_w1 %>% filter(w1_bd006 > 45), table(w1_bd006, w1_bd005, useNA = "ifany"))
view(charls_w1 %>% filter(w1_bd005 < 20 & w1_bd006 > 45) %>% select(ID, w1_ba002_1,
                                                                    contains("bd00")))
#formal schooling ending: contains adult education

# Adult education
with(charls %>% filter(ID %in% charls_w1$ID), summarytools::freq(w1_bd007))
proc_univariate(charls_w1$w1_bd008)
view(charls_w1 %>% filter(w1_bd008 > 4) %>% select(ID, w1_ba002_1,
                                                   contains("bd00"), 
                                                   contains("bd01"))) 

#---- ** Wave 2 ----
# n = 18605
# Highest edu level
charls_w2 %<>%
  mutate(w2_bd001_clean = case_when(w2_bd001_w2_4 == 12 ~ w2_bd001_w2_3,
                                    !is.na(w2_bd001_w2_4) ~ w2_bd001_w2_4,
                                    !is.na(w2_bd001_w2_3) ~ w2_bd001_w2_3,
                                    !is.na(w2_bd001) ~ w2_bd001))
with(charls_w2, table(w2_bd001, w2_bd001_w2_4, useNA = "ifany"))
with(charls_w2, table(w2_bd001_w2_3, w2_bd001_w2_4, useNA = "ifany"))
with(charls_w2, table(w2_bd001_w2_3, w2_bd001_w2_4, useNA = "ifany"))
with(charls_w2 %>% filter(w2_bd001_w2_4 != 12 & is.na(w2_bd001_w2_4)), 
     table(w2_bd001, w2_bd001_w2_3, useNA = "ifany"))

with(charls_w2, table(w2_bd001, w2_bd001_clean, useNA = "ifany"))

summarytools::freq(charls_w2$w2_bd001_clean)
# nmissing = 9118

# Highest grade finished in primary school
summarytools::freq(charls_w2$w2_bd002)
with(charls_w2, table(w2_bd001_clean, w2_bd002, useNA = "ifany"))
# We could use bd002 for those who filled in bd001 as 1-3 (no formal edu to Sishu)
# For 30 people fill in did not finish primary school, they have missing data for highest grade finished in primary school

# Age of formal schooling begins
proc_univariate(charls_w2$w2_bd005)

# Age finished schooling
proc_univariate(charls_w2$w2_bd006)

with(charls_w2 %>% filter(w2_bd006 > 45), table(w2_bd006, w2_bd005, useNA = "ifany"))
view(charls_w2 %>% filter(w2_bd005 < 20 & w2_bd006 > 45) %>% select(ID, w2_ba002_1,
                                                                    contains("bd00")))
#formal schooling ending: contains adult education

#---- Adult education age distribution ----
summarytools::freq(charls$adult_edu)

#---- ** wave 1 ----
proc_univariate(charls_w1$w1_bd008)
proc_univariate(charls_w1$w1_bd010)

with(charls_w1, table(w1_bd011, useNA = "ifany"))

charls_w1 %<>% mutate(w1_age_finish_adltedu = w1_bd010 - w1_ba002_1)

proc_univariate(charls_w1$w1_age_finish_adltedu)

view(charls_w1 %>% filter(w1_age_finish_adltedu < 20 & 
                            w1_age_finish_adltedu != w1_bd006) %>%
       select(ID, w1_ba002_1, contains("bd00"), w1_age_finish_adltedu,
              contains("bd01")))

#---- **working ----
charls_w1 %<>% mutate(w1_age_work = 
                        case_when(w1_fb001 == 1 ~ w1_fb001_1,
                                  w1_fb001 == 2 ~ w1_fb001_2 - w1_ba002_1))
# The reason of stop working
proc_freq(charls_w1, w1_fb010)
# 24 of them stopped 
# Age start working * work unit at first job
with(charls_w1, table(w1_age_work, w1_fb002, useNA = "ifany"))

with(charls_w1 %>% filter(w1_age_work < 20), 
     table(w1_age_work, w1_fb004, useNA = "ifany"))

#---- ** wave 2 ----
proc_univariate(charls_w2$w2_bd008)
proc_univariate(charls_w2$w2_bd010)

#---- Education + work trajectories ----
charls %<>%
  mutate(age_edu_start = case_when(!is.na(w2_bd005) ~ w2_bd005,
                                   !is.na(w1_bd005) ~ w1_bd005),
         age_edu_finish = case_when(!is.na(w4_bd006) ~ w4_bd006,
                                    !is.na(w3_bd006) ~ w3_bd006,
                                    !is.na(w2_bd006) ~ w2_bd006,
                                    !is.na(w1_bd006) ~ w1_bd006))

with(charls, table(age_edu_start, w1_bd005, useNA = "ifany"))
summarytools::freq(charls$age_edu_start) # missing 9999 people

with(charls, table(age_edu_start, edu_highest, useNA = "ifany"))
with(charls, table(age_edu_finish, edu_highest, useNA = "ifany"))
with(charls, proc_sum(age_edu_finish - age_edu_start, more = T))
