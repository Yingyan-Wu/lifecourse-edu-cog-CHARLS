# detectable effect analysis

options(scipen = 999)
#---- Aim 1----
load(here::here("data", "analysis_data", "charls_selected.RData"))
# longitudinal mixed effect
p_load("lme4")
charls_long <- charls_selected %>%
  select(ID, continuing_edu, childhood_edu, contains("imrc"), contains("dlrc"),
         -contains("r4")) %>%
  pivot_longer(cols = !ID:childhood_edu,
               names_to = c("wave", ".value"),
               names_pattern = "r(.)(.*)") %>%
  mutate(year = case_when(wave == 1 ~ 2011,
                          wave == 2 ~ 2013,
                          wave == 3 ~ 2015,
                          wave == 4 ~ 2018))

model0_dlrc <- lmer(dlrc ~ year + (1 + year|ID), data = charls_long)
summary(model0_dlrc)
# SE of slope = 0.005
# varratio
# 2.47743926689/0.004626^2

model0_imrc <- lmer(imrc ~ year + (1 + year|ID), data = charls_long)
summary(model0_imrc)
# SE of slope = 0.004
# Varratio
# 1.792426140056/0.003936^2

model1 <- lmer(dlrc ~ as.factor(continuing_edu)*year + childhood_edu + (1 + year|ID),
               charls_long)
summary(model1)
car::Anova(model1)

model2 <- lmer(imrc ~ as.factor(continuing_edu)*year + childhood_edu + (1 + year|ID),
               charls_long)
summary(model2)
car::Anova(model2)
p_load(sjPlot)
tab_model(list(model0_dlrc, model0_imrc, model1, model2), show.ci = F)

power.long <- function(n, d, C, vare, varu, alpha){
  ncp <- d/sqrt(4*(vare/(varu*C)+1)/n)
  crit <- qt(1-alpha/2, n-2)
  power <- 1-pt(crit, n-2, ncp)
  return(power)
}
# variance of slope 
seu <- 0.005
varu <- seu^2
x <- 0:7 # Time range wave 1-4: 0-7 years
ct <- poly(x, degree=1)*sqrt(sum((x-mean(x))^2))
C <- sum(ct[1:length(x),]^2)
C

# power.long(19912, 0.000215/0.005, C, seu^2*6, seu^2, 0.05)
# power.long(19912, 0.000215/0.005, C, seu^2*7, seu^2, 0.05)
# power.long(19912, 0.000217/0.005, C, seu^2*8, seu^2, 0.05)
# power.long(19912, 0.00022/0.005, C, seu^2*9, seu^2, 0.05)
# power.long(19912, 0.000225/0.005, C, seu^2*11, seu^2, 0.05)
power.long(19912, 0.000228/0.005, C, seu^2*12, seu^2, 0.05)
# The values for beta comes from various round of running the power.long
aim1 <- tibble(
  beta = c(0.000213, 0.000215, 0.000217, 0.00022, 0.000221, 0.000225, 0.000227),
  # beta = seq(0.0002, 0.00025, by = 0.00001),
  d = beta/seu, # from a study
  varratio = 6:12,
  n = 19912,
  vare = varu*varratio,
  C = C)

aim1 %<>%
  mutate(power = power.long(n, d, C, vare, varu, 0.05))
aim1 %>% print(n = Inf)


