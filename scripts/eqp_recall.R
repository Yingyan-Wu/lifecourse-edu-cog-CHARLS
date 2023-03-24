
load("/Users/yuanzhang/Downloads/charls_equating_samp.RData")

# Load the equate package
library(equate)

### Immediate recall score
r3freqim <- freqtab(harmonized$r3imrc_cleaned)
r4freqim <- freqtab(harmonized$r4imrc_cleaned)

summary(harmonized$r3imrc)
summary(harmonized$r4imrc)

# Perform the equating
eqim_linear <- equate(r4freqim, r3freqim, type = "l")
eqim_mean <- equate(r4freqim, r3freqim, type = "mean")
eqim_eqp <- equate(r4freqim, r3freqim, type = "equipercentile")

eqim_linear
eqim_mean
eqim_eqp$concordance

plot(eqim_eqp, eqim_mean, eqim_linear)

library(tidyverse)
harmonized <- harmonized %>%
  mutate(r4imrc_rev = case_when(
    r4imrc == 0 ~ 0.514,
    r4imrc == 1 ~ 2.268,
    r4imrc == 2 ~ 2.916,
    r4imrc == 3 ~ 3.759,
    r4imrc == 4 ~ 4.637,
    r4imrc == 5 ~ 5.478,
    r4imrc == 6 ~ 6.396,
    r4imrc == 7 ~ 7.330,
    r4imrc == 8 ~ 8.295,
    r4imrc == 9 ~ 9.318,
    r4imrc == 10 ~ 10
  ))

#filter the data to only include complete obs for r*imrc
comp_imrc <- harmonized %>% 
  filter(!is.na(r1imrc) & !is.na(r2imrc) & !is.na(r3imrc) & !is.na(r4imrc))

mean_r1imrc <- mean(comp_imrc$r1imrc)
mean_r2imrc <- mean(comp_imrc$r2imrc)
mean_r3imrc <- mean(comp_imrc$r3imrc)
mean_r4imrc <- mean(comp_imrc$r4imrc)
mean_r4imrc_rev <- mean(comp_imrc$r4imrc_rev)
cat("Mean 2011:", mean_r1imrc, "\nMean 2013:", mean_r2imrc, 
    "\nMean 2015:", mean_r3imrc, "\nMean 2018(raw):", mean_r4imrc, 
    "\nMean 2018(equiv):", mean_r4imrc_rev)

###############################################################################

### Delayed recall score
r3freqdl <- freqtab(harmonized$r3dlrc)
r4freqdl <- freqtab(harmonized$r4dlrc)

summary(harmonized$r3dlrc)
summary(harmonized$r4dlrc)

# Perform the equating
eqdl_linear <- equate(r4freqdl, r3freqdl, type = "l")
eqdl_mean <- equate(r4freqdl, r3freqdl, type = "mean")
eqdl_eqp <- equate(r4freqdl, r3freqdl, type = "equipercentile")

eqdl_linear
eqdl_mean
eqdl_eqp$concordance

plot(eqdl_eqp, eqdl_mean, eqdl_linear)

library(tidyverse)
harmonized <- harmonized %>%
  mutate(r4dlrc_rev = case_when(
    r4dlrc == 0 ~ 0,
    r4dlrc == 1 ~ 0.397,
    r4dlrc == 2 ~ 0.865,
    r4dlrc == 3 ~ 1.768,
    r4dlrc == 4 ~ 2.530,
    r4dlrc == 5 ~ 3.208,
    r4dlrc == 6 ~ 3.953,
    r4dlrc == 7 ~ 4.714,
    r4dlrc == 8 ~ 5.493,
    r4dlrc == 9 ~ 6.468,
    r4dlrc == 10 ~ 7.894 ))


#filter the data to only include complete obs for r*dlrc
comp_dlrc <- harmonized %>% 
  filter(!is.na(r1dlrc) & !is.na(r2dlrc) & !is.na(r3dlrc) & !is.na(r4dlrc))

mean_r1dlrc <- mean(comp_dlrc$r1dlrc)
mean_r2dlrc <- mean(comp_dlrc$r2dlrc)
mean_r3dlrc <- mean(comp_dlrc$r3dlrc)
mean_r4dlrc <- mean(comp_dlrc$r4dlrc)
mean_r4dlrc_rev <- mean(comp_dlrc$r4dlrc_rev)

cat("Mean 2011:", mean_r1dlrc, "\nMean 2013:", mean_r2dlrc, 
    "\nMean 2015:", mean_r3dlrc, "\nMean 2018(raw):", mean_r4dlrc, "\nMean 2018(equiv):", mean_r4dlrc_rev)
