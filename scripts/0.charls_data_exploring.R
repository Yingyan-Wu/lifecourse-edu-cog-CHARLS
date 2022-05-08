# CHARLS_data_exploring
# Created by: Yingyan Wu
# Some code adapted from Crystal Shaw's and Mingfei Dong's Data construction scripts
# May.08.2022

#---- Package loading + options ----
mise::mise()

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "glue", 
       "survey")

#---- function ----
source(here::here("scripts", "functions.R"))