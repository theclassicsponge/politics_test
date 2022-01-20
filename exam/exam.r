install.packages("texreg") # install only once
library(texreg) # load in the beginning of every R session
library(tidyverse) # also load tidyverse!

# Dataset
blm_dat <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/BLM_exam.csv")

# Question 1: 

# Question 2:
Turnout_reg <- lm(Per_Black ~ gov_new + educexp_gov + postfinsc_gini + effpar_ele, data = blm_dat)

screenreg(Turnout_reg)
