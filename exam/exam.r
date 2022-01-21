install.packages("texreg") # install only once
library(texreg) # load in the beginning of every R session
library(tidyverse) # also load tidyverse!
library(ggplot2)

# Dataset
blm_dat <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/BLM_exam.csv")

# Part 2: Predicting the Black Live Matters protests

# Descriptive statistics

# table of regression output

### model 1 ###
# dependent variable = tot.protests 
# explanatory variable = deaths_black_pc



# summary of varibles 
summary(blm_dat$tot.protests)
summary(blm_dat$deaths_black_pc)




# scatterplot 
p1 <- ggplot(blm_dat, aes(y = tot.protests, x = deaths_black_pc)) + # basic plot instructions
  geom_point( # make it a scatterplot
    color = "red", # make the points 'chartreuse'
    alpha = 0.5, # make the points semi-transparent
    shape = 19) + # make the points circles
  labs(x =  "Number of police caused deaths of black people", y = "total number of protests") + # axis labels
  theme_bw() # simplistic theme

p1

# regression

model1 <- lm(tot.protests ~ deaths_black_pc, # formula
             data = blm_dat) # dataset
summary(model1)


# add regression line

# add the geom_smooth() funciton to add the regression line
p1 + geom_smooth(method = "lm", # line should be fit as a linear model
                 color = "hotpink4", 
                 se = FALSE) # hide the confidence intervals

# present finding in an easy to read  table
screenreg(model1)

### model 2 ###

summary(blm_dat)

# dependent variable = tot.protests 
# explanatory variable = deaths_black_pc, crime, Per_Black, TotalPop, mayorrep, blackmayor , dem_share

# model with more explanatory variables
multi_model_1 <- lm(tot.protests ~ 
                      deaths_black_pc + crime + Per_Black + TotalPop + mayorrep + blackmayor   + dem_share,
                    data = blm_dat)

summary(multi_model_1)

screenreg(list(model1, multi_model_1))
