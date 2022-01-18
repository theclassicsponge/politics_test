### Bivariate Linear Regression Models ###

### textreg ###
install.packages("texreg") # install only once
library(texreg) # load in the beginning of every R session
library(tidyverse) # also load tidyverse!
# US census data 
# exploring the relationship between unemployment rate a low educaton
dat <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/communities.csv")
# first rename varibles to be clearer
# view all names
names(dat) # 'PctUnemployed' and 'PctNotHSGrad'
# rename
dat <- dat %>%
  rename(
    UnemploymentRate = PctUnemployed,
    NoHighSchool = PctNotHSGrad
  )
# summarize the variables, boths masured as proportions
summary(dat$UnemploymentRate)
summary(dat$NoHighSchool)

# will be easier to interpre tohe regression output if we convert these to percentages rather than proportions 
dat <- dat %>%
  mutate(
    UnemploymentRate = UnemploymentRate*100,
    NoHightSchool = NoHightSchool*100
  )
  