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
    NoHighSchool = NoHighSchool*100
  )

# we can begin by drawing a scatterplot with the percentage of unemployed people an y and percentage of adult without highschool on x

p1 <- ggplot(dat, aes(y = UnemploymentRate, x = NoHighSchool)) + # basic plot instructions
  geom_point( # make it a scatterplot
    color = "chartreuse3", # make the points 'chartreuse'
    alpha = 0.5, # make the points semi-transparent
    shape = 19) + # make the points circles
  labs(x =  "Adults without High School education (%)", y = "Unemployment (%)") + # axis labels
  theme_bw() # simplistic theme

p1

# what is the assosciation between unemploymet rate and lack of high school education?

# to answer that question formally we will run linear regressiion using lm()

model1 <- lm(UnemploymentRate ~ NoHighSchool, #formula
             data = dat) # dataset
summary(model1)

# Now let's add a regression line to the scatter plot using the geom_smooth function.

# add the geom_smooth() funciton to add the regression line
p1 + geom_smooth(method = "lm", # line should be fit as a linear model
                 color = "hotpink4", 
                 se = FALSE) # hide the confidence intervals
