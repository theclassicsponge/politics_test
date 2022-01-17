# Studying relationships between two categorical variables or two continuous variables


# Cross tabulations = contingency tables

### Seminar ###

# T-tests are useful to study the relationship between a continuous variable and a categorical variable

### Producing cross tabulations ###

# also called contingency tables, a useful way of exploring relationships between two categorical varibles that do not have too many levels or categories.
library(tidyverse)
install.packages("janitor", repos = 'http://cran.us.r-project.org')
library(janitor)
# Produce a cross tabulation of victimization, a categorical unordered variable, by whethere the rubbish in the street is percived to be a problem.
# read the data in the csv file into an object called BCS0708
bcs <- read.csv("https://raw.githubusercontent.com/QMUL-SPIR/Public_files/master/datasets/BCS0708.csv")
# frequiencies of the two varibles
table(bcs$rubbcomm)
# frequiencies of victimisation
table(bcs$bcsvictim)
# making presentation changes to make it easier to read
bcs <- bcs %>%
  mutate(rubbcomm = ordered(
    fct_relevel(rubbcomm, # fct_relevel() from 'forcats' in tidyverse
                "not at all common", 
                "not very common", 
                "fairly common", 
                "very common"))) # reorder levels
# improve how we label the response categories 
bcs <- bcs %>%
  mutate(rubbcomm = fct_recode(rubbcomm, # fct_recode() from 'forcats' in tidyverse
                               "Not at all" = "not at all common", 
                               "Not very" = "not very common", 
                               "Fairly" = "fairly common", 
                               "Very" = "very common")) # rename levels
# tables with the changes 
table(bcs$rubbcomm)

# present to proportions in each category
rubbish.table <- table(bcs$rubbcomm)

prop.table(rubbish.table) # shows the proportions in each response category
# With "tabyl()" we can make nicer tables with both proportions and frequiencies, in tidyverse style code
t1 <- bcs %>%
  tabyl(rubbcomm)

t1

# Now we can produce the cross tabulation of victimisation by wheter the presence of rubbish in the streets is a problem. 

# "tabyl()" is designed with minimul assumptions so we have to tell it exactly how we want our table to look.
# we add "adorn" functions for each thing we want in the table.
# add frequiencies in each cell, plus the percentage of the row in that cell, rounded to two decimal places plus row totals
t2 <- bcs %>%
  tabyl(rubbcomm, # rows of the table
        bcsvictim) %>% # columns of the table
  adorn_totals("row") %>% # show row totals
  adorn_percentages("row") %>% # show percentages of each row in each cell
  adorn_pct_formatting(digits = 2) %>% # round these to 2 decimal places
  adorn_ns() # show frquencies



t2

# 63 people say that rubbish is very common and were victims of crime which represents 30.88%
# the people who say rubbish is very common.
# you are only intrested in the percentages or proportions to make meaningful comparisons

# Read Chapter 7 on dependent and independent variables
# if you switch rows and columns we use column percentages here instead.
t3 <- bcs %>%
  tabyl(bcsvictim, # rows of the table
        rubbcomm) %>% # columns of the table
  adorn_totals(c("row", "col")) %>% # show row totals
  adorn_percentages("col") %>% # show percentages of each row in each cell
  adorn_pct_formatting(digits = 2) %>% # round these to 2 decimal places
  adorn_ns() # show frquencies



t3

# marginal frequiencies appear along the right and the bottom. Row margins show the total number of cases in each row:
# 204 people living where rubbish is very common, 1244 people living where rubbish is fairly common
# column marginals indicate the total number of cases in each column: 
# 9318 non-victims where crime is not very common, and 2358 victims
# this includes N/As in the "rubbcomm" variable, this can be changes in "show_na = FALSE" in "tabyl()".
t4 <- bcs %>%
  tabyl(bcsvictim, # rows of the table
        rubbcomm, # columns of the table
        show_na = FALSE) %>% # remove NAs from count
  adorn_totals(c("row", "col")) %>% # show row and column totals
  adorn_percentages("col") %>% # show percentages of each column in each cell
  adorn_pct_formatting(digits = 2) %>% # round these to 2 decimal places
  adorn_ns() # show frquencies



t4

### Expected Frequencies and Chi-square, two categorical variables ###

# So far we are only describing out sample can we infer that these differences that we observe can be generalised to the population of which the sample is drawn?
# to assess the possibility , we carry out a test to statistical significance, the Chi Square. 
# Read chapter 7 explanation

numbered_data <- bcs %>% # new dataset with only vars of interest, recoded to have numeric labels
  transmute(rubbcomm = factor(rubbcomm, labels = c(1,2,3,4)),
            bcsvictim = factor(bcsvictim, labels = c(1,2))) 

t_num <- numbered_data %>% # crosstab of rubbcomm and bcsvictim
  tabyl(rubbcomm,
        bcsvictim,
        show_na = FALSE) # make sure to get rid of NAs!

# chi square test
num_chi <- janitor::chisq.test(t_num, tabyl_results = TRUE) # use janitor:: to clarify we want the function from the janitor package, otherwise R might get confused

num_chi

# we can see the expected frecquiencies fr each cell by accessing the expected part of the num_chi object
num_chi$expected
# This is because the num_chi object is acctually a list which prints as a simple chi square output when it is called, but also contains much more info
# We can see a basic corss-tab of the original observed frequiencies
num_chi$observed
# Read Ch7 explanation 
# make base R table
t5 <- table(bcs$rubbcomm, bcs$bcsvictim)

# run chi square test
chisq.test(t5)

# fisher test 
fisher.test(bcs$rubbcomm, # independent variable
            bcs$bcsvictim, # dependent variable
            workspace = 2e+07, # increase size of 'workspace'
            hybrid = TRUE) # use hybrid approximation

# we can also do it using the output of the tabyl approach above
janitor::fisher.test(t_num,
                     workspace = 2e+07, # increase size of 'workspace'
                     hybrid = TRUE) # use hybrid approximation

# Cramer's V - two categorical varibles
# install and load vcd
install.packages("vcd", repos='http://cran.us.r-project.org')
