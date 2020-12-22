#### Preamble ####
# Purpose: Cleans the CES survey data for specific variables.
# Author: Kelvin Lin
# Data: December 22, 2020
# Contact: slh.kelvin@gmail.com
# License: MIT

library(haven)
library(tidyverse)

devtools::install_github("hodgettsp/cesR")
library(cesR)
library(tidyverse)
library(visdat)
library(skimr)
setwd("C:/Users/Kelvin/Desktop/STA304/Final Project")

ces <- get_ces("ces2019_phone") #survey data
raw_data <- labelled::to_factor(ces2019_phone)

reduced_data <- raw_data %>% 
  select(age, q3, q11, q71, q4)
  # Mapping the selected variables: age = age, q3 = gender, q11 = vote, q71 = hh_size, q4 = province

reduced_data<-
  reduced_data %>%
  
  count(age, q3, q11, q71, q4)%>%
  group_by(age, q3, q11, q71, q4)%>%
  filter((age) >= 18 && (age) <= 80) %>%

  filter(q3 != "(3) Other") %>%

  mutate(sex = case_when(
  q3 == "(1) Male" ~ "Male",
  q3 == "(2) Female" ~ "Female"
  )) %>%

  mutate(vote = case_when(
    q11 == "(1) Liberal (Grits)" ~ 1,
    q11 == "(2) Conservatives (Tory, PCs, Conservative Party of Canada)" ~ 0,
    q11 == "(3) NDP (New Democratic Party, New Democrats, NDPers)" ~ 0,
    q11 == "(4) Bloc Québécois (BQ, PQ, Bloc, Parti Québécois)" ~ 0,
    q11 == "(5) Green Party (Greens)" ~ 0,
    q11 == "(6) People's Party" ~ 0,
    q11 == "(7) Other" ~ 0,
    q11 == "(8) Will not vote" ~ 0,
    q11 == "(9) None of these" ~ 0,
    q11 == "(10) Will spoil ballet" ~ 0,
    q11 == "(-8) Refused" ~ 0,
    q11 == "(-9) Don't know / Undecided" ~ 0
  )) %>%

  filter(q71 != "7") %>%
  filter(q71 != "8") %>%
  filter(q71 != "9") %>%
  filter(q71 != "10") %>%
  filter(q71 != "11") %>%
  filter(q71 != "12") %>%
  filter(q71 != "13") %>%
  filter(q71 != "14") %>%
  filter(q71 != "15") %>%
  filter(q71 != "(-7) Skipped") %>%
  filter(q71 != "(-8) Refused") %>%
  filter(q71 != "(-7) Skipped") %>%
  
  mutate(hh_size = case_when(
    q71 == "1" ~ "1",
    q71 == "2" ~ "2",
    q71 == "3" ~ "3",
    q71 == "4" ~ "4",
    q71 == "5" ~ "5",
    q71 == "6" ~ "6"
  )) %>%
  
  mutate(province = case_when(
    q4 == "(1) Newfoundland and Labrador" ~ "Newfoundland and Labrador",
    q4 == "(2) Prince Edward Island" ~ "Prince Edward Island",
    q4 == "(3) Nova Scotia" ~ "Nova Scotia",
    q4 == "(4) New Brunswick" ~ "New Brunswick",
    q4 == "(5) Quebec" ~ "Quebec",
    q4 == "(6) Ontario" ~ "Ontario",
    q4 == "(7) Manitoba" ~ "Manitoba",
    q4 == "(8) Saskatchewan" ~ "Saskatchewan",
    q4 == "(9) Alberta" ~ "Alberta",
    q4 == "(10) British Columbia" ~ "British Columbia"
  ))
  
write_csv(reduced_data, "new_ces2019.csv")

# References:
# 1. Alexander, R., & Caetano, S. J. (2020). Telling Stories With Data. Retrieved December 20, 2020, from https://www.tellingstorieswithdata.com/
# 2. Stephenson, L.B.; Harell, A., Rubenson, D. & Loewen, P.J. (2020). 2019 Canadian Election Study - Phone Survey. Harvard Dataverse, V1. UNF:6:eyR28qaoYlHj9qwPWZmmVQ==fileUNF. https://doi.org/10.7910/DVN/8RHLG1.
# 3. Wickham, H. & Miller, E. (2020). haven: Import and Export. 'SPSS', 'Stata' and 'SAS' Files. R package version 2.3.1. https://CRAN.R-project.org/package=haven
# 4. Wickham et al., (2019). Welcome to the tidyverse. Journal of Open. Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
# 5. Stephenson, L.B., Harrel A., Rubenson, D. & Loewen, P.J. Forthcoming. 'Measuring Preferences and Behaviour in the 2019 Canadian Election Study,' Canadian Journal of Political Science.
# 6. Hodgetts, P. & Alexander, R. (2020). cesR: Access the CES Datasets a Little Easier.. R package version 0.1.0.
# 7. Tierney, N. (2017). "visdat: Visualising Whole Data Frames." _JOSS_,*2*(16), 355. doi: 10.21105/joss.00355 (URL:https://doi.org/10.21105/joss.00355), <URL:http://dx.doi.org/10.21105/joss.00355>.
# 8. Waring, E., Quinn, M., McNamara, A., Arino de la Rubia, E., Zhu, H. & Ellis, S. (2020). skimr: Compact and Flexible Summaries of Data. R package version 2.1.2. https://CRAN.R-project.org/package=skimr
# 9. Wickham, H., Hester, J. & Chang, W. (2020). devtools: Tools to Make Developing R Packages Easier. R package version 2.3.2. https://CRAN.R-project.org/package=devtools
# 10. Larmarange, J. (2020). labelled: Manipulating Labelled Data. R package version 2.7.0. https://CRAN.R-project.org/package=labelled
# 11. Alexander, R. & Caetano, S. (2020). Quercus. Retrieved December 21, 2020, from https://q.utoronto.ca/