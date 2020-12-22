#### Preamble ####
# Purpose: Cleans the GSS census data for specific variables.
# Author: Kelvin Lin
# Data: December 22, 2020
# Contact: slh.kelvin@gmail.com
# License: MIT

library(haven)
library(tidyverse)

setwd("C:/Users/Kelvin/Desktop/STA304/Final Project")
raw_data <- read.csv("gss.csv")

raw_data <- labelled::to_factor(raw_data)

reduced_data <- 
  raw_data %>% 
  select(sex, age, hh_size, province) 

reduced_data <- 
  reduced_data %>%
  count(sex, age, hh_size, province) %>%
  group_by(sex, age, hh_size, province) %>%
  filter((age) >= 18)

reduced_data$age <- as.integer(reduced_data$age)

write_csv(reduced_data, "census_data.csv")

# References:
# 1. Alexander, R., & Caetano, S. J. (2020). Telling Stories With Data. Retrieved December 20, 2020, from https://www.tellingstorieswithdata.com/
# 2. Wickham, H. & Miller, E. (2020). haven: Import and Export. 'SPSS', 'Stata' and 'SAS' Files. R package version 2.3.1. https://CRAN.R-project.org/package=haven
# 3. Wickham et al., (2019). Welcome to the tidyverse. Journal of Open. Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
# 4. Larmarange, J. (2020). labelled: Manipulating Labelled Data. R package version 2.7.0. https://CRAN.R-project.org/package=labelled
# 5. Alexander, R. & Caetano, S. (2020). Quercus. Retrieved December 21, 2020, from https://q.utoronto.ca/