#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA at
# https://usa.ipums.org/usa/index.shtml
# Author: Yijie Zhao, Yifan Xu, Yuyan Xu and Yuze Kang
# Data: 1 November 2020
# Contact: yijie.zhao@mail.utoronto.ca
# License: MIT
# To save the downloaded data in inputs/data
# To gitignore it.


#### Workspace setup ####
install.packages("haven")
install.packages("tidyverse")
install.packages("labelled")
library(haven)
library(tidyverse)
library(labelled)


#### Load data and choose variables ####

# Read the raw data
ACS2018_raw_data <- read_dta("usa_00008.dta")
# Add the labels
ACS2018_raw_data <- labelled::to_factor(ACS2018_raw_data)

# Keep some variables corresponding with survey data
reduced_ACS2018_data <- ACS2018_raw_data %>% 
  select(age,
         sex,
         race,
         educd,
         hhincome,
         citizen,
         stateicp,
         perwt
         )


#### Clean data ####

# I. Delete NA
# data - 1001961 observations (no NA apparently)
ACS2018_data <- na.omit(reduced_ACS2018_data)

# II. Drop "cannot vote"
# Drop data of observations who aren't eligible to vote
# Change age from factor to numeric 
ACS2018_data$age <- as.numeric(ACS2018_data$age)

# Check available categories of citizen
ACS2018_data %>% group_by(citizen) %>% summarise(count=n())

# Drop observations that:
# - age under 18
# - not a citizen ("n/a" and "not a citizen")
# data - 1001961 to 71369 observations (lots of n/a in citizen)
ACS2018_data <- ACS2018_data[!(
  ACS2018_data$age<18 |
    ACS2018_data$citizen=="n/a" |
    ACS2018_data$citizen=="not a citizen"),]


#### Adjust variables with survey data ####

# I. age
# Use age to create a categorical variable - age_group
# In accordance with survey data
ACS2018_data <- ACS2018_data %>%
  mutate(age_group=case_when(age>=18 & age<=23 ~ "18-23",
                             age>=24 & age<=29 ~ "24-29",
                             age>=30 & age<=39 ~ "30-39",
                             age>=40 & age<=49 ~ "40-49",
                             age>=50 & age<=59 ~ "50-59",
                             age>=60 & age<=69 ~ "60-69",
                             age>=70 ~ "70+"))

# II. race
# Combine categories of race
# Delete observations with more than one major races
# In accordance with survey data
# data - 71369 to 69191 observations
ACS2018_data <- ACS2018_data[!( 
  ACS2018_data$race=="two major races" | 
    ACS2018_data$race=="three or more major races"),] %>%
  mutate(race=case_when(
    race=="white" ~ "White",
    race=="black/african american/negro" ~ "Black",
    race=="american indian or alaska native" |
      race=="chinese" |
      race=="japanese" | 
      race=="other asian or pacific islander" ~ "AAPI",
    race=="other race, nec" ~ "other races"))

# III. educd
# Create new variable education instead of educd
# Combine categories of educd
# In accordance with survey data
ACS2018_data <- ACS2018_data[!(ACS2018_data$educd=="n/a"),] %>%  # delete n/a
  mutate(education=case_when(
    educd=="no schooling completed" |
      educd=="nursery school, preschool" |
      educd=="kindergarten" |
      educd=="grade 1" |
      educd=="grade 2" |
      educd=="grade 3" |
      educd=="grade 4" |
      educd=="grade 5" |
      educd=="grade 6" |
      educd=="grade 7" |
      educd=="grade 8" |
      educd=="grade 9" |
      educd=="grade 10" |
      educd=="grade 11" |
      educd=="12th grade, no diploma" ~ "No high school diploma",
    educd=="regular high school diploma" |
      educd=="ged or alternative credential" ~ "High school diploma",
    educd=="some college, but less than 1 year" |
      educd=="1 or more years of college credit, no degree" ~ "Some college",
    educd=="associate's degree, type not specified" ~ "Associate's Degree",
    educd=="bachelor's degree" ~ "Bachelor's degree",
    educd=="master's degree" |
      educd=="professional degree beyond a bachelor's degree" |
      educd=="doctoral degree" ~ "Graduate degree"))

# IV. hhincome
# Create new variable household_income instead of hhincome
# Combine categories of hhincome
# In accordance with survey data
# data - 69191 to 67419 observations
ACS2018_data <- ACS2018_data[!(ACS2018_data$hhincome==9999999),] %>% # delete n/a
  mutate(household_income=case_when(
    hhincome<=19999 ~ "$19,999 or less",
    hhincome>=20000 & hhincome<=34999 ~ "$20k-$34,999",
    hhincome>=35000 & hhincome<=49999 ~ "$35k-$49,999",
    hhincome>=50000 & hhincome<=64999 ~ "$50k-$64,999",
    hhincome>=65000 & hhincome<=79999 ~ "$65k-$79,999",
    hhincome>=80000 & hhincome<=99999 ~ "$80k-$99,999",
    hhincome>=100000 & hhincome<=124999 ~ "$100k-$124,999",
    hhincome>=125000 & hhincome<=199999 ~ "$125k-$199,999",
    hhincome>=200000 ~ "$200k or more"))

# V. stateicp
# Create new variable state instead of stateicp
# Change category names in accordance with survey data
ACS2018_data <- ACS2018_data %>%
  mutate(state=case_when(stateicp=="connecticut" ~ "CT",
                         stateicp=="maine" ~ "ME",
                         stateicp=="massachusetts" ~ "MA",
                         stateicp=="new hampshire" ~ "NH",
                         stateicp=="rhode island" ~ "RI",
                         stateicp=="vermont" ~ "VT",
                         stateicp=="delaware" ~ "DE",
                         stateicp=="new jersey" ~ "NJ",
                         stateicp=="new york" ~ "NY",
                         stateicp=="pennsylvania" ~ "PA",
                         stateicp=="illinois" ~ "IL",
                         stateicp=="indiana" ~ "IN",
                         stateicp=="michigan" ~ "MI",
                         stateicp=="ohio" ~ "OH",
                         stateicp=="wisconsin" ~ "WI",
                         stateicp=="iowa" ~ "IA",
                         stateicp=="kansas" ~ "KS",
                         stateicp=="minnesota" ~ "MN",
                         stateicp=="missouri" ~ "MO",
                         stateicp=="nebraska" ~ "NE",
                         stateicp=="north dakota" ~ "ND",
                         stateicp=="south dakota" ~ "SD",
                         stateicp=="virginia" ~ "VA",
                         stateicp=="alabama" ~ "AL",
                         stateicp=="arkansas" ~ "AR",
                         stateicp=="florida" ~ "FL",
                         stateicp=="georgia" ~ "GA",
                         stateicp=="louisiana" ~ "LA",
                         stateicp=="mississippi" ~ "MS",
                         stateicp=="north carolina" ~ "NC",
                         stateicp=="south carolina" ~ "SC",
                         stateicp=="texas" ~ "TX",
                         stateicp=="kentucky" ~ "KY",
                         stateicp=="maryland" ~ "MD",
                         stateicp=="oklahoma" ~ "OK",
                         stateicp=="tennessee" ~ "TN",
                         stateicp=="west virginia" ~ "WV",
                         stateicp=="arizona" ~ "AZ",
                         stateicp=="colorado" ~ "CO",
                         stateicp=="idaho" ~ "ID",
                         stateicp=="montana" ~ "MT",
                         stateicp=="nevada" ~ "NV",
                         stateicp=="new mexico" ~ "NM",
                         stateicp=="utah" ~ "UT",
                         stateicp=="wyoming" ~ "WY",
                         stateicp=="california" ~ "CA",
                         stateicp=="oregon" ~ "OR",
                         stateicp=="washington" ~ "WA",
                         stateicp=="alaska" ~ "AK",
                         stateicp=="hawaii" ~ "HI",
                         stateicp=="district of columbia" ~ "DC"))

# Only select useful variables for modeling after adjustment
ACS2018_data <- ACS2018_data %>% 
  select(age_group,
         sex,
         race,
         education,
         household_income,
         state,
         perwt
         )

# Export the dataset - ACS2018_data
write.csv(ACS2018_data, 
          file="/cloud/project/ACS2018_data.csv", row.names=F)


