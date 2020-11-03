#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded 
# from Democracy Fund + UCLA Nationscape at
# https://www.voterstudygroup.org/publication/nationscape-data-set
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
survey_raw_data <- read_dta("ns20200625.dta")
# Add the labels
survey_raw_data <- labelled::to_factor(survey_raw_data)

# Keep some variables
reduced_survey_data <- survey_raw_data %>% 
  select(vote_2020,
         age,
         gender,
         race_ethnicity,
         education,
         household_income,
         state,
         vote_intention
         )


#### Clean data ####

# I. Delete NA
# data - 6479 to 6101 observations
survey_data <- na.omit(reduced_survey_data)

# II. Drop "not voting"
# Drop data of observations who have great possibility 
# to not vote any candidates in 2020 election
# as our model analysis only cares about potential voters
survey_data %>% group_by(vote_intention) %>% summarise(count=n())

# Out of 4 groups of vote_intention:
# We keep "Yes, I will vote" and "Not sure"
# We drop "No, I will not vote but I am eligible" 
# and "No, I am not eligible to vote"
# data - 6101 to 5394 observations
survey_data <- survey_data[!(
  survey_data$vote_intention=="No, I will not vote but I am eligible" |
    survey_data$vote_intention=="No, I am not eligible to vote"),]

# III. Keep potential voters for Trump and Biden only
# We intend to have a prediction on the competition between Trump and Biden
# two candidates with great probability to win
# We only cares about potential voters for them
survey_data %>% group_by(vote_2020) %>% summarise(count=n())

# Out of 5 groups of vote_2020
# We keep "Donald Trump" (2212) and "Joe Biden" (2397)
# We drop "Someone else" (205), "I would not vote" (84) 
# and "I am not sure/don't know" (496)
# data - 5394 to 4609 observations
survey_data <- survey_data[!(
  survey_data$vote_2020=="Someone else" |
    survey_data$vote_2020=="I would not vote" |
    survey_data$vote_2020=="I am not sure/don't know"),]


#### Adjust variables with ACS_2018 data ####

# I. vote_2020
# Use vote_2020 to create a binary variable - vote_Trump as response variable
survey_data <- survey_data %>% 
  mutate(vote_Trump=ifelse(vote_2020=="Donald Trump",1,0))

# II. age
# Use age to create a categorical variable - age_group
# Refer to Weighting Targets of Nationscape User Guide
survey_data <- survey_data %>%
  mutate(age_group=case_when(age>=18 & age<=23 ~ "18-23",
                             age>=24 & age<=29 ~ "24-29",
                             age>=30 & age<=39 ~ "30-39",
                             age>=40 & age<=49 ~ "40-49",
                             age>=50 & age<=59 ~ "50-59",
                             age>=60 & age<=69 ~ "60-69",
                             age>=70 ~ "70+"))

# III. gender
# Use sex instead of gender as ACS_2018
survey_data <- rename(survey_data, sex=gender)
survey_data <- survey_data %>%
  mutate(sex=case_when(sex=="Female" ~ "female",
                       sex=="Male" ~ "male"))

# IV. race_ethnicity
# Create new variable race instead of race_ethnicity
# Combine categories of race_ethnicity
# Refer to Weighting Targets of Nationscape User Guide
survey_data <- survey_data %>%
  mutate(race=case_when(
    race_ethnicity=="White" ~ "White",
    race_ethnicity=="Black, or African American" ~ "Black",
    race_ethnicity=="American Indian or Alaska Native" |
      race_ethnicity=="Asian (Asian Indian)" |
      race_ethnicity=="Asian (Chinese)" | 
      race_ethnicity=="Asian (Filipino)" |
      race_ethnicity=="Asian (Japanese)" |
      race_ethnicity=="Asian (Korean)" |
      race_ethnicity=="Asian (Vietnamese)" |
      race_ethnicity=="Asian (Other)" |
      race_ethnicity=="Pacific Islander (Native Hawaiian)" |
      race_ethnicity=="Pacific Islander (Guamanian)" |
      race_ethnicity=="Pacific Islander (Samoan)" |
      race_ethnicity=="Pacific Islander (Other)" ~ "AAPI",
    race_ethnicity=="Some other race" ~ "other races"))

# V. education
# Combine categories of education
# Refer to Weighting Targets of Nationscape User Guide
survey_data <- survey_data %>%
  mutate(education=case_when(
    education=="3rd Grade or less" |
      education=="Middle School - Grades 4 - 8" |
      education=="Completed some high school" ~ "No high school diploma",
    education=="High school graduate" ~ "High school diploma",
    education=="Other post high school vocational training" |
      education=="Completed some college, but no degree" ~ "Some college",
    education=="Associate Degree" ~ "Associate's Degree",
    education=="College Degree (such as B.A., B.S.)" |
      education=="Completed some graduate, but no degree" ~ "Bachelor's degree",
    education=="Masters degree" |
      education=="Doctorate degree" ~ "Graduate degree"))

# VI. household_income
# Combine categories of household_income
# Refer to Weighting Targets of Nationscape User Guide
survey_data <- survey_data %>%
  mutate(household_income=case_when(
    household_income=="Less than $14,999" |
      household_income=="$15,000 to $19,999" ~ "$19,999 or less",
    household_income=="$20,000 to $24,999" |
      household_income=="$25,000 to $29,999" |
      household_income=="$30,000 to $34,999" ~ "$20k-$34,999",
    household_income=="$35,000 to $39,999" |
      household_income=="$40,000 to $44,999" |
      household_income=="$45,000 to $49,999" ~ "$35k-$49,999",
    household_income=="$50,000 to $54,999" |
      household_income=="$55,000 to $59,999" |
      household_income=="$60,000 to $64,999" ~ "$50k-$64,999",
    household_income=="$65,000 to $69,999" |
      household_income=="$70,000 to $74,999" |
      household_income=="$75,000 to $79,999" ~ "$65k-$79,999",
    household_income=="$80,000 to $84,999" |
      household_income=="$85,000 to $89,999" |
      household_income=="$90,000 to $94,999" |
      household_income=="$95,000 to $99,999" ~ "$80k-$99,999",
    household_income=="$100,000 to $124,999" ~ "$100k-$124,999",
    household_income=="$125,000 to $149,999" |
      household_income=="$150,000 to $174,999" |
      household_income=="$175,000 to $199,999" ~ "$125k-$199,999",
    household_income=="$200,000 to $249,999" |
      household_income=="$250,000 and above" ~ "$200k or more"))

# VII. state
# Change age from character to factor 
survey_data$state <- as.factor(survey_data$state)

# Only select useful variables for modeling after adjustment
survey_data <- survey_data %>% 
  select(vote_Trump,
         age_group,
         sex,
         race,
         education,
         household_income,
         state
         )

# Export the dataset - survey_data
write.csv(survey_data, 
          file="/cloud/project/survey_data.csv", row.names=F)
