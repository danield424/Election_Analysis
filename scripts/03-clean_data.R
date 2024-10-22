#### Preamble ####
# Purpose: Cleans downloaded dataset.
# Author: Daniel Du
# Date: 16 October 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: Download data from 538 website
# Any other information needed? Make sure you are in the `election_analysis` rproj

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/01-raw_data/president_polls.csv")

cleaned_data <- raw_data |> 
  # Select relevant attributes
  select(pollster, numeric_grade, state, candidate_name, party, pct, sample_size, 
         population, methodology, start_date, end_date, hypothetical, partisan,
         pollscore, transparency_score) |>
  # Drop pollsters without a numeric grade
  drop_na(numeric_grade) 

# Summary statistics for numeric_grade
summary(cleaned_data$numeric_grade)
quantiles <- quantile(cleaned_data$numeric_grade, probs = c(0.67, 0.7, 0.75))
print(quantiles) # 75th quantile: 2.8

# Filter data to Harris and Trump estimates based on high-quality non-partisan polls
cleaned_data <- cleaned_data |>
  mutate(start_date = mdy(start_date),
    end_date = mdy(end_date)) |>
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump"),
    numeric_grade >= 2.8, # 75th quantile, 
    # Also need to look at whether the pollster has multiple polls or just one or two - filter out later
    start_date >= as.Date("2024-07-21"), # When Harris declared
    is.na(partisan),
    hypothetical == FALSE) 

# Clean state column, assigning NA polls to national. Get total counts from percentages 
cleaned_data <- cleaned_data |>
  mutate(state = if_else(is.na(state), "National", state), # Hacky fix for national polls - come back and check
         state = if_else(state == "Maine CD-1", "Maine", state),
         state = if_else(state == "Maine CD-2", "Maine", state),
         state = if_else(state == "Nebraska CD-2", "Nebraska", state),
         voters = round((pct / 100) * sample_size, 0))

print(unique(cleaned_data$pollster))
print(cleaned_data |> filter(pollster %in% c("University of Massachusetts Lowell/YouGov", "McCourtney Institute/YouGov")))
# Clean pollster column, highlighting only main pollster.
cleaned_data <- cleaned_data |>
  mutate(pollster = if_else(pollster == "SurveyUSA/High Point University", "SurveyUSA", pollster),
         pollster = if_else(pollster == "University of Massachusetts Lowell/YouGov", "U of Massachusetts Lowell", pollster),
         pollster = if_else(pollster %in% c(
           "YouGov/Center for Working Class Politics", "McCourtney Institute/YouGov"), "YouGov", pollster),
         pollster = if_else(pollster == "SurveyUSA/High Point University", "SurveyUSA", pollster))

print(cleaned_data |> filter(is.na(sample_size)))

#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/analysis_data.csv")
