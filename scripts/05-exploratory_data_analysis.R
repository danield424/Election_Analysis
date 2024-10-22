#### Preamble ####
# Purpose: Explores variables of cleaned data and tests basic models.
# Author: Daniel Du
# Date: 19 October 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data in 03-clean_data.R
# Any other information needed? Make sure you are in the `election_analysis` rproj

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(grid)
#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Plot poll estimates for both candidates ####
basic_plot <- ggplot(analysis_data, aes(x = end_date, y = pct, color = candidate_name)) +
  theme_minimal() +
  labs(y = "Percent", x = NULL, color = "Candidate",
       title = "Polling Percentages for Harris and Trump Over Time") +
  geom_point() + geom_smooth()
basic_plot

# Color by pollster, but still differentiate candidates
base_plot <- ggplot(analysis_data, aes(x = end_date, y = pct)) +
  theme_minimal() +
  labs(y = "Percent", x = NULL, 
    title = "Polling Percentages for Harris and Trump Over Time") +
# Plot points with color based on pollster and shape based on candidate name
  geom_point(aes(color = pollster, shape = candidate_name)) +
# Add separate trendlines for each candidate
  geom_smooth(data = subset(analysis_data, candidate_name == "Donald Trump"),
    color = "red") +
  geom_smooth(data = subset(analysis_data, candidate_name == "Kamala Harris"),
    color = "blue") +
  theme(legend.position = "right",
    legend.key.size = unit(0.5, 'cm'),  # Shrink the legend keys
    legend.text = element_text(size = 8), # Reduce legend text size
    legend.box = "vertical") +
  # Separate guides for color and shape legends
  guides(
    color = guide_legend(title = "Pollster", nrow = 18, title.position = "top"),
    shape = guide_legend(title = "Candidate", title.position = "top", override.aes = list(color = c("red", "blue"))) 
  )
base_plot

harris_by_pollster <- ggplot(data = subset(analysis_data, candidate_name == "Kamala Harris"), 
                             aes(x = end_date, y = pct)) +
  theme_minimal() +
  labs(y = "Harris percent", x = "Date") +
  geom_point() +
  geom_smooth(color = "red") +
  facet_wrap(vars(pollster)) + ylim(30, 70) +
  labs(title = "Polling Percentages Faceted by Pollster for Kamala Harris")
harris_by_pollster

trump_by_pollster <- ggplot(data = subset(analysis_data, candidate_name == "Donald Trump"), 
                            aes(x = end_date, y = pct)) +
  theme_minimal() +
  labs(y = "Trump percent", x = "Date") +
  geom_point() +
  geom_smooth(color = "red") +
  facet_wrap(vars(pollster)) + ylim(30, 70) +
  labs(title = "Polling Percentages Faceted by Pollster for Donald Trump")
trump_by_pollster

# Color by pollscore
harris_pollscore <- ggplot(data = subset(analysis_data, candidate_name == "Kamala Harris"), 
  aes(x = end_date, y = pct)) +
  theme_minimal() +
  labs(y = "Harris percent", x = "Date") +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")
harris_pollscore

trump_pollscore <- ggplot(data = subset(analysis_data, candidate_name == "Donald Trump"), 
                           aes(x = end_date, y = pct)) +
  theme_minimal() +
  labs(y = "Trump percent", x = "Date") +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth(color = "red") +
  theme(legend.position = "bottom")
trump_pollscore
