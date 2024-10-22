#### Preamble ####
# Purpose: Simulates a dataset of pollster predictions of the popular vote, for
# the upcoming US Election between Kamala Harris and Donald Trump.
# Author: Daniel Du
# Date: 15 October 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `election_analysis` rproj

#### Workspace setup ####
library(tidyverse)
set.seed(424)

#### Create data ####
# Pollster names
pollsters <- c(
  "Pollster A",
  "Pollster B",
  "Pollster C",
  "Pollster D",
  "Pollster E"
)

# Predicted percentages for each candidate
pollster_data <- tibble(
  pollster = pollsters,
  trump_percentage = c(40, 43, 47, 45, 51),  # Predicted popular vote for Trump
  harris_percentage = c(52, 48, 44, 48, 43), # Predicted popular vote for Harris
  state = c("National", "Idaho", "Hawaii", "National", "Florida")
)

#### Save data ####
write_csv(pollster_data, "data/00-simulated_data/simulated_data.csv")
