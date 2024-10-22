#### Preamble ####
# Purpose: Tests the structure and validity of the simulated American election
# polling dataset.
# Author: Daniel Du
# Date: 16 October 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj

#### Workspace setup ####
library(tidyverse)

analysis_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded
if (exists("analysis_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

#### Test data ####

# Check if the dataset has the expected number of rows (5 in this example)
expected_rows <- 5
if (nrow(analysis_data) == expected_rows) {
  message("Test Passed: The dataset has ", expected_rows, " rows.")
} else {
  stop("Test Failed: The dataset does not have ", expected_rows, " rows.")
}

# Check if the dataset has 4 columns
if (ncol(analysis_data) == 4) {
  message("Test Passed: The dataset has 4 columns.")
} else {
  stop("Test Failed: The dataset does not have 4 columns.")
}

# Check if all values in the 'pollster' column are unique
if (n_distinct(analysis_data$pollster) == nrow(analysis_data)) {
  message("Test Passed: All values in 'pollster' are unique.")
} else {
  stop("Test Failed: The 'pollster' column contains duplicate values.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(analysis_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if the 'trump_percentage' and 'harris_percentage' columns contain valid numeric values (0-100)
if (all(analysis_data$trump_percentage >= 0 & analysis_data$trump_percentage <= 100) &&
    all(analysis_data$harris_percentage >= 0 & analysis_data$harris_percentage <= 100)) {
  message("Test Passed: The 'trump_percentage' and 'harris_percentage' columns contain valid numeric values.")
} else {
  stop("Test Failed: The 'trump_percentage' or 'harris_percentage' columns contain invalid numeric values.")
}
