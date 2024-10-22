#### Preamble ####
# Purpose: Tests cleaned data.
# Author: Daniel Du
# Date: 16 October 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data in 03-clean_data.R
# Any other information needed? Make sure you are in the `election_analysis` rproj


#### Workspace setup ####
library(tidyverse)
library(testthat)

analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Test data ####

# Test that the dataset has 704 rows, 352 each for Trump and Harris
test_that("dataset has 704 rows", {
  expect_equal(nrow(analysis_data), 704)
})

test_that("dataset has 352 rows for Trump", {
  expect_equal(sum(analysis_data$candidate_name == "Donald Trump"), 352)
})

test_that("dataset has 352 rows for Harris", {
  expect_equal(sum(analysis_data$candidate_name == "Kamala Harris"), 352)
})

# Test that the dataset has 14 columns
test_that("dataset has 14 columns", {
  expect_equal(ncol(analysis_data), 14)
})

# Test that the date columns are character type
test_that("'start_date' is date", {
  expect_true(inherits(analysis_data$start_date, "Date"))
})
test_that("'end_date' is date", {
  expect_true(inherits(analysis_data$end_date, "Date"))
})

# Test that the 'partisan' column is NA
test_that("'partisan' column is all NA", {
  expect_true(all(is.na(analysis_data$partisan)))
})

# Test that the 'party' column is correctly labeled as "DEM" or "REP"
test_that("'party' column has only 'DEM' or 'REP'", {
  expect_true(all(analysis_data$party %in% c("DEM", "REP")))
})

# Test that 'numeric_grade' is 2.8 or higher
test_that("'numeric_grade' is 2.8 or higher", {
  expect_true(all(analysis_data$numeric_grade >= 2.8, na.rm = TRUE))
})

# Test that there are no NA values for character columns: 'pollster', 'state', 'population'
test_that("no NA values for character columns", {
  expect_true(all(!is.na(analysis_data$pollster)))
  expect_true(all(!is.na(analysis_data$state)))
  expect_true(all(!is.na(analysis_data$population)))
})

# Test that there are no NA values for 'pct'
test_that("no NA values for percentage", {
  expect_true(all(!is.na(analysis_data$pct)))
})


