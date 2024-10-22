#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(zoo)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

# Aggregate polling percentages over a 7-day rolling average
analysis_data <- analysis_data %>%
  arrange(end_date) %>%
  group_by(candidate_name) %>%
  mutate(rolling_avg7_pct = rollmean(pct, k = 7, fill = NA, align = "right")) %>%
  ungroup()

# Model 1 (fit using aggregated data):
model <- lm(pct ~ candidate_name * (end_date + pollscore + state + sample_size), 
             data = analysis_data, na.action = na.exclude)

# Model 2:
analysis_data <- analysis_data %>%
  mutate(is_national = ifelse(state == "National", 1, 0))

# Fit a simplified model excluding sample_size and using is_national
model2 <- lm(pct ~ candidate_name * (end_date + pollscore + is_national) + sample_size, 
                       data = analysis_data, na.action = na.exclude)

summary(model2)
# Augment data with model predictions
analysis_data <- analysis_data %>%
  mutate(fitted_model = ifelse(complete.cases(candidate_name, end_date, pollscore, state, sample_size),
                                predict(model, newdata = analysis_data), NA),
        fitted_model2 = ifelse(complete.cases(candidate_name, end_date, pollscore, is_national),
                                predict(model2, newdata = analysis_data), NA))

# Plot actual polling percentages with predictions from the models
model_plot <- ggplot(analysis_data, aes(x = end_date, y = pct, color = candidate_name)) +
  theme_minimal() +
  geom_point(alpha = 0.5) + # Actual data points
  geom_line(aes(y = fitted_model, group = candidate_name), size = 1) + # Smoothed model predictions
  labs(y = "Polling Percentage", x = "Date", 
       title = "Smoothed Polling Percentages with Rolling Average and Linear Model") +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position = "bottom")
model_plot

model2_plot <- ggplot(analysis_data, aes(x = end_date, y = pct, color = candidate_name)) +
  theme_minimal() +
  geom_point(alpha = 0.5) + # Actual data points
  geom_line(aes(y = fitted_model2, group = candidate_name), size = 1) + # Smoothed model predictions
  labs(y = "Polling Percentage", x = "Date", 
       title = "Polling Percentages with Rolling Average and Linear Model") +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position = "bottom")
model2_plot

# #### Bayesian model
# # Define priors
# priors <- normal(0, 2.5, autoscale = TRUE)
# 
# # Fit the Bayesian linear model using stan_glm
# model3 <- stan_glm(
#   pct ~ candidate_name * (end_date + pollscore + is_national) + sample_size, 
#   data = analysis_data,
#   prior = priors,
#   prior_intercept = priors,
#   family = gaussian(),  # Use a Gaussian family for continuous outcomes
#   seed = 123,
#   cores = 4,
#   adapt_delta = 0.95
# )
# 
# # Summarize the Bayesian model
# summary(model3)
# 
# bayesian_predictions <- posterior_predict(model3, newdata = analysis_data, draws = 1000)
# bayesian_predictions
# mean_predictions <- colMeans(bayesian_predictions)
# 
# # Add predictions to the data frame
# analysis_data <- analysis_data %>%
#   mutate(bayesian_prediction = mean_predictions)
# 
# # Plot actual polling percentages with Bayesian model predictions
# model3_plot <- ggplot(analysis_data, aes(x = end_date, y = pct, color = candidate_name)) +
#   theme_minimal() +
#   geom_point(alpha = 0.5) + # Actual data points
#   geom_line(aes(y = bayesian_prediction, group = candidate_name), size = 1) + # Bayesian model predictions
#   labs(y = "Polling Percentage", x = "Date", 
#        title = "Polling Percentages with Bayesian Model") +
#   scale_color_manual(values = c("red", "blue")) +
#   theme(legend.position = "bottom")
# 
# # Display the plot
# model3_plot




prediction_data <- analysis_data %>%
  filter(!is.na(pct)) %>%
  distinct(candidate_name, .keep_all = TRUE) %>% # Keep only one entry per candidate
  mutate(end_date = as.Date("2024-11-05")) # Set the date to the election date

# Predict the polling percentage for each candidate on the election date
prediction_data <- prediction_data %>%
  mutate(predicted_pct = predict(model, newdata = prediction_data))

# Print the prediction results
print(prediction_data %>% select(candidate_name, predicted_pct))


# All of the above would be in scripts - data cleaning scripts and modelling scripts. 
# This is an example of how you get a results table that you could put into your Quarto doc
modelsummary(models = list("Model 1" = model, "Model 2" = model2))

