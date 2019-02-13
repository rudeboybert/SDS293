#------------------------------------------------------------------------------
# Lec02: 2019/01/30
#------------------------------------------------------------------------------
library(tidyverse)
library(moderndive)

# 1. Load in training and test data
train <- read_csv("https://rudeboybert.github.io/SDS293/static/train.csv")
test <- read_csv("https://rudeboybert.github.io/SDS293/static/test.csv")

# 2. Fit model on training data
house_model <- lm(SalePrice ~ YrSold, data = train)

# 3. Apply fitted model to get predictions for test data
submission <- get_regression_points(house_model, newdata = test, ID = "Id") %>%
  select(Id, SalePrice = SalePrice_hat)

# 4. Output predictions to CSV
write_csv(submission, "submission.csv")



#------------------------------------------------------------------------------
# Lec03: 2019/02/04 Splines
#------------------------------------------------------------------------------
library(tidyverse)
library(nycflights13)
library(broom)

#------------------------------------------------------------------------------
# Create training data and perform exploratory data analysis
# Define training data: hourly temperature recordings at JFK airport in June 2013
training <- weather %>%
  filter(origin == "JFK", month == 6) %>%
  select(time_hour, temp, humid)
training

# Optional: convert temperature from F to C
# training <- training %>%
#   mutate(temp = (temp-32)/1.8)

# Always do an exploratory data analysis first!

# Histogram of outcome variable: temperature
ggplot(training, aes(x = temp)) +
  geom_histogram()

# Histogram of relationship of outcome variable and predictor variable: humidity
training_plot <- ggplot(training, aes(x = humid, y = temp)) +
  geom_point()
training_plot


#------------------------------------------------------------------------------
# Step 1: Fit spline model to training data and save in model_spline object.
fitted_spline_model <- smooth.spline(x = training$humid, y = training$temp, df = 10)

# Extract data frame of info based on fitted model:
fitted_spline_model_points <- fitted_spline_model %>%
  broom::augment()
fitted_spline_model_points

# Plot fitted model on training data:
training_plot +
  geom_line(data = fitted_spline_model_points, aes(x = x, y = .fitted), col = "blue", size = 1)


#------------------------------------------------------------------------------
# Create test data: hourly temperature recordings at JFK airport in May 2013.
# Note here we "know" the outcome variable temp, but in a real Kaggle
# competition you won't!
test <-  weather %>%
  filter(origin == "JFK", month == 5) %>%
  select(time_hour, temp, humid)
test

# Optional: convert temperature from F to C
# test <- test %>%
#   mutate(temp = (temp-32)/1.8)


#------------------------------------------------------------------------------
# Step 2: Make predictions on test data by applying fitted_spline_model
predicted_points <- predict(fitted_spline_model, x = test$humid) %>%
  as_tibble()
predicted_points

# Plot!
ggplot() +
  geom_point(data = test, aes(x = humid, y = temp)) +
  geom_line(data = predicted_points, aes(x = x, y = y), col = "blue", size = 1)


#------------------------------------------------------------------------------
# Exercises: Not to be submitted. I will go over solutions next lecture
# 1. In Step 1, vary the complexity of the model by using values different
# values of df. What happens to you fitted model?
# 2. Change the test set to be measurements at JFK for December. How does our
# fitted model do?
# 3. What is the RMSE of your fitted model on June temperatures?
# July temperatures? December temperatures.

#------------------------------------------------------------------------------
# Solutions:
# 2. Recall in example above training was June data, test was May data. Redefine
# test data to be December
test <-  weather %>%
  filter(origin == "JFK", month == 12) %>%
  select(time_hour, temp, humid)

# Optional: convert temperature from F to C
# test <- test %>%
#   mutate(temp = (temp-32)/1.8)

# Get predicted values:
predicted_points <- predict(fitted_spline_model, x = test$humid) %>%
  as_tibble()

# Plot!
ggplot() +
  geom_point(data = test, aes(x = humid, y = temp)) +
  geom_line(data = predicted_points, aes(x = x, y = y), col = "blue", size = 1) +
  labs(title = "Trained on June, Tested on December")

# Add column of predicted values temp_hat to test
test <- test %>%
  mutate(temp_hat = predicted_points$y)
test

# 3. Many ways to compute RMSE for December
# From scratch
test %>%
  mutate(
    residual = temp - temp_hat,
    squared_residual = residual^2
  ) %>%
  summarize(mse = mean(squared_residual)) %>%
  mutate(rmse = sqrt(mse))

# Using existing functions:
library(yardstick)
test %>%
  yardstick::rmse(truth = temp, estimate = temp_hat)

library(MLmetrics)
MLmetrics::RMSE(y_pred = test$temp_hat, y_true = test$temp)

# 3. To compute RMSE for June, let's combine all the above steps into a
# single dplyr chain:
weather %>%
  # Set test set to be June data
  filter(origin == "JFK", month == 6) %>%
  select(time_hour, temp, humid) %>%
  # Get predicted values temp_hat:
  mutate(
    temp_hat = predict(fitted_spline_model, x = humid) %>% as_tibble() %>% pull(y)
  ) %>%
  # Compute RMSE:
  mutate(
    residual = temp - temp_hat,
    squared_residual = residual^2
  ) %>%
  summarize(mse = mean(squared_residual)) %>%
  mutate(rmse = sqrt(mse))
