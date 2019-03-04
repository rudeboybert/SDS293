#------------------------------------------------------------------------------
# Lec09: 2019/03/04
#------------------------------------------------------------------------------
library(tidyverse)
library(broom)

# Read in training data from https://www.kaggle.com/c/GiveMeSomeCredit/
financial_distress_orig <-
  "https://rudeboybert.github.io/SDS293/static/methods/logisitic/cs-training.csv" %>%
  read_csv() %>%
  select(ID = X1, in_financial_distress = SeriousDlqin2yrs, age)

# Let's deliberately tinker and engineer this data for educational purposes
# only: For those individuals who are in financial distress, let's add an offset
# of 50 to their ages
offset <- 50
financial_distress <- financial_distress_orig %>%
  mutate(age = ifelse(in_financial_distress == 1, age + offset, age))

# Split data into train and test so that we can fit to train and predict on
# test. Note that this corresponds to the "validation set" approach that is
# used mostly for illustrative purposes and not used in practice as using this
# approach you wouldn't be making predictions on every observation.
# Be sure to View() these data frames after you create them:
set.seed(76)
cs_training <- financial_distress %>%
  sample_frac(0.25)
cs_test <- financial_distress %>%
  anti_join(cs_training, by="ID")

# EDA: Recall that we engineering the two boxplots to not overlap by adding an
# offset
ggplot(cs_training, aes(x = as.logical(in_financial_distress), y = age)) +
  geom_boxplot() +
  labs(x = "In financial distress?", y = "Age")

# Let's create a scatterplot but with age on the x-axis. Note this plot suffers
# from overplotting:
ggplot(cs_training, aes(x = age, y = in_financial_distress)) +
  geom_point() +
  labs(x = "Age", y = "In financial distress?")

# Let's "jitter" the plot a little to break up the overplotting. In other words,
# add random vertical "nudges" to the points so that we can get a sense of how
# many plots are on top of each other. Note this is only a visualization tool;
# it does not alter the original values in the data frame.
# For more info on geom_jitter read:
# https://moderndive.netlify.com/3-viz.html#overplotting
ggplot(cs_training, aes(x = age, y = in_financial_distress)) +
  geom_jitter(height = 0.01) +
  labs(x = "Age", y = "In financial distress?")

# The best fitting linear regression line in blue is no good in this particular
# case; you end up with fitted probabilities less than 0
ggplot(cs_training, aes(x = age, y = in_financial_distress)) +
  geom_jitter(height = 0.01) +
  labs(x = "Age", y = "In financial distress?") +
  geom_smooth(method = "lm", se = FALSE)

# Fit a logistic regression model. Note the use of glm() instead of lm()
model_logistic <- glm(in_financial_distress ~ age, family = "binomial", data = cs_training)

# 2.a) Extract regression table with confidence intervals
# Notice coefficient for age. Is it positive or negative?
model_logistic %>%
  broom::tidy(conf.int = TRUE)

# 2.b) Extract point-by-point info of points used to fit model
fitted_points_logistic <- model_logistic %>%
  broom::augment()
fitted_points_logistic

# The .fitted values are the fitted log-odds however, NOT fitted probabilities.
# We convert to fitted probabilities using inverse-logit function:
fitted_points_logistic <- fitted_points_logistic %>%
  mutate(fitted_prob = 1/(1 + exp(-.fitted)))
fitted_points_logistic

# 2.c) Extract model summary info
model_logistic %>%
  broom::glance()

# 3. Make predictions on test data. Compare this to use of broom::augment()
# for fitted_points_logistic
predicted_points_logistic <- model_logistic %>%
  broom::augment(newdata = cs_test)
predicted_points_logistic

# 4. Visualize fitted model only the training data for now:
ggplot(data = fitted_points_logistic, aes(x = age, y = in_financial_distress)) +
  # Training data with black points:
  geom_jitter(height = 0.01) +
  # Best fitting linear regression line in blue:
  geom_smooth(method = "lm", se = FALSE) +
  # Best fitting logistic curve in red:
  geom_line(data = fitted_points_logistic, mapping = aes(y = fitted_prob), col = "red", size = 1) +
  labs(x = "Age", y = "In financial distress?")


#------------------------------------------------------------------------------
# Exercises

# 1. Using the visualization above, for what age would you say that there is a
# 50% probability that an individual is in financial distress?

# 2. Compare the visualization above with a scatterplot with:
# a) x = age
# b) y = the observed proportion of individuals in cs_training that are in
# financial
observed_proportions <- cs_training %>%
  group_by(age) %>%
  summarize(prop = mean(in_financial_distress))

ggplot(data = fitted_points_logistic, aes(x = age, y = in_financial_distress)) +
  # Training data with black points:
  geom_jitter(height = 0.01) +
  # Best fitting linear regression line in blue:
  geom_smooth(method = "lm", se = FALSE) +
  # Best fitting logistic curve in red:
  geom_line(data = fitted_points_logistic, mapping = aes(y = fitted_prob), col = "red", size = 1) +
  labs(x = "Age", y = "In financial distress?") +
  geom_line(data = observed_proportions, aes(x = age, y = prop), col = "orange", size = 1)

# 3. Change the offset in age to 10 and -50. What do you notice happens to:
# a) the coefficient for age in the regression table.
# b) the shape of the logistic curve of the fitted model?

# 4. Challenge question: Change the offset in age to 6.9. Why is the logistic curve
# flat? At what value is it?




#------------------------------------------------------------------------------
# Lec08: 2019/02/25
#------------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(stringr)


#------------------------------------------------------------------------------
# Data for today
# Read over the help file
?mtcars

# Data wrangling
mtcars <- mtcars %>%
  # Convert to tibble data frame:
  as_tibble() %>%
  # Add identification variable as first column:
  mutate(ID = 1:n()) %>%
  select(ID, everything()) %>%
  # vs & am variables were recorded as numerical 0/1, but are really categorical
  # so convert them
  mutate(
    vs = ifelse(vs == 0, "V-shaped", "straight"),
    am = ifelse(am == 0, "automatic", "manual")
  )

# Set up validation set framework: create training and test set at random
set.seed(76)
mtcars_train <- mtcars %>%
  sample_frac(0.75)
mtcars_test <- mtcars %>%
  anti_join(mtcars_train, by="ID")

glimpse(mtcars_train)
glimpse(mtcars_test)


#------------------------------------------------------------------------------
# Model 1:
# y: fuel efficiency: measured in miles per gallon
# x: engine horsepower: unit of power where 1hp = work needed to lift 75kg a
# vertical distance of 1 meter in 1 second: https://en.wikipedia.org/wiki/Horsepower

# 1. Fit model to training data
model_1_formula <- as.formula("mpg ~ hp")
model_1 <- lm(model_1_formula, data = mtcars_train)

# 2.a) Extract regression table with confidence intervals
model_1 %>%
  broom::tidy(conf.int = TRUE)

# 2.b) Extract point-by-point info of points used to fit model
fitted_points_1 <- model_1 %>%
  broom::augment()
fitted_points_1

# 2.c) Extract model summary info
model_1 %>%
  broom::glance()

# 3. Make predictions on test data. Compare this to use of broom::augment()
# for fitted_points()
predicted_points_1 <- model_1 %>%
  broom::augment(newdata = mtcars_test)
predicted_points_1

# 4. Visualize
ggplot(NULL) +
  # Training data with black points:
  geom_point(data = fitted_points_1, aes(x = hp, y = mpg)) +
  # Fitted simple linear regression model with blue line:
  geom_line(data = fitted_points_1, aes(x = hp, y = .fitted), col = "blue") +
  # Predictions for test set with red points
  geom_point(data = predicted_points_1, aes(x = hp, y = .fitted), col = "red") +
  labs(x = "Horse power", y = "Miles per gallon")


#------------------------------------------------------------------------------
# Model 2:
# y: fuel efficiency: measured in miles per gallon
# x: All numerical predictors

# 1. Fit model to training data
model_2_formula <- as.formula("mpg ~ cyl + disp + hp + drat + wt + qsec + gear + carb")
model_2 <- lm(model_2_formula, data = mtcars_train)

# 1. Here is a hint for generating the right-hand side (RHS) of formulas when
# you have a lot of predictors. Note how I removed ID and mpg from RHS.
names(mtcars) %>%
  str_c(collapse = " + ")

# 2.a) Extract regression table with confidence intervals
model_2 %>%
  broom::tidy(conf.int = TRUE)

# 2.b) Extract point-by-point info of points used to fit model
fitted_points_2 <- model_2 %>%
  broom::augment()
fitted_points_2

# 2.c) Extract model summary info
model_2 %>%
  broom::glance()

# 3. Make predictions on test data. Compare this to use of broom::augment()
# for fitted_points()
predicted_points_2 <- model_2 %>%
  broom::augment(newdata = mtcars_test)
predicted_points_2


#------------------------------------------------------------------------------
# Exercises

# 1. What are the test set RMSEs of Models 1 & 2? Which is higher?

# 2. What is the ratio of n/p for our trained Model 2. i.e. the number of points
# in the training set vs the number of predictors

# 3. Change the train/test validation ratio from 3:1 to 1:1. What are the RMSEs
# of Models 1 & 2? Which is higher?

# 4. What is the new ratio of n/p for our new trained Model 2?

# 5. How does the difference in test set RMSE for Model 1 & 2 itself differ when the
# train/test validation ratio went from 3:1 to 1:1

# 6. Try a different combination of variables and see if you can lower your
# RMSE.





predicted_points_1 %>%
  yardstick::rmse(truth = mpg, estimate = .fitted)
predicted_points_2 %>%
  yardstick::rmse(truth = mpg, estimate = .fitted)






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
