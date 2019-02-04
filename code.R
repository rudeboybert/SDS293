library(tidyverse)
library(nycflights13)
library(broom)

# Define training data: hourly temperature recordings at JFK airport in June 2013
training <- weather %>%
  filter(origin == "JFK", month == 6) %>%
  select(time_hour, temp, humid)
training

# Optional: convert temperature from F to C
training <- training %>%
  mutate(temp = (temp-32)/1.8)

# Always do an exploratory data analysis first!
training_plot <- ggplot(training, aes(x = humid, y = temp)) +
  geom_point()
training_plot

#------------------------------------------------------------------------------
# Step 1: Fit spline model to training data and save in model_spline object.
fitted_spline_model <- smooth.spline(x = training$humid, y = training$temp, df = 40)

# Extract data frame of info based on fitted model:
fitted_spline_model_points <- fitted_spline_model %>%
  broom::augment()
fitted_spline_model_points

# Plot fitted model on training data:
training_plot +
  geom_line(data = fitted_spline_model_points, aes(x = x, y = .fitted), col = "blue", size = 1)


#------------------------------------------------------------------------------
# Step 2: Predict on test data

# Define test data: hourly temperature recordings at JFK airport in July 2013
test <-  weather %>%
  filter(origin == "JFK", month == 7) %>%
  select(time_hour, temp, humid)
test

# Optional: convert temperature from F to C
test <- test %>%
  mutate(temp = (temp-32)/1.8)

# Make predictions on test data by applying fitted_spline_model
predicted_points <- predict(fitted_spline_model, x = test$humid) %>%
  as_tibble()

# Plot!
ggplot() +
  geom_point(data = test, aes(x = humid, y = temp)) +
  geom_line(data = predicted_points, aes(x = x, y = y), col = "blue", size = 1)



