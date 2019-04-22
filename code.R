#------------------------------------------------------------------------------
# Lec21 trimmed: 2019/04/22
#------------------------------------------------------------------------------
library(tidyverse)
library(glmnet)
library(broom)
library(modelr)
library(ISLR)


# 1. Load Credit data set from ISLR package, convert to tibble, select subset
# of variables, and only 20 rows!
set.seed(76)
credit <- ISLR::Credit %>%
  as_tibble() %>%
  select(Balance, Income, Limit, Rating, Cards, Age, Education, Married) %>%
  sample_n(20)
credit


# 2. Define model formula where credit card balance (debt) is the outcome variable
model_formula <-
  "Balance ~ Income + Limit + Rating + Cards + Age + Education + Married" %>%
  as.formula()

# Fit unregularized multiple regression model and output regression table. The
# unregularized beta-hat coefficients are in the estimate column. Recall from
# Lec18 notes that this is one "extreme". REMEMBER THESE VALUES!!!
lm(model_formula, data = credit) %>%
  tidy(conf.int = TRUE)

# Recall the other "extreme" is a model that is completely regularized, meaning
# you use none of the predictors, so that y_hat is simply the mean balance.
# REMEMBER THIS VALUE AS WELL!!!
mean(credit$Balance)


# 3. Based on the above model formula, create "model matrix" representation of
# the predictor variables. Note:
# -the model_matrix() function conveniently converts all categorical predictors
# to numerical ones using one-hot encoding as seen in MP4
# -we remove the first column corresponding to the intercept because it is
# simply a column of ones.
x_matrix <- credit %>%
  modelr::model_matrix(model_formula, data = .) %>%
  select(-`(Intercept)`) %>%
  as.matrix()

# Compare the original data to the model matrix. What is different?
credit
x_matrix


# 4.a) Fit a LASSO model. Note the inputs
# -Instead of inputing a model formula, you input the corresponding x_matrix and
# outcome variable
# -Setting alpha = 1 sets the regularization method to be LASSO. Setting it to be 0
# sets the regularization method to be "ridge regression", another regulization
# method that we don't have time to cover in this class
# -lambda is complexity/tuning parameter whose value we specify. Here let's
# specify 10, an arbitrarily chosen value
LASSO_fit_a <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = 10)
LASSO_fit_a

# Unfortunately the output isn't that informative. Let's use a wrapper function
# that yields a more informative output:
get_LASSO_coefficients <- function(LASSO_fit){
  beta_hats <- LASSO_fit %>%
    broom::tidy(return_zeros = TRUE) %>%
    select(term, estimate, lambda) %>%
    arrange(desc(lambda))
  return(beta_hats)
}
get_LASSO_coefficients(LASSO_fit_a)

# For that value of lambda = 10, we have the beta-hat coefficients that minimizes
# the equation seen in Lec19 via numerical optimization. Observe how all the
# beta-hats have been shrunk while the beta-hat for Limit variable has been
# "shrunk" to 0 and hence is dropped from the model. Compare above output with
# previously seen "unregularized" regression results
lm(model_formula, data = credit) %>%
  tidy(conf.int = TRUE)


# 4.b) Fit a LASSO model considering TWO lambda tuning/complexity parameters at
# once and look at beta-hats
lambda_inputs <- c(10, 1000)
LASSO_fit_b <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = lambda_inputs)
get_LASSO_coefficients(LASSO_fit_b)

# The above output is in tidy/long format, which makes it hard to compare beta-hats
# for both lambda values. Let's convert it to wide format and compare the beta-hats
get_LASSO_coefficients(LASSO_fit_b) %>%
  spread(lambda, estimate)

# Notice how for the larger lambda, all non-intercept beta-hats have been shrunk
# to 0. All that remains is the intercept, whose value is the mean of the y.
# This is because lambda = 1000 penalizes complexity more harshly.


# 4.c) Fit a LASSO model with several lambda tuning/complexity parameters at once
# and look at beta-hats
lambda_inputs <- seq(from = 0, to = 1000)
lambda_inputs
LASSO_fit_c <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = lambda_inputs)

# Since we are now considering several possible values of lambda tuning parameter
# let's visualize instead:
get_LASSO_coefficients(LASSO_fit_c) %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda", y = "beta-hat")

# Observe:
# -For lambda = 0 i.e. no complexity penalization, the values of the beta-hats
# are the unregularized lm() multiple regression values from earlier.
# i.e. one extreme
# -At around lambda = 500, all the beta-hat slopes for all our predictor variables
# have been shrunk to 0 and all that remains is the intercept, which is the mean
# y value in our training set. i.e. the other extreme

# However a typical LASSO plot doesn't show the intercept since it is a beta-hat
# value that is not a candidate to be shrunk to zero, so let's remove it from
# our plot:
get_LASSO_coefficients(LASSO_fit_c) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda", y = "beta-hat")

# It's hard to see in what order the beta-hats get shrunk to 0, so let's zoom-in
# the plot a bit
get_LASSO_coefficients(LASSO_fit_c) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda", y = "beta-hat") +
  coord_cartesian(xlim=c(0, 500), ylim = c(-10, 10))

# The results are a little compressed on the left-end of the x-axis, so
# let's rescale the x-axis to be on a log10 scale:
get_LASSO_coefficients(LASSO_fit_c) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda (log10-scale)", y = "beta-hat") +
  coord_cartesian(xlim=c(1, 500), ylim = c(-10, 10)) +
  scale_x_log10()

# Ask yourself, in what order are the variables being shrunk to 0?


# 4.d) Fit a LASSO model with a narrower search grid of lambda tuning/complexity
# parameter values AND such that the lambdas are spaced by multiplicative powers
# of 10, instead of additive differences, and look at beta-hats
lambda_inputs <- 10^seq(from = -5, to = 3, length = 100)
summary(lambda_inputs)
LASSO_fit_d <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = lambda_inputs)

# Plot all beta-hats with lambda on log10-scale
LASSO_coefficients_plot <- get_LASSO_coefficients(LASSO_fit_d) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda (log10-scale)", y = "beta-hat") +
  scale_x_log10()
LASSO_coefficients_plot

# Zoom-in. In what order to the beta-hat slopes get shrunk to 0?
LASSO_coefficients_plot +
  coord_cartesian(xlim = c(10^0, 10^3), ylim = c(-2, 2))


# 5. However, how do we know which lambda value to use? Should we set it to
# yield a less complex or more complex model? Let's use the glmnet package's
# built in crossvalidation functionality, using the same search grid of
# lambda_input values:
lambda_inputs <- 10^seq(from = -5, to = 3, length = 100)
LASSO_CV <- cv.glmnet(
  x = x_matrix,
  y = credit$Balance,
  alpha = 1,
  lambda = lambda_inputs,
  nfolds = 10,
  type.measure = "mse"
)
LASSO_CV

# Alas that output is not useful, so let's broom::tidy() it
LASSO_CV %>%
  broom::tidy() %>%
  rename(mse = estimate)

# What is te smallest estimated mse?
LASSO_CV %>%
  broom::tidy() %>%
  rename(mse = estimate) %>%
  arrange(mse)

# The lambda_star is in the top row. We can extract this lambda_star value from
# the LASSO_CV object:
lambda_star <- LASSO_CV$lambda.min
lambda_star

# What do the all these values mean? For each value of the lambda
# tuning/complexity parameter, let's plot the estimated MSE generated by
# crossvalidation:
CV_plot <- LASSO_CV %>%
  broom::tidy() %>%
  rename(mse = estimate) %>%
  arrange(mse) %>%
  # plot:
  ggplot(aes(x = lambda)) +
  geom_point(aes(y = mse)) +
  scale_x_log10() +
  labs(x = "lambda (log10-scale)", y = "Estimated MSE")
CV_plot

# Zoom-in:
CV_plot +
  coord_cartesian(xlim=c(10^(-2), 10^2), ylim = c(40000, 50000))

# Mark the lambda_star with dashed blue line
CV_plot +
  coord_cartesian(xlim=c(10^(-2), 10^2), ylim = c(40000, 50000)) +
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue")


# 6. Now mark lambda_star in beta-hat vs lambda plot:
LASSO_coefficients_plot +
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue")

# zoom-in:
LASSO_coefficients_plot +
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue") +
  coord_cartesian(ylim = c(-3, 3))

# What are the beta_hat values resulting from lambda_star? Which are shrunk to 0?
get_LASSO_coefficients(LASSO_fit_d) %>%
  filter(lambda == lambda_star)


# 7. Get predictions from f_hat LASSO model using lambda_star
credit <- credit %>%
  mutate(y_hat_LASSO = predict(LASSO_fit_d, newx = x_matrix, s = lambda_star)[,1])
credit


# 8. Train/test framework
credit_train <- credit %>%
  slice(1:10)
credit_test <- credit %>%
  slice(11:20) %>%
  # Remove outcome variable for test set
  select(-Balance)

# model matrix representation of predictor variables for training set:
x_matrix_train <- credit_train %>%
  modelr::model_matrix(model_formula, data = .) %>%
  select(-`(Intercept)`) %>%
  as.matrix()

# model matrix representation of predictor variables for test set:
x_matrix_test <- credit_test %>%
  modelr::model_matrix(model_formula, data = .) %>%
  select(-`(Intercept)`) %>%
  as.matrix()

# The previous didn't work b/c there is no outcome variable Balance in test as
# specified in model_formula. The solution is to create a temporary dummy
# variable of 1's (or any value); it makes no difference since ultimately we
# only care about x values.
x_matrix_test <- credit_test %>%
  # Create temporary outcome variance just to get model matrix to work:
  mutate(Balance = 1) %>%
  modelr::model_matrix(model_formula, data = .) %>%
  select(-`(Intercept)`) %>%
  as.matrix()

# Fit/train model to training set using arbitrarily chosen lambda = 1-
LASSO_fit_train <- glmnet(x = x_matrix_train, y = credit_train$Balance, alpha = 1, lambda = 10)

# Predict y_hat's for test data using model and same lambda = 10.
credit_test <- credit_test %>%
  mutate(y_hat_LASSO = predict(LASSO_fit_train, newx = x_matrix_test, s = 10)[,1])
credit_test


















#------------------------------------------------------------------------------
# Lec21 original: 2019/04/22
#------------------------------------------------------------------------------
library(tidyverse)
library(glmnet)
library(broom)
library(modelr)
library(ISLR)


# 1. Load Credit data set from ISLR package, convert to tibble, and select subset
# of variables
credit <- ISLR::Credit %>%
  as_tibble() %>%
  select(Balance, Income, Limit, Rating, Student, Cards, Age, Education, Married)
credit


# 2. Define model formula where credit card balance (debt) is the outcome variable
model_formula <-
  "Balance ~ Income + Limit + Rating + Student + Cards + Age + Education + Married" %>%
  as.formula()

# Fit unregularized multiple regression model and output regression table. The
# unregularized beta-hat coefficients are in the estimate column. Recall from
# a previous lecture that this is one "extreme". REMEMBER THESE VALUES!!!
lm(model_formula, data = credit) %>%
  tidy(conf.int = TRUE)

# Recall the other "extreme" is a model that is completely regularized, meaning
# you use none of the predictors, so that y_hat is simply the mean balance.
# REMEMBER THIS VALUE AS WELL!!!
mean(credit$Balance)


# 3. Based on the above model formula, create "model matrix" of the predictor
# variables. Note:
# -the model_matrix() function conveniently converts all categorical predictors
# to numerical ones using one-hot encoding as seen in MP4
# -we remove the first column corresponding to the intercept because it is
# simply a column of ones.
x_matrix <- modelr::model_matrix(model_formula, data = credit) %>%
  select(-`(Intercept)`) %>%
  as.matrix()

# Compare the original data to the model matrix. What is different?
View(credit)
View(x_matrix)


# 4.a) Fit a LASSO model. Note the inputs
# -Instead of inputing a model formula, you input the x_matrix and outcome variable
# -Setting alpha = 1 sets the regularization method to be LASSO. Setting it to be 0
# sets the regularization method to be "ridge regression", another regulization
# method that we don't have time to cover in this class
# -lambda is complexity/tuning parameter whose value we specify. Here let's
# specify 10, an arbitrarily chosen value
LASSO_fit_a <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = 10)
LASSO_fit_a

# Unfortunately the output isn't that informative. Let's use a wrapper function
# that yield a more informative output:
get_LASSO_coefficients <- function(LASSO_fit){
  beta_hats <- LASSO_fit %>%
    broom::tidy(return_zeros = TRUE) %>%
    select(term, estimate, lambda) %>%
    arrange(desc(lambda))
  return(beta_hats)
}

# For that value of lambda = 10, we have the beta-hat coefficients that minimizes
# the equation seen in Lec19 via numerical optimization. Observe how the beta-hats for
# Education and the one-hot encoded MarriedYes variable have been "shrunk" to 0.
get_LASSO_coefficients(LASSO_fit_a)


# 4.b) Fit a LASSO model considering two lambda tuning/complexity parameters at once
# and look at beta-hats
lambda_inputs <- c(10, 1000)
LASSO_fit_b <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = lambda_inputs)
get_LASSO_coefficients(LASSO_fit_b)

# The above output is in tidy/long format, which makes it hard to compare beta-hats
# for both lambda values. Let's convert it to wide and study the beta-hats
get_LASSO_coefficients(LASSO_fit_b) %>%
  spread(lambda, estimate)

# Notice how for the larger lambda, all non-intercept beta-hat have been shrunk
# to 0. This is because lambda = 1000 penalizes model complexity more harshly.


# 4.c) Fit a LASSO model with several lambda tuning/complexity parameters at once
# and look at beta-hats
lambda_inputs <- seq(from = 0, to = 1000)
lambda_inputs
LASSO_fit_c <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = lambda_inputs)

# Since we are now considering several possible values of lambda tuning parameter
# let's visualize instead:
get_LASSO_coefficients(LASSO_fit_c) %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda", y = "beta-hat")

# Observe:
# -For lambda = 0 i.e. no complexity penalization, the values of the beta-hats
# are the unregularized lm() multiple regression values from earlier.
# i.e. it's one extreme
# -At around lambda = 400, all the beta-hat slopes for all our predictor variables
# have been shrunk to 0 and all that remains is the intercept, which is the mean
# y value in our training set $520.01. i.e. the other extreme

# However a typical LASSO plot doesn't show the intercept since it is a beta-hat
# value that is not a candidate to be shrunk to zero, so let's remove it from
# our plot:
get_LASSO_coefficients(LASSO_fit_c) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda", y = "beta-hat")

# Now because of the large magnitude of the beta-hat for StudentYes, it's hard
# to see in what order the beta-hats get shrunk to 0, so let's zoom-in the plot
# a bit
get_LASSO_coefficients(LASSO_fit_c) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda", y = "beta-hat") +
  coord_cartesian(xlim=c(0, 400), ylim = c(-10, 10))

# The results are a little compressed on the left-end of the x-axis, so
# let's rescale the x-axis to be on a log10 scale:
get_LASSO_coefficients(LASSO_fit_c) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda (log10-scale)", y = "beta-hat") +
  coord_cartesian(xlim=c(1, 400), ylim = c(-10, 10)) +
  scale_x_log10()


# 4.d) Fit a LASSO model with a tighter search grid of lambda tuning/complexity
# parameter values, but also space them in powers of 10, and look at beta-hats
lambda_inputs <- 10^seq(from = -5, to = 3, length = 100)
summary(lambda_inputs)
LASSO_fit_d <- glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = lambda_inputs)

# Plot all beta-hats with lambda on log10-scale
LASSO_coefficients_plot <- get_LASSO_coefficients(LASSO_fit_d) %>%
  filter(term != "(Intercept)") %>%
  # Plot:
  ggplot(aes(x = lambda, y = estimate, col = term)) +
  geom_line() +
  labs(x = "lambda (log10-scale)", y = "beta-hat") +
  scale_x_log10()
LASSO_coefficients_plot

# Zoom-in. In what order to the beta-hat slopes get shrunk to 0?
LASSO_coefficients_plot +
  coord_cartesian(ylim = c(-3, 3))


# 5. However, how do we know which lambda value to use? Should we set it to
# yield a less complex vs more complex model? Let's use the glmnet package's
# built in crossvalidation functionality, using the same search grid of
# lambda values:
lambda_inputs <- 10^seq(from = -5, to = 3, length = 100)
LASSO_CV <- cv.glmnet(x = x_matrix, y = credit$Balance, alpha = 1, lambda = lambda_inputs,
                      nfolds = 10, type.measure = "mse")
LASSO_CV

# Alas that output is not useful, so let's broom::tidy() it
LASSO_CV %>%
  broom::tidy()

# What do these mean? For each value of the lambda tuning/complexity parameter,
# let's plot the estimated MSE generated by crossvalidation:
CV_results <- LASSO_CV %>%
  broom::tidy() %>%
  # plot:
  ggplot(aes(x = lambda)) +
  geom_point(aes(y = estimate)) +
  scale_x_log10() +
  labs(x = "lambda (log10-scale)", y = "Estimated MSE")
CV_results

# Which lambda value yields the smallest estimate MSE? It seems like the smallest
# lambda_input value considered: 10^(-5). We can extract this lambda value from
# LASSO_CV:
lambda_star <- LASSO_CV$lambda.min
lambda_star

# But there is another "optimal" lambda: the lambda corresponding to the simplest
# model within 1 standard error of the minimal MSE
lambda_star_1SE <- LASSO_CV$lambda.1se
lambda_star_1SE

# What does this mean? Recall that since we have estimated MSE's, they inherently
# vary. And what value quantifies the how much an estimate varies? The standard
# error. Let's add +/- 1 standard error bars to our estimated MSE's:
CV_results +
  # Add standard error bars:
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), col = "red")

# Zoom-in:
CV_results +
  # Add standard error bars:
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), col = "red") +
  # zoom-in:
  coord_cartesian(xlim=c(10^(-5), 20), ylim = c(9000, 11500))

# Mark the lambda_star and the corresponding estimated MSE + 1 x SE
# with dashed blue lines
CV_results +
  # Add standard error bars:
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), col = "red") +
  # zoom-in:
  coord_cartesian(xlim=c(10^(-5), 20), ylim = c(9000, 11500)) +
  # mark lambda_star and estimated MSE + 1 x SE
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue") +
  geom_hline(yintercept = 9934.387 + 725.7056, linetype = "dashed", col = "blue")

# The idea is:
# -As you go from smaller to larger lambda values, you are considering simpler
# and simpler models
# -While the dashed blue line is the lambda value leading to the smallest
# estimated MSE, you want to account for possible variation in this estimate
# -For example, lambda = 10^-4 and 10^-2 seem to have near identical estimated
# MSE

# Mark the lambda_star_1SE and the corresponding estimated MSE with solid
# blue lines. Notice how the horizontal blue line falls below the dashed horizontal
# line.
CV_results +
  # Add standard error bars:
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), col = "red") +
  # zoom-in:
  coord_cartesian(xlim=c(10^(-5), 20), ylim = c(9000, 11500)) +
  # mark lambda_star and estimated MSE + 1 x SE
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue") +
  geom_hline(yintercept = 9934.387 + 725.7056, linetype = "dashed", col = "blue") +
  # mark lambda_star_1SE and estimated MSE
  geom_vline(xintercept = lambda_star_1SE, col = "blue") +
  geom_hline(yintercept = 10469.25, col = "blue")

# However the next most simple model yielding lambda's estimated MSE falls outside
# the dashed blue line. Let's mark these in green
CV_results +
  # Add standard error bars:
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), col = "red") +
  # zoom-in:
  coord_cartesian(xlim=c(10^(-5), 20), ylim = c(9000, 11500)) +
  # mark lambda_star and estimated MSE + 1 x SE
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue") +
  geom_hline(yintercept = 9934.387 + 725.7056, linetype = "dashed", col = "blue") +
  # mark lambda_star_1SE and estimated MSE
  # geom_vline(xintercept = lambda_star_1SE, col = "blue") +
  # geom_hline(yintercept = 10469.25, col = "blue") +
  # mark next simplest model:
  geom_vline(xintercept = 7.924829, col = "forestgreen") +
  geom_hline(yintercept = 10722.733, col = "forestgreen")


# 6. Find beta-hat coefficients for lambda_star and lambda_star_1SE
LASSO_coefficients_plot +
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue") +
  geom_vline(xintercept = lambda_star_1SE, col = "blue")

# zoom-in:
LASSO_coefficients_plot +
  geom_vline(xintercept = lambda_star, linetype = "dashed", col = "blue") +
  geom_vline(xintercept = lambda_star_1SE, col = "blue") +
  # zoom-in
  coord_cartesian(ylim = c(-3, 3))

get_LASSO_coefficients(LASSO_fit_d) %>%
  filter(lambda == lambda_star) %>%
  filter(estimate != 0)

get_LASSO_coefficients(LASSO_fit_d) %>%
  filter(lambda == lambda_star_1SE) %>%
  filter(estimate != 0)


# 7. Get predictions from f_hat LASSO model using lambda_star_1SE
y_hat <- predict(LASSO_fit_d, newx = x_matrix, s = lambda_star_1SE)[,1]
credit <- credit %>%
  mutate(Balance_hat_LASSO = y_hat)

x_matrix_train <- modelr::model_matrix(model_formula, data = credit_train) %>%
  select(-`(Intercept)`) %>%
  as.matrix()

x_matrix_te <- modelr::model_matrix(model_formula, data = credit_train) %>%
  select(-`(Intercept)`) %>%
  as.matrix()



#------------------------------------------------------------------------------
# Lec14: 2019/03/25
#------------------------------------------------------------------------------
library(tidyverse)
# Pre-process iris dataset
iris <- iris %>%
  # Convert to tibble data frame:
  as_tibble() %>%
  # Add identification variable to uniquely identify each row:
  rownames_to_column(var="ID")


# Fit CART model, in this case for classification
library(rpart)
model_formula <- as.formula(Species ~ Sepal.Length + Sepal.Width)
tree_parameters <- rpart.control(maxdepth = 3)
model_CART <- rpart(model_formula, data = iris, control = tree_parameters)

# Plot CART model
plot(model_CART, margin=0.25)
text(model_CART, use.n = TRUE)
title("Predicting iris species using sepal length & width")
box()


#------------------------------------------------------------------------------
# Exercises with your partner:

# a) If the condition at a given node of the tree evaluates to true, do you go
# down the left branch or the right branch?
# b) Note the bottom-left most "leaf" 44/1/0, corresponds to 44 setosa, 1
# versicolor, 0 virginia, and thus the "majority" winner is setosa. Apply a
# sequence of dplyr commands to the iris data frame to end up with a data frame
# of 44 + 1 + 0 = 45 rows corresponding to these 45 flowers
# c) Read the help file for `rpart.control` and play around with different
# arguments that control the shape of the tree in the tree_parameters object
# above:
tree_parameters_2 <- rpart.control(CHANGE THIS)



# Create training (100 flowers) and test (50 flowers)
set.seed(76)
iris_train <- iris %>%
  sample_frac(2/3)
iris_test <- iris %>%
  anti_join(iris_train, by = "ID")

# 1.a) Fit model to train
model_CART_2 <- rpart(model_formula, data = iris_train, control = tree_parameters)

# 1.b) Plot CART model
plot(model_CART_2, margin = 0.25)
text(model_CART_2, use.n = TRUE)
title("Predicting iris species using sepal length & width")
box()

# 1.c) Get fitted probabilities for each class on train
p_hat_matrix_train <- model_CART_2 %>%
  predict(type = "prob", newdata = iris_train) %>%
  # Convert matrix object to data frame:
  as_tibble()
p_hat_matrix_train

# 1.d) Look at distinct probabilities
p_hat_matrix_train %>%
  distinct()

# 2.a) Apply model to test to get fitted probabilities for each class
p_hat_matrix_test <- model_CART_2 %>%
  predict(type = "prob", newdata = iris_test) %>%
  # Convert matrix object to data frame:
  as_tibble()
p_hat_matrix_test

# 2.b) Instead of fitted probabilities, return fitted y's, where highest
# probability wins and ties are broken at random
y_hat <- model_CART %>%
  predict(type="class", newdata = iris) %>%
  # Function to convert a vector to a data frame
  enframe()
y_hat



# Look at help file for the (multi-class) logarithmic loss function, which is
# one possible "score" for categorical variables when you have more than 2
# categories.
library(yardstick)
?mn_log_loss

# Create a new data frame:
bind_cols(
  # Observed y:
  Species = iris_test$Species,
  # Fitted probabilities for each class
  p_hat_matrix_test
) %>%
  # Compute multi-class log-loss
  mn_log_loss(truth = Species, c(setosa, versicolor, virginica))


#------------------------------------------------------------------------------
# Exercises with your partner:

# d) In 1.d) you saw there are only 3 unique possible 3-tuples (i.e. triplets)
# of fitted probabilties. Which leaf in the tree does each of these 4 possible
# 3-tuples correspond to?

# e) Are larger (multi-class) logarithmic loss function indicative of better
# predictions or worse predictions?





#------------------------------------------------------------------------------
# Solutions

# a) Looking at the top node of the plot of model_CART and going left, there are
# total of 44 + 1 + 0 + 1 + 5 + 1 = 52 flowers in all children leaves. Since
iris %>%
  filter(Sepal.Length < 5.45) %>%
  nrow()
# yields a data frame with 52 rows, if the boolean evaluates to true, then you
# go left

# b) Note there are 0 virginica:
iris %>%
  filter(Sepal.Length < 5.45) %>%
  filter(Sepal.Width >= 2.8) %>%
  count(Species)

# c) Let's set the minsplit to 50 for example
tree_parameters_2 <- rpart.control(minsplit = 100)
model_CART_3 <- rpart(model_formula, data = iris, control = tree_parameters_2)

# Plot CART model. Once there are less than 100 trees at a node, we stop
# splitting
plot(model_CART_3, margin=0.25)
text(model_CART_3, use.n = TRUE)
title("Predicting iris species using sepal length & width")
box()

# d)
p_hat_matrix_train %>%
  distinct()
# First row above is the 32/4/0 leaf, since we have probabilities of
# 32/36 = 0.889, 4/36 = 0.111, 0/36. The winner is setosa
# Second row above is the 1/19/30 leaf, thus the winner is virginica
# Third row above is the 3/11/0 row, thus the winnder is versicolor

# e) Look at: https://cdn-images-1.medium.com/max/1600/0*i2_eUc_t8A1EJObd.png if
# p_ij = 1, the log(p_ij) = 0, and thus sum = 0, and thus the whole thing is 0
# Thus low (multi-class) logarithmic loss function are indicative of good
# predictions


# Figure from Lec
region_labels <- tibble(
  Sepal.Length = c(4.5, 4.5, 7, 5.95, 5.7),
  Sepal.Width = c(4.4, 2.1, 4.4, 4.4, 2.1),
  label = c("R1", "R2", "R5", "R3", "R4")
)

ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width)) +
  geom_jitter(aes(col = Species)) +
  annotate("segment", x = 5.45, xend = 5.45, y = 2, yend = 4.5, size = 1) +
  annotate("segment", x = 4, xend = 5.45, y = 2.8, yend = 2.8, size = 1) +
  annotate("segment", x = 6.15, xend = 6.15, y = 2, yend = 4.5, size = 1) +
  annotate("segment", x = 6.15, xend = 5.45, y = 3.1, yend = 3.1, size = 1) +
  geom_text(data = region_labels, aes(label = label), size = 10) +
  labs(x = "x1: Sepal Length", y = "x2: Sepal Width",
       title = "Jittered scatterplot of CART", col = "y: Species")

ggsave("static/methods/CART/scatterplot.png", width = 16/1.8, height = 9/1.8)







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
# Maybe around 87?

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
#
# Offset 50:
# a) Coefficient = 0.239
# b) Shape: very S-like,

# Offset 10:
# a) Coefficient = 0.0132
# b) Shape: less S-like

# Offset -50:
# a) Coefficient = -0.467
# b) Shape: Very inverse S-like. (looks like a "Z" or "2" instead of "S")

# 4. Challenge question: Change the offset in age to 6.9. Why is the logistic curve
# flat? At what value is it?
financial_distress %>%
  group_by(in_financial_distress) %>%
  summarize(avg_age = mean(age))
# Both groups have the same mean age, so there is no information provided by
# the variable age. The red line is at the total proportion of people in
# financial distress irrespective of group = 0.0668 = 6.68%
financial_distress %>%
  summarize(overall_prop = mean(in_financial_distress))




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
# Lec08 Exercises. Solutions at bottom

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


#------------------------------------------------------------------------------
# Lec10: 2019/03/04
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
# Lec10 Exercises. Solutions at bottom
# 1. Using the visualization above, for what age would you say that there is a

# 2. Compare the visualization above with a scatterplot with:
# a) x = age
# b) y = the observed proportion of individuals in cs_training that are in
# financial

# 3. Change the offset in age to 10 and -50. What do you notice happens to:
# a) the coefficient for age in the regression table.
# b) the shape of the logistic curve of the fitted model?

# 4. Challenge question: Change the offset in age to 6.9. Why is the logistic curve
# flat? At what value is it?


#------------------------------------------------------------------------------
# Lec08 Solutions

# 1. What are the test set RMSEs of Models 1 & 2? Which is higher?
# Being sure to set.seed(76)
predicted_points_1 %>%
  yardstick::rmse(truth = mpg, estimate = .fitted)
predicted_points_2 %>%
  yardstick::rmse(truth = mpg, estimate = .fitted)
# Model 1 = 5.38 < 9.05 = Model 2

# 2. What is the ratio of n/p for our trained Model 2. i.e. the number of points
# in the training set vs the number of predictors
# n = 24, p = 8, thus n/p = 3

# 3. Change the train/test validation ratio from 3:1 to 1:1. What are the RMSEs
# of Models 1 & 2? Which is higher?
# Being sure to set.seed(76)
# Model 1 = 5.05 < 7.89 = Model 2

# 4. What is the new ratio of n/p for our new trained Model 2?
# n = 16, p = 8, thus n/p = 2

# 5. How does the difference in test set RMSE for Model 1 & 2 itself differ when the
# train/test validation ratio went from 3:1 to 1:1
# 5.38 vs 9.05
# 5.05 vs 7.89

# Overfitting is less of a problem when the ratio n/p is smaller

# 6. Try a different combination of variables and see if you can lower your
# RMSE.



#------------------------------------------------------------------------------
# Lec10 Solutions

# 1. Using the visualization above, for what age would you say that there is a
# 50% probability that an individual is in financial distress?
# Maybe around 87?

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
#
# Offset 50:
# a) Coefficient = 0.239
# b) Shape: very S-like,

# Offset 10:
# a) Coefficient = 0.0132
# b) Shape: less S-like

# Offset -50:
# a) Coefficient = -0.467
# b) Shape: Very inverse S-like. (looks like a "Z" or "2" instead of "S")

# 4. Challenge question: Change the offset in age to 6.9. Why is the logistic curve
# flat? At what value is it?
financial_distress %>%
  group_by(in_financial_distress) %>%
  summarize(avg_age = mean(age))
# Both groups have the same mean age, so there is no information provided by
# the variable age. The red line is at the total proportion of people in
# financial distress irrespective of group = 0.0668 = 6.68%
financial_distress %>%
  summarize(overall_prop = mean(in_financial_distress))






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
