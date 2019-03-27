library(tidyverse)
library(yardstick)
library(broom)

# Newsflash! Your professors are not perfect! In my case, I don't even aspire to
# be. I should've made you do this in MP3: Exploratory data analysis!

example <- read_csv("https://rudeboybert.github.io/SDS293/static/binary_example.csv")

# 0. Always look at your data first!
glimpse(example)

# Observe:
# - We have 2000 observations!
# - Even though y is binary categorical, it is "encoded" as a numerical
# with values 0 & 1. This is the source of much confusion.

# 1.a) Distribution of outcome variable y. While histograms show the distribution of a
# numerical variable, barplots show distribution of a categorical variable.
# Note that we convert y to a factor just before plotting.
ggplot(example, aes(x = as.factor(y))) +
  geom_bar() +
  labs(x = "Outcome variable y",
       title = "Distribution of categorical outcome")

# QUESTION: What proportion of 1's do we have? In other words, if we randomly
# selected someone from dataset, what is the probability they have y = 1

# 1.b) Distribution of numerical predictor
ggplot(example, aes(x = x_num)) +
  geom_histogram(binwidth = 1)

# 1.c) Distribution of categorical predictor
ggplot(example, aes(x = x_categ)) +
  geom_bar()



# 2.a) Relationship of categorical outcome variable y and categorical predictor
# x. To show the relationship between two categorical outcome variables, one
# approach is a stacked barchart!
ggplot(example, aes(x = x_categ, fill = as.factor(y))) +
  geom_bar() +
  labs(x = "Categorical x", fill = "Outcome y")

# QUESTION: What proportion of 1's do we have for each category? In other words, if we randomly
# selected someone from each category?, what is the probability they have y = 1?

# This is a little hard to tell because the bars have different heights. Let's
# normalize the bar heights:
ggplot(example, aes(x = x_categ, fill = as.factor(y))) +
  geom_bar(position = "fill") +
  labs(x = "Categorical x", fill = "Outcome y") +
  geom_hline(yintercept = 0.397, col = "blue")

# QUESTION: What proportion of 1's do we have for each category? In other words,
# if we randomly selected someone from each category?, what is the probability
# they have y = 1?


# 2.b) Relationship of categorical outcome variable y and numerical predictor x.
# Here we plot y as a numerical, so that we can later add probabilities on the
# y-axis
ggplot(data = example, aes(x = x_num, y = y)) +
  geom_point()

# Oh no! Overplotting! Let's add a little random jitter to break up the ties!
y_vs_x_num_plot <- ggplot(data = example, aes(x = x_num, y = y)) +
  geom_jitter(height = 0.01, width = 0.05)
y_vs_x_num_plot


# QUESTION: What proportion of 1's do we have for people with x = 70? In other words,
# if we randomly selected someone with x = 70?, what is the probability
# they have y = 1?

# This is a little hard to tell because there are a lot of points at x = 70.
# Logistic regression to the rescue!
model <- glm(y ~ x_num, family = "binomial", data = example)

# Note the coefficient for x_num is negative:
model %>%
  tidy(conf.int = TRUE)

# Add fitted probabilities to plot!
fitted_points <- model %>%
  augment() %>%
  mutate(fitted_prob = 1/(1 + exp(-.fitted)))

y_vs_x_num_plot +
  geom_line(data = fitted_points, mapping = aes(y = fitted_prob), col = "red", size = 1) +
  labs(x = "Numerical x", y = "Outcome y") +
  geom_hline(yintercept = 0.397, col = "blue")

# NOW ANSWER QUESTION: What proportion of 1's do we have for people with x = 70?
# In other words, if we randomly selected someone with x = 70?, what is the
# probability they have y = 1?



# ROC curve. The example in the help file is a little sparse:
data("two_class_example")
roc_auc(two_class_example, truth, Class1)

# Let's amplify. First convert the two_class_example to a tibble so we can
# look at it easier
two_class_example <- two_class_example %>%
  as_tibble()
two_class_example

# Let's drill down on the outcome variable y in this case:
two_class_example$truth

# Notice the levels: Class1 comes first, Class2 comes second. So Class1 is the
# binary outcome of interest

# Great, what are the arguments to roc_auc?
# data and truth are obvious, but what about ...?
# A set of unquoted column names or one or more dplyr selector functions to
# choose which variables contain the class probabilities. If truth is binary,
# only 1 column should be selected. Otherwise, there should be as many columns
# as factor levels of truth.

# In other words, we should feed 1 column for the probabilities of Class1
roc_auc(two_class_example, truth = truth, Class1)

# In our case
fitted_points <- fitted_points %>%
  mutate(y_as_factor = factor(y, levels = c("1", "0")))

fitted_points %>%
  roc_auc(y_as_factor, fitted_prob)

fitted_points %>%
  roc_curve(y_as_factor, fitted_prob) %>%
  autoplot()





library(tidyverse)
library(yardstick)
library(broom)

# Load example:
example <- read_csv("https://rudeboybert.github.io/SDS293/static/binary_example.csv")

# Input to glm() should be with y as a numerical variable. Do not convert
# y into a factor yet.
model <- glm(y ~ x_num, family = "binomial", data = example)

# Compute fitted probabilities p-hat:
fitted_points <- model %>%
  augment() %>%
  mutate(p_hat = 1/(1 + exp(-.fitted))) %>%
  select(y, x_num, p_hat)

# Plot with y as numerical so that we can compare it with p-hats. Do not convert
# y into a factor yet.
ggplot(fitted_points, aes(x = x_num)) +
  geom_jitter(aes(y = y), height = 0.01, width = 0.05) +
  geom_line(aes(y = p_hat), col = "red")

# Get AUC via roc_auc()
# -first argument (that is piped) is the data frame
# -second is truth, which is the outcome variable
# -third argument are the fitted probabilities corresponding to the binary
# outcome of interest
fitted_points %>%
  roc_auc(truth = y, p_hat)

# That doesn't work since y is numerical. So now:
# -Convert y into a factor/categorical variable
# -Set the first level of categorical variable to be "1" using
# fct_relevel() from the forcats package (Note there are many ways to do this)
fitted_points %>%
  mutate(
    y_as_factor = factor(y),
    y_as_factor = fct_relevel(y_as_factor, "1")
  ) %>%
  roc_auc(truth = y_as_factor, p_hat)




library(yardstick)
library(dplyr)
data("two_class_example")
two_class_example <- two_class_example %>%
  as_tibble()

options(yardstick.event_first = FALSE)

# First factor level is assumed to be the event: Class1
levels(two_class_example$truth)

# AUC using Class1 probabilties
two_class_example %>%
  roc_auc(truth, Class1)

# AUC using Class2 probabilties
two_class_example %>%
  roc_auc(truth, Class2)

options(yardstick.event_first = TRUE)

# First factor level is assumed to be the event: Class1
levels(two_class_example$truth)

# AUC using Class1 probabilties
two_class_example %>%
  roc_auc(truth, Class1, estimator = "binary")

# AUC using Class2 probabilties
two_class_example %>%
  roc_auc(truth, Class2, estimator = "binary")






# Change first factor level to be Class2
two_class_example <- two_class_example %>%
  mutate(truth = fct_relevel(truth, "Class2"))
levels(two_class_example$truth)

two_class_example %>%
  roc_auc(truth, Class2)
