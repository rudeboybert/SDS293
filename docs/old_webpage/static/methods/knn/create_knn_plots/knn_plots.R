# Code based on: http://stackoverflow.com/questions/31234621/
library(class)
library(dplyr)
library(ggplot2)

# Stuff for making plots:
fig_width <- 11*0.8
fig_height <- 8.5*0.8
# Color palette chosen from:
# http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
color_palette <- "Dark2"

# We set the random number generator seed value to get replicable randomness
set.seed(76)

# Load the iris data and consider
# -two predictors variables: sepal length & width
# -outcome variable: species
data("iris")
iris <- iris %>%
  tbl_df() %>% 
  select(Sepal.Length, Sepal.Width, Species)
View(iris)



# Define training set -----------------------------------------------------
# Let's use the entire iris data set as our train data
train <- iris
ggplot(train, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + 
  geom_point() + 
  scale_color_brewer(palette = color_palette) +
  labs(title="Training Data")
ggsave("assets/classification/train1.pdf", width = fig_width, height = fig_height)

# Unfortunately there is a little overplotting. So we use geom_jitter to add a
# little random noise so we can see situations when points are plotted on top
# of each other:
ggplot(train, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + 
  geom_jitter(width = 0.05, height = 0.05) + 
  scale_color_brewer(palette = color_palette)  +
  labs(title="Training Data with Random Noise Added")
ggsave("assets/classification/train2.pdf", width = fig_width, height = fig_height)



# Define test set ---------------------------------------------------------
# We'll define the test set to be a grid of values that encompasses the range
# of both sepal length and width

# Actual range of values:
range(train$Sepal.Length)
# Desired grid width:
range(train$Sepal.Length) + c(-1, 1)

# Actual range of values:
range(train$Sepal.Width)
# Desired grid width:
range(train$Sepal.Width) + c(-1, 1)

# We define our grid using expand.grid(). We will make predictions for each of 
# these points
test <- expand.grid(
  Sepal.Length = seq(3.3, 8.7, by=0.1),
  Sepal.Width = seq(1, 5.4, by=0.1)
) %>% 
  tbl_df()

ggplot(test, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(size=0.5) +
  labs(title="Test Data (Grid)")
ggsave("assets/classification/test.pdf", width = fig_width, height = fig_height)




# Fit model & get predictions --------------------------------------------------
# Define number of neighbors:
num_neigh <- 3

# Assign categorical outcome variable:
classifications <- train$Species

# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables
train_input <- train %>%
  select(Sepal.Length, Sepal.Width)
test_input <- test %>% 
  select(Sepal.Length, Sepal.Width)

# Fit model:
model_knn <- 
  class::knn(train=train_input, test=test_input, cl=classifications, k = num_neigh, prob=TRUE)

# Add predictions and probabilities to test set
test <- test %>% 
  mutate(
    Species = model_knn,
    Probability = attr(model_knn, "prob")
)




# Basic plots ------------------------------------------------------------
# The larger points represent the training data, the smaller points the
# test data (i.e. the grid)
ggplot(test, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + 
  geom_point(size=1) +
  geom_jitter(data=train, size=3) + 
  scale_color_brewer(palette = color_palette)  +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Points"))
ggsave("assets/classification/knn1.pdf", width = fig_width, height = fig_height)

# The following is the same plot as above, but using geom_tile() instead of
# geom_point() to denote the prediction for each grid point:
ggplot(test, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_tile(aes(fill=Species), alpha=0.5) +
  geom_jitter(data=train, aes(col=Species), size=3) + 
  scale_fill_brewer(palette = color_palette)  +
  scale_color_brewer(palette = color_palette)  +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Tiles"))
ggsave("assets/classification/knn2.pdf", width = fig_width, height = fig_height)



# Plots that incorporate uncertainty --------------------------------------
# The above isn't great. I like the following plot most. Note the training set 
# data are denoted with points with:
# -shape=21 are points with an outline color and a fill color
# -stroke controls the size of the outline
ggplot(test, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_tile(aes(fill=Species, alpha=Probability)) +
  geom_jitter(data=train, aes(fill=Species), size=3, shape=21, stroke=0.5) + 
  scale_fill_brewer(palette = color_palette)  +
  labs(title=paste("k =", num_neigh, "Nearest Neighbors with Prediction Tiles"))
ggsave("assets/classification/knn3.pdf", width = fig_width, height = fig_height)
