library(tidyverse)
library(caret)
library(gridExtra)

# Fit model ---------------------------------------------------------------
data(iris)
iris <- iris %>%
  as_tibble() %>%
  # Add ID column:
  mutate(ID = 1:n()) %>% 
  select(ID, Species, Sepal.Length, Sepal.Width)

# What data looks like. Note: points jittered to break overplotting
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_text(aes(label=ID, col=Species))

# Fit knn(k) model
k <- 3
model_formula <- as.formula(Species ~ Sepal.Length + Sepal.Width)
model_knn <- caret::knn3(model_formula, data=iris, k = k)


# Possible output 1: Majority rules vote winner ---------------------------
y_hat <- model_knn %>% 
  predict(newdata=iris, type=c("class"))
y_hat




# Possible output 2: Keep fitted probabilities ----------------------------
# In other words, don't hold the vote!
p_hat_matrix <- model_knn %>% 
  predict(newdata=iris, type=c("prob", "class")) %>% 
  round(3)
View(p_hat_matrix)

# What's going on in row 64? Isn't this k=3 nearest neighbors?
p_hat_matrix[64,]
# Let's zoom in. What's going on?
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_text(aes(label=ID, col=Species)) +
  coord_cartesian(xlim=c(6, 6.2), ylim=c(2.8, 3.0))

# Convert to tidy data format and plot
p_hat_tidy <- p_hat_matrix %>% 
  as_tibble() %>% 
  bind_cols(iris, .) %>% 
  tidyr::gather(fitted_class, fitted_prob, -c(ID, Species, Sepal.Length, Sepal.Width)) %>% 
  arrange(ID, Species)
p_hat_tidy

# Here:
# -the color of the points represent the true outcome y
# -the facets contain fitted probabilities for each of the 3 levels
# -the shade of the points represent the fitted probability of the species in
# the facet
p_hat_plot <- ggplot(p_hat_tidy, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(col=Species, alpha=fitted_prob)) + 
  facet_wrap(~fitted_class) +
  labs(x="Sepal Length", y="Sepal Width", alpha="Fitted Prob", title="Fitted probabilities")
p_hat_plot

# Recall the truth:
truth_plot <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(col=Species)) +
  labs(x="Sepal Length", y="Sepal Width", alpha="Fitted Prob", title="True outcomes")
truth_plot

# Compare
layout <- matrix(c(1,1,2,2,2), nrow=1)
grid.arrange(truth_plot, p_hat_plot, ncol=2, layout_matrix = layout)




library(MLmetrics)

# Possible output 1: Majority rules vote winner ---------------------------
iris$y_hat <- y_hat

# Proportion correct:
mean(iris$Species == iris$y_hat)

# Specific breakdown of correct/incorrect (done two ways):
ftable(iris$Species, iris$y_hat)
# or
iris %>% 
  group_by(Species, y_hat) %>% 
  count() %>% 
  mutate(correct = Species == y_hat)



# Possible output 2: Keep fitted probabilities ----------------------------
MLmetrics::MultiLogLoss(y_true = iris$Species, y_pred = fitted_probs)