library(tidyverse)

# Download train https://www.kaggle.com/c/digit-recognizer/data
digits_orig <- read_csv("~/Downloads/train.csv")

# Only consider subset of all training data
digits <- digits_orig %>% 
  sample_n(10000)

# See how data is formatted
digits %>% 
  slice(1:5) %>% 
  View()



# Plot digits -------------------------------------------------------------
# (Hack) function to plot digit for a given row
plot_digit <- function(digits_data, rownumber){
  pixel_darkness <- digits_data %>% 
    slice(rownumber) %>% 
    select(starts_with("pixel")) %>% 
    as.numeric()
  
  digit <- data_frame(
    x = rep(28:1, each=28),
    y = rep(1:28, times=28),
    pixel_darkness = pixel_darkness
  ) %>% 
    mutate(
      x = as.factor(x),
      y = as.factor(y)
    )
  ggplot(digit, aes(x=x, y=y, fill=pixel_darkness)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black") +
    coord_flip() +
    labs(title=paste("True digit =", digits_data$label[rownumber])) + 
    guides(fill=FALSE) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

# Plot
plot_digit(digits, 815)



# Cluster into k groups/clusters -------------------------------------------
# We don't train on label (true digit), only pixel values
train <- digits %>% 
  select(-label)

# k-means clustering. we set the seed value to get replicable results.
k <- 10
set.seed(76)
# Why 76? # https://www.youtube.com/watch?v=xjJ7FheCkCU
results <- kmeans(train, k, nstart = 20)

# These are the group labels. But do these labels correspond to the digits 0
# thru 9?
results$cluster %>% table()

# Add resulting groups to original data frame
digits <- digits %>% 
  mutate(cluster = results$cluster) %>% 
  select(label, cluster, everything())



# Investigation -----------------------------------------------------------
# Let's focus on group/cluster == 1. What true digit do you think this corresponds
# to?
digits %>% 
  filter(cluster == 1) %>% 
  select(label, cluster) %>% 
  View()

# group label 4
digits %>% 
  filter(cluster == 1) %>% 
  count(label)


# Based on these results, which clusters are the "purest"?
digits %>% 
  mutate(label=as.factor(label), cluster=as.factor(cluster)) %>% 
  group_by(label, cluster) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=cluster, y=label, fill=n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x="(Assigned) cluster", y="True digit",fill="Count", title="(Assigned) cluster vs true digit") +
  theme_classic()
ggsave("images/digits.png", width=16/2.5, height=16/2.5)

