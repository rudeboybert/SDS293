library(tidyverse)
library(proxy)
fig_width <- 11*0.8
fig_height <- 8.5*0.8

set.seed(79)
k <- 2
points <- data_frame(
  x = c(runif(25), runif(25, min=1, max=2)),
  y = c(runif(25), runif(25, min=1, max=2)),
  cluster = as.factor(sample(2, size=50, replace=TRUE))
)

ggplot(NULL, aes(x=x, y=y)) +
  geom_point(data=points) +
  labs(x="X1", y="X2", title="50 Random Points") +
  coord_equal()
ggsave("static/k_means/k_means1.pdf", width = fig_width, height = fig_height)


# ggplot(NULL, aes(x=x, y=y)) +
#   geom_point(data=points, aes(col=cluster)) +
#   labs(x="X1", y="X2", title="Randomly Assign 50 Points to 2 Clusters") +
#   coord_equal()
# ggsave("static/k_means/k_means2.pdf", width = fig_width, height = fig_height)


centroids <- data_frame(
  cluster = as.factor(1:2),
  x = runif(k, min=0, max=2),
  y = runif(k, min=0, max=2)
)
ggplot(NULL, aes(x=x, y=y)) +
  geom_point(data=points) +
  geom_point(data=centroids, aes(col=cluster), size=5) +
  labs(x="X1", y="X2", title="Randomly Drop Two Cluster Centers") +
  coord_equal()
ggsave("static/k_means/k_means3.pdf", width = fig_width, height = fig_height)





D <- proxy::dist(x=points[, c("x","y")], y=centroids[, c("x","y")])
points <- points %>%
  mutate(cluster = as.factor(apply(D, 1, which.min)))
ggplot(NULL, aes(x=x, y=y)) +
  geom_point(data=points, aes(col=cluster)) +
  geom_point(data=centroids, aes(col=cluster), size=5) +
  labs(x="X1", y="X2", title="Reassign Points Based on Proximity to Cluster Centers") +
  coord_equal()
ggsave("static/k_means/k_means4.pdf", width = fig_width, height = fig_height)


centroids <- points %>%
  group_by(cluster) %>%
  summarise(x=mean(x), y=mean(y))
ggplot(NULL, aes(x=x, y=y)) +
  geom_point(data=points, aes(col=cluster)) +
  geom_point(data=centroids, aes(col=cluster), size=5) +
  labs(x="X1", y="X2", title="Recenter the Cluster Centers") +
  coord_equal()
ggsave("static/k_means/k_means5.pdf", width = fig_width, height = fig_height)


D <- proxy::dist(x=points[, c("x","y")], y=centroids[, c("x","y")])
points <- points %>%
  mutate(cluster = as.factor(apply(D, 1, which.min)))
ggplot(NULL, aes(x=x, y=y)) +
  geom_point(data=points, aes(col=cluster)) +
  geom_point(data=centroids, aes(col=cluster), size=5) +
  labs(x="X1", y="X2", title="Reassign Points Based on Proximity to Cluster Centers") +
  coord_equal()
ggsave("static/k_means/k_means6.pdf", width = fig_width, height = fig_height)


centroids <- points %>%
  group_by(cluster) %>%
  summarise(x=mean(x), y=mean(y))
ggplot(NULL, aes(x=x, y=y)) +
  geom_point(data=points, aes(col=cluster)) +
  geom_point(data=centroids, aes(col=cluster), size=5) +
  labs(x="X1", y="X2", title="Recenter the Cluster Centers") +
  coord_equal()
ggsave("static/k_means/k_means7.pdf", width = fig_width, height = fig_height)
