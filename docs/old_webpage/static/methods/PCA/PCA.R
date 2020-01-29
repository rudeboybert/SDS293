library(tidyverse)
library(raster)

# Changes this directory
faces <- read_csv("faces.csv", col_names = FALSE) %>% 
  as.matrix()

# The data consists of 5000 images, each of size 32 x 32 greyscale values
# between -128 and 128. In other words each image is represented as a vector of 
# length 32 x 32 = 1024
dim(faces)

# Let's plot the 53rd image for example
faces[53,] %>% 
  matrix(nrow=32) %>% 
  raster() %>% 
  plot(col = gray.colors(256, start = 0, end = 1, gamma = 2.2, alpha = NULL), 
       main = "Original Pic")


# Compute all principal components using built-in R function, and not raw
# linear algebra commands like in the state education example
pca <- prcomp(faces, center = FALSE)

# Set the number of principal components to use (out of a possible 1024) and
# let's continue to focus on the 53rd image
num_pc <- 1
picture_number <- 200

# Run lines 31 thru 44:
par(mfrow=c(1,2))
compressed <- pca$x[, 1:num_pc] %*% t(pca$rotation[, 1:num_pc])
# Original picture
faces[picture_number,] %>% 
  matrix(nrow=32) %>% 
  raster() %>% 
  plot(col = gray.colors(256, start = 0, end = 1, gamma = 2.2, alpha = NULL), 
       main="Original Pic")
# Compressed picture
compressed[picture_number,] %>% 
  matrix(nrow=32) %>% 
  raster() %>% 
  plot(col = gray.colors(256, start = 0, end = 1, gamma = 2.2, alpha = NULL), 
       main = paste(num_pc, "PC Compressed Pic"))

# Now try increasing the num_pc and see at which point the compressed image
# starts looking like the original image. Try lines 31-44 for different num_pc,
# but also different picture_numbers


# When is num_pc high enought? Let's compute the proportion of the total 
# variance of the original 1024 dimensions explained:
faces_recentered <- apply(faces, 2, function(x){x-mean(x)})
eigen <- cov(faces_recentered) %>%
  as.matrix() %>%
  eigen()
Gamma <- eigen$vectors
Y <- faces_recentered %*% Gamma
var_Y <- cov(Y) %>% diag()
prop_variance_explained <- data_frame(
  n = 1:length(var_Y),
  prop_var = cumsum(var_Y)/sum(var_Y)
)

# We see that after 332 principal components, 99% of the total variation is 
# explained!
prop_variance_explained %>% 
  filter(prop_var > 0.99)

# Plot this.
ggplot(prop_variance_explained, aes(x=n, y=prop_var)) +
  geom_line() +
  labs(x="Number of principal components used", y="Proportion of total variance explained") +
  geom_hline(yintercept = 0.99, linetype="dashed") +
  geom_vline(xintercept = 332, linetype="dashed")

# Moral: Using only 332 variables, we can capture over 99% of the total
# variation in faces across the 1024 pixels.

# Source:
# https://www.r-bloggers.com/image-compression-with-principal-component-analysis/

