library(tidyverse)
library(broom)
library(gridExtra)

# Plot 1: Define truth -------------------------------------------------
# Let these be the unknowns:
# -the true function f(x) i.e. the signal
# -the standard deviation of the true epsilon i.e. the noise
f <- function(x) {
  x^2
}
sigma <- 0.3

# This is the target point we'll be trying to predict: (0.95, f(0.95))
x0 <- 0.95
target <- data_frame(x=x0, y=f(x0))

# Base plot:
baseplot <- ggplot(data=NULL) +
  geom_vline(xintercept = x0, linetype="dashed") +
  geom_point(data=target, aes(x=x,y=y), col="red", size=3) +
  coord_cartesian(xlim=c(0, 1), ylim=c(-0.75, 1.5))

# We will later compare splines fits with df=2 and 99 using these:
plot_df_2 <- baseplot +
  labs(title="Degrees of freedom = 2")
plot_df_99 <- baseplot +
  labs(title="Degrees of freedom = 99")
arrangeGrob(plot_df_2, plot_df_99, nrow=2)

plot1 <- baseplot +
  stat_function(data=data_frame(x=c(0,1)), aes(x=x), fun=f, col="red") +
  labs(title="Plot 1: True f(x) + Target point to predict (0.95, f(0.95))")
plot1



# Plot 2: Generated new sampled points ----------------------------------
# This function generates a random sample. Random in terms of both
# -the predictor x (uniform on [0,1])
# -the amount of noise eps (normal with mean=0 & sd=sigma=0.3)
# Think of this as a "get new data" function:
generate_sample <- function(f, sample_size, sigma) {
  sample <- data_frame(
    x = runif(n = sample_size, min = 0, max = 1),
    f_x = f(x),
    epsilon = rnorm(n = sample_size, mean = 0, sd = sigma),
    y = f_x + epsilon
  )
  # Recall: We don't observe f(x) and epsilon, just (x, y)
  sample <- sample %>% 
    select(x, y)
  
  return(sample)
}

# Sample n=100 points
sampled_points <- generate_sample(f=f, sample_size=100, sigma)
sampled_points

plot2a <- baseplot +
  stat_function(data=data_frame(x=c(0,1)), aes(x=x), fun=f, col="red") +
  geom_point(data = sampled_points, aes(x=x, y=y)) +
  labs(title="Plot 2a: Generate sample of n=100 new points")
  
plot2b <- baseplot +
  geom_point(data = sampled_points, aes(x=x, y=y)) +
  labs(title="Plot 2b: Recall in practice we won't know true f(x)")

plot2 <- arrangeGrob(plot2a, plot2b, nrow=2)
plot2



# Plot 3: Based on sampled_points fit splines with df=2 and df=99 ------------

# Fit splines with df=2 and add to plot
fitted_df_2 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=2) %>%
  augment()
plot_df_2 <- baseplot +
  geom_point(data=fitted_df_2, aes(x=x, y=y)) +
  geom_line(data=fitted_df_2, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 3: Spline fit w/ df = 2")

# Fit splines with df=99 and add to plot
fitted_df_99 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=99) %>%
  augment()
plot_df_99 <- baseplot +
  geom_point(data=fitted_df_99, aes(x=x, y=y)) +
  geom_line(data=fitted_df_99, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 3: Spline fit w/ df = 99")

# Superimpose red dot
plot_df_2 <- plot_df_2 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)
plot_df_99 <- plot_df_99 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)

plot3 <- arrangeGrob(plot_df_2, plot_df_99, nrow=2)
plot3



# Plot 4: Sample 100 new points and do this again ---------------
sampled_points <- generate_sample(f=f, sample_size=100, sigma)

# Fit splines with df=2 and add to plot
fitted_df_2 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=2) %>%
  augment()
plot_df_2 <- plot_df_2 +
  geom_point(data=fitted_df_2, aes(x=x, y=y)) +
  geom_line(data=fitted_df_2, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 4: New spline fit w/ df = 2 based on new sample of size n=100")

# Fit splines with df=99 and add to plot
fitted_df_99 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=99) %>%
  augment()
plot_df_99 <- plot_df_99 +
  geom_point(data=fitted_df_99, aes(x=x, y=y)) +
  geom_line(data=fitted_df_99, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 4: New spline fit w/ df = 99 based on new sample of size n=100")

# Superimpose red dot
plot_df_2 <- plot_df_2 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)
plot_df_99 <- plot_df_99 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)

plot4 <- arrangeGrob(plot_df_2, plot_df_99, nrow=2)
plot4



# Plot 5: Do this again ---------------
sampled_points <- generate_sample(f=f, sample_size=100, sigma)

# Fit splines with df=2 and add to plot
fitted_df_2 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=2) %>%
  augment()
plot_df_2 <- plot_df_2 +
  geom_point(data=fitted_df_2, aes(x=x, y=y)) +
  geom_line(data=fitted_df_2, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 5: New spline fit w/ df = 2 based on new sample of size n=100 again")

# Fit splines with df=99 and add to plot
fitted_df_99 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=99) %>%
  augment()
plot_df_99 <- plot_df_99 +
  geom_point(data=fitted_df_99, aes(x=x, y=y)) +
  geom_line(data=fitted_df_99, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 5: New spline fit w/ df = 99 based on new sample of size n=100 again")

# Superimpose red dot
plot_df_2 <- plot_df_2 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)
plot_df_99 <- plot_df_99 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)

plot5 <- arrangeGrob(plot_df_2, plot_df_99, nrow=2)
plot5



# Plot 6: And again ---------------
sampled_points <- generate_sample(f=f, sample_size=100, sigma)

# Fit splines with df=2 and add to plot
fitted_df_2 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=2) %>%
  augment()
plot_df_2 <- plot_df_2 +
  geom_point(data=fitted_df_2, aes(x=x, y=y)) +
  geom_line(data=fitted_df_2, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 6: New spline fit w/ df = 2 based on new sample of size n=100 again")

# Fit splines with df=99 and add to plot
fitted_df_99 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=99) %>%
  augment()
plot_df_99 <- plot_df_99 +
  geom_point(data=fitted_df_99, aes(x=x, y=y)) +
  geom_line(data=fitted_df_99, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 5: New spline fit w/ df = 99 based on new sample of size n=100 again")

# Superimpose red dot
plot_df_2 <- plot_df_2 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)
plot_df_99 <- plot_df_99 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)

plot6 <- arrangeGrob(plot_df_2, plot_df_99, nrow=2)
plot6



# Plot 7: And again ---------------
sampled_points <- generate_sample(f=f, sample_size=100, sigma)

# Fit splines with df=2 and add to plot
fitted_df_2 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=2) %>%
  augment()
plot_df_2 <- plot_df_2 +
  geom_point(data=fitted_df_2, aes(x=x, y=y)) +
  geom_line(data=fitted_df_2, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 7: New spline fit w/ df = 2 based on new sample of size n=100 again")

# Fit splines with df=99 and add to plot
fitted_df_99 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=99) %>%
  augment()
plot_df_99 <- plot_df_99 +
  geom_point(data=fitted_df_99, aes(x=x, y=y)) +
  geom_line(data=fitted_df_99, aes(x=x, y=.fitted), col="blue", size=1) + 
  labs(title="Plot 7: New spline fit w/ df = 99 based on new sample of size n=100 again")

# Superimpose red dot
plot_df_2 <- plot_df_2 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)
plot_df_99 <- plot_df_99 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)

plot7 <- arrangeGrob(plot_df_2, plot_df_99, nrow=2)
plot7




# Plot 8: Do this 50 times
plot_df_2 <- baseplot +
  labs(title="Plot 8: 50 different spline fits w/ df = 2 based on 50 different samples of size n = 100")
plot_df_99 <- baseplot +
  labs(title="Plot 8: 50 different spline fits w/ df = 99 based on 50 different samples of size n = 100")
for(i in 1:50) {
  sampled_points <- generate_sample(f, sample_size = 100, sigma)

  # Fit splines with df=2 and add to plot
  fitted_df_2 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=2) %>%
    augment()
  plot_df_2 <- plot_df_2 +
    geom_point(data=fitted_df_2, aes(x=x, y=y), alpha=0.1) +
    geom_line(data=fitted_df_2, aes(x=x, y=.fitted), col="blue", alpha=0.1, size=1)
  
  # Fit splines with df=99 and add to plot
  fitted_df_99 <- smooth.spline(x=sampled_points$x, y=sampled_points$y, df=99) %>%
    augment()
  plot_df_99 <- plot_df_99 +
    geom_point(data=fitted_df_99, aes(x=x, y=y), alpha=0.1) +
    geom_line(data=fitted_df_99, aes(x=x, y=.fitted), col="blue", alpha=0.1, size=1)
}

# Superimpose red dot
plot_df_2 <- plot_df_2 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)
plot_df_99 <- plot_df_99 +
  geom_point(data=data_frame(x=x0, y=f(x0)), aes(x=x,y=y), col="red", size=3)

plot8 <- arrangeGrob(plot_df_2, plot_df_99, nrow=2)
plot8


# Save to pdf
ggsave("plot1.pdf", plot1, width=11*0.8, height=8.5*0.8)
ggsave("plot2.pdf", plot2, width=11*0.8, height=8.5*0.8)
ggsave("plot3.pdf", plot3, width=11*0.8, height=8.5*0.8)
ggsave("plot4.pdf", plot4, width=11*0.8, height=8.5*0.8)
ggsave("plot5.pdf", plot5, width=11*0.8, height=8.5*0.8)
ggsave("plot6.pdf", plot6, width=11*0.8, height=8.5*0.8)
ggsave("plot7.pdf", plot7, width=11*0.8, height=8.5*0.8)
ggsave("plot8.pdf", plot8, width=11*0.8, height=8.5*0.8)
system("'/System/Library/Automator/Combine PDF Pages.action/Contents/Resources/join.py' -o plots.pdf *.pdf")
paste("plot", 1:8, ".pdf", sep="") %>% 
  paste("rm", ., "; ", collapse = " ") %>% 
  system()


