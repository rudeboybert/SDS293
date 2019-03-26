library(okcupiddata)
library(tidyverse)
library(here)

profiles <- profiles %>%
  as_tibble()

glimpse(profiles)

example <- profiles %>%
  filter(height >= 50, height <= 85) %>%
  filter(!is.na(height)) %>%
  mutate(y = ifelse(sex == "f", 1, 0)) %>%
  mutate(ID = 1:n()) %>%
  mutate(x_categ = recode_factor(
    orientation,
    "straight" = "grape",
    "gay" = "orange",
    "bisexual" = "apple")
  ) %>%
  select(ID, y, x_num = height, x_categ) %>%
  sample_n(2000)

write_csv(example, path = here("static", "binary_example.csv"))
