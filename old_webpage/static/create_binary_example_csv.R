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


library(reprex)

library(tidyverse)
library(yardstick)

data("two_class_example")
roc_auc(two_class_example, truth, Class1)



roc_curve(two_class_example, truth, Class1) %>%
  autoplot()

two_class_example <- two_class_example %>%
  as_tibble()

two_class_example %>%
  roc_auc(truth = truth, Class1)

two_class_example <- two_class_example %>%
  mutate(truth = fct_relevel(truth, "Class2"))

two_class_example %>%
  select(-Class2) %>%
  mutate(Class1 = 1-Class1) %>%
  roc_auc(truth = truth, Class1)


values <- read_csv("https://rudeboybert.github.io/SDS293/static/methods/ROC/example.csv") %>%
  mutate(y = factor(y))

roc_curve(values, truth = y, p_hat) %>%
  autoplot()

values <- values %>%
  mutate(y = fct_relevel(y, "0"))

