## tracey mangin
## april 14, 2023
## cookie model

library(tidyverse)
library(here)

## files
clean_response_file <- c("clean_responses.csv")

## read in cleaned data
clean_df <- read_csv(here("outputs", "processed", clean_response_file))


## look up table
recipe_lut <- tibble(preference = c("Cookie A", "Cookie B", "Cookie C", "Cookie D", "No preference"),
                     recipe = c("LA Times: chips", "NYT: chips", "LA Times: chip-less", "NYT: chip-less", "No preference"),
                     recipe_general = c("LA Times", "NYT", "LA Times", "NYT", "No preference"))

## process
## -----------------------------
cookie_model_df <- clean_df %>%
  select(emlab, age, fave_choc_chip, fave_chipless, fave_overall, hours) %>%
  pivot_longer(fave_choc_chip:fave_overall, names_to = "category", values_to= "preference") %>%
  left_join(recipe_lut) %>%
  mutate(cat_label = ifelse(category == "fave_chipless", "Chip-less",
                            ifelse(category == "fave_choc_chip", "Chocolate chip", "Overall"))) %>%
  select(cat_label, recipe_general, hours, emlab) %>%
  # mutate(emlab = ifelse(emlab == "Yes", 1, 0)) %>%
  mutate(emlab = ifelse(emlab == "Yes", "emLab", "non-emLab")) %>%
  arrange(cat_label, hours) %>%
  mutate(prefer_lat = ifelse(recipe_general == "LA Times", 1, 0))

## run model
## -------------------------

## view
cookie_model_df

## model
models <-  cookie_model_df %>% 
  group_by(cat_label) %>% 
  do(model = glm(prefer_lat ~
                   emlab +
                   hours,
                 data = .,
                 family = binomial))

models$model

## chip test
chip_df <- cookie_model_df %>% 
  filter(cat_label == "Chocolate chip")

chip_model <- glm(formula = prefer_lat ~ emlab + hours,
                                 data = chip_df,
                                 family = binomial)

summary(chip_model)$coefficients


