# packages and libraries --------------------------------------------------

install.packages("snakecase")
library(tidyverse)
library(janitor)
library(snakecase)


# data reading ------------------------------------------------------------

cake_raw_data_code <- read_csv("raw_data/cake_ingredient_code.csv")


# data cleaning -----------------------------------------------------------

cake_raw_data_code <- cake_raw_data_code %>% 
  mutate(ingredient = str_to_lower(ingredient)) %>% 
  mutate(code = str_to_lower(code)) %>% 
  mutate(ingredient = str_replace_all(ingredient, "\\s", "_")) %>% 
  mutate(ingredient = str_replace(ingredient, "_cup", "") ) %>% 
  mutate(measure = ifelse(is.na(measure) == T, "cup", measure))
