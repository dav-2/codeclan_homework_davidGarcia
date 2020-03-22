# packages and libraries --------------------------------------------------

library(tidyverse)
library(janitor)
library(outliers)


# data reading ------------------------------------------------------------

cake_raw_data_1961 <- read_csv("raw_data/cake-ingredients-1961.csv")
cake_raw_data_1961_presentation <- cake_raw_data_1961

# data cleaning -----------------------------------------------------------

cake_raw_data_1961 <- cake_raw_data_1961 %>% 
  clean_names() %>% 
  mutate(cake = str_to_lower(cake)) %>% 
  mutate(cake = str_replace_all(cake, "\\s", "_")) %>% 
  pivot_longer(-cake, 
               names_to = "code", 
               values_to = "amount") 

cake_raw_data_1961 <- 
  left_join(cake_raw_data_1961, cake_raw_data_code, "code") %>% 
  select(-code) %>% 
  drop_na() %>% 
  select(1, ingredient,measure, amount)

# I am pretty sure that the NAs correspond to ingredients with amount == 0.
# We can't imput values to them as that would add new ingredients to the cakes. 
# The best option is to drop them. 
# If we set -3 < zscores to < 3 we have three outliers in amounts.
# But they are proportional to the other amounts, so we must keep them.


# data writing ------------------------------------------------------------

write_csv(cake_clean_data_1961, "clean_data/cake_clean_data_1961.csv")

