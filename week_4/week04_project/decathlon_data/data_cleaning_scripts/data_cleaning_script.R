# packages and libraries --------------------------------------------------

install.packages("here")
library(tidyverse)
library(here)
library(janitor)


# data reading ------------------------------------------------------------

decathlon_raw_data <- read_rds("raw_data/decathlon.rds")


# data cleaning -----------------------------------------------------------

decathlon_clean_data <- decathlon_raw_data %>% 
    clean_names(case = "snake") %>% 
    rownames_to_column(var = "name") %>% 
    mutate(name = str_to_lower(name), competition = str_to_lower(competition)) %>% 
    pivot_longer(
      cols = -c(name, rank, points, competition),
      names_to = "discipline",
      values_to = "score",
      ) %>% 
    select(1, competition, discipline, score, points, rank)

# After checking with plots and formulas the few outliers,
# it is possible to say that every outlier is a possible value. 
# There aren't any values extremely better than the others
# or better than the world's records. 


# data writing ------------------------------------------------------------

write_csv(decathlon_clean_data, "clean_data/decathlon_clean_data.csv")
