
# packages and libraries --------------------------------------------------

install.packages("readxl")
install.packages("openxlsx")
library(tidyverse)
library(readxl)
library(openxlsx)
library(outliers)

# data reading ------------------------------------------------------------

raw_ship_id <- read_excel("raw_data/seabirds.xls", 
                          sheet = 1)
raw_bird_id <- read_excel("raw_data/seabirds.xls", 
                          sheet = 2)

# data cleaning ----------------------------------------------------------------

raw_bird_id <- raw_bird_id %>% 
  select(`RECORD ID`, 
         `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`, 
         `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`, 
         `Species abbreviation`, 
         COUNT
         )

names(raw_bird_id) <- c("record_id", 
                        "common_name",
                        "scientific_name",
                        "abbreviation",
                        "count"
                        )

raw_bird_id <- raw_bird_id %>% 
  mutate(common_name = str_to_lower(common_name), 
         scientific_name = str_to_lower(scientific_name), 
         abbreviation = str_to_lower(abbreviation)
         )

raw_bird_id <- raw_bird_id %>% 
  mutate(common_name = str_replace_all(common_name, "\\s", "_")) %>% 
  mutate(common_name = str_replace_all(common_name, "_/_", "/")) %>% 
  mutate(common_name = str_replace_all(common_name, "_/", "/")) %>% 
  mutate(common_name = str_replace_all(common_name, "/_", "/"))

raw_bird_id <- raw_bird_id %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "\\s", "_")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_/_", "/")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_/", "/")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "/_", "/"))

raw_bird_id <- raw_bird_id %>% 
  mutate(common_name = str_replace_all(common_name, "_pl[0-9]", "")) %>% 
  mutate(common_name = str_replace_all(common_name, "_ad", "")) %>% 
  mutate(common_name = str_replace_all(common_name, "_subad", "")) %>% 
  mutate(common_name = str_replace_all(common_name, "_juv", "")) %>% 
  mutate(common_name = str_replace_all(common_name, "_imm", "")) %>% 
  mutate(common_name = str_replace_all(common_name, "_drk", "")) %>% 
  mutate(common_name = str_replace_all(common_name, "_lght", "")) %>% 
  mutate(common_name = str_replace_all(common_name, "_white", ""))

# As there aren't any NAs in common_name, it's not needed to replace the characteristics above 
# for scientific_name or abbreviation, because we don't need to work with scientific_name or abbreviation 
# to find any of the questions asked. 


# NAs ---------------------------------------------------------------------

# We can drop the NAs in count which are NAs due to common_name == [no_birds_recorded]. 

raw_bird_id <- raw_bird_id %>% 
  drop_na(count)

# I finally dropped every NA from count because
# I was not sure whether it was better to drop them or input them a mean or median value. 
# I didn't drop the NAs from scientific_name because they have names in common_name 


# outliers ----------------------------------------------------------------

count_zscores <- scores(raw_bird_id$count)
count_zscores
is_outlier <- count_zscores > 3 | count_zscores < -3
raw_bird_id <- raw_bird_id %>%
  mutate(is_outlier = is_outlier)

#ggplot(raw_bird_id, aes(x = common_name, y = count)) +
  #geom_boxplot()

# I don't think I should drop all the outliers. Those birds migrate in enormous flocks. I will drop
# only the values 99999.

raw_bird_id <- raw_bird_id %>% 
  filter(count != 99999) %>% 
  select(-is_outlier)
view(raw_bird_id)



