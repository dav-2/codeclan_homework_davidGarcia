# packages and libraries --------------------------------------------------

library(tidyverse)
library(readxl)
library(openxlsx)
library(outliers)
library(janitor)

# data reading ------------------------------------------------------------

raw_ship_id <- read_excel("raw_data/seabirds.xls", 
                          sheet = 1)

# data cleaning ----------------------------------------------------------------
raw_ship_id <- raw_ship_id %>% 
  select(`RECORD ID`, LAT)

# LATCELL and LAT have the same NAs, so I am only going to use LAT. 

names(raw_ship_id) <- c("record_id", "latitude")

# I am going to round lat because several values are already rounded to 0 decimals. 

raw_ship_id <- raw_ship_id %>% 
  mutate(latitude = round(latitude))


# NAs ---------------------------------------------------------------------

# It is not needed to drop the NAs in lat as they don't appear when filtering latitudes by greater than 
# or less than.  
# I have decided to input them the median latitude, just to practise. 

raw_ship_id <- raw_ship_id %>% 
  mutate(latitude = ifelse(is.na(latitude) == T, median(latitude, na.rm = T), latitude))


# outliers ----------------------------------------------------------------

#latitude_zscores <- scores(raw_ship_id$latitude)
#latitude_is_outlier <- latitude_zscores > 3 | latitude_zscores < -3
#raw_ship_id <- raw_ship_id %>%
#  mutate(is_outlier = latitude_is_outlier)

# There are no outliers > 90 or < -90 so we must keep them. 

clean_ship_id <- raw_ship_id

# data writing ------------------------------------------------------------

write_csv(clean_ship_id, "clean_data/clean_ship_id.csv")
