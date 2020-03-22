# libraries and packages --------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

# data reading ------------------------------------------------------------

raw_candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
view(raw_candy_2015)

names(raw_candy_2015) 

raw_candy_2015 <- raw_candy_2015 %>% 
  select(Timestamp, `How old are you?`, `Are you going actually going trick or treating yourself?`, starts_with("["))

names(raw_candy_2015)
view(raw_candy_2015)
