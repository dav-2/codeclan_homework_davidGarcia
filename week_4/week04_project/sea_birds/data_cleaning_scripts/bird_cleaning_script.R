
# packages and libraries --------------------------------------------------

install.packages("readxl")
install.packages("openxlsx")
library(tidyverse)
library(readxl)
library(openxlsx)
library(outliers)

# data reading ------------------------------------------------------------

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
  mutate(abbreviation = str_replace_all(abbreviation, "\\s", "_")) %>% 
  mutate(abbreviation = str_replace_all(abbreviation, "_/_", "/")) %>% 
  mutate(abbreviation = str_replace_all(abbreviation, "_/", "/")) %>% 
  mutate(abbreviation = str_replace_all(abbreviation, "/_", "/"))

raw_bird_id <- raw_bird_id %>% 
  mutate(common_name = str_replace_all(common_name, "[_][A-Z][A-Z0-6]+", ""))

# The expression above achieves the same result that the lines commented below and it is much shorter. 

  #mutate(common_name = str_replace_all(common_name, "_PL[0-9]", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_SUBAD", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_ADF", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_AD", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_JUV", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_IMM", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_INT", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_DRK", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_LGHT", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "_WHITE", "")) %>% 
  #mutate(common_name = str_replace_all(common_name, "Lesser_frigatebird_M", "Lesser_frigatebird"))


# Let's make another column for the characteristics: age, sex and plumage phase.

raw_bird_id <-  raw_bird_id %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_PL1", "_aaa_PL1")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_PL2", "_aaa_PL2")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_PL3", "_aaa_PL3")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_PL4", "_aaa_PL4")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_PL5", "_aaa_PL5")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_PL6", "_aaa_PL6")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_SUBAD", "_aaa_SUBAD")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_AD", "_aaa_AD")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_JUV", "_aaa_JUV")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_IMM", "_aaa_IMM")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_INT", "_aaa_INT")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_DRK", "_aaa_DRK")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_WHITE", "_aaa_WHITE")) %>% 
  mutate(scientific_name = str_replace_all(scientific_name, "_LGHT", "_aaa_LGHT"))

raw_bird_id <- raw_bird_id %>% 
  separate(scientific_name, 
           c("scientific_name", "characteristics"), 
           sep = "_aaa_")

raw_bird_id <- raw_bird_id %>% 
  mutate(characteristics = ifelse(scientific_name == "Fregata_ariel_M", "M", characteristics))


raw_bird_id <- raw_bird_id %>% 
  mutate(scientific_name = str_replace(scientific_name, "Fregata_ariel_M", "Fregata_ariel"))

raw_bird_id <- raw_bird_id %>% 
  mutate(common_name = str_to_lower(common_name), 
         scientific_name = str_to_lower(scientific_name), 
         abbreviation = str_to_lower(abbreviation)
         )


# As there aren't any NAs in common_name, it's not needed to replace the characteristics in
# scientific_name or abbreviation, because we don't need to work with scientific_name or abbreviation 
# to find any of the questions asked.


# NAs ---------------------------------------------------------------------

# We can drop the NAs in count which are NAs due to common_name == [no_birds_recorded]. 

raw_bird_id <- raw_bird_id %>% 
  drop_na(count)

# I finally dropped every NA in count because
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

clean_bird_id <- raw_bird_id

# data writing ------------------------------------------------------------

write_csv(clean_bird_id, "clean_data/clean_bird_id.csv")
