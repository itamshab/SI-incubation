# combines fractions sample metadata and data 
# mutates several variables and returns an .rds file

# Author: Name
# Version: 2020-01-26

# Libraries
library(tidyverse)
library(here)

# Parameters

  # fractions data file
file_fractions <- here("data/fractionation_experiment.rds")

  # sample metadata file
file_metadata <- here("data/fractions_sample_metadata.rds")

  # fractions elemental and isotopes
file_elemental <- here("data/fractions_elemental.rds")
    
# Output file
file_out <- here("data/fractions_combined.rds")

# ============================================================================

fractions_data <-
  file_fractions %>% 
  read_rds() %>% 
  select(-2)

fractions_metadata <-
  file_metadata %>% 
  read_rds()

# add elements isotopes data
fractions <- elemental %>% 
  file_elemental %>% 
  read_rds()

fractions <-  inner_join(fractions_metadata, fractions_data, by = "jar_ID") %>%
  mutate(litter_added_mg_g = litter_weight_mg / soil_weight) %>% 
  select(-c(8,9)) %>%  # removal initial soil weight
  write_rds(file_out)

  
  
 
 