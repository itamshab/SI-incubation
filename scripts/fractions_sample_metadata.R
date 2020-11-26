# loads sample metadata

# Author: Itamar Shabtai
# Version: 2020-01-26

# Libraries
library(tidyverse)
library(here)

# Parameters

  # input_file
file_raw_metadata <- here("data-raw/fractions_sample_metadata.csv")
  
# output_file
file_out_metadata <- here("data/fractions_sample_metadata.rds") 

# ============================================================================

file_raw_metadata  <-  
  read_csv("./data-raw/fractions_sample_metadata.csv") %>% 
  select(-8) %>% 
  write_rds(file_out_metadata)

