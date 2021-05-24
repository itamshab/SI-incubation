# This script combines CO2 and microbial biomass data to calculate metabolic quotient
# and substrate use efficiency

# Author: Itamar Shabtai
# Version: 2020-01-26

# Libraries
library(tidyverse)
library(here)
here()
# Parameters

MBC_file_data <- here("C:/Users/itama/OneDrive - Cornell University/Projects/research/SI-2/SI2-microbial-biomass/data/mbc_data.rds")

beg_CO2_file_data <- here("data/4day_min_data.rds")

end_CO2_file_data <- here("data/total_min_data.rds")
# ============================================================================

# code

# load data----

mbc_df <- read_rds(MBC_file_data) %>% 
  rename(Treatment = treatment,
         Moisture = water_content) %>% 
  select(-1) %>%   
  filter(rep < 4) # Include reps 1-3 to join with CO2 data

# input from other reps instead of NA's and outliers
mbc_df[1,11] <- 0.361 
mbc_df[37,11] <- 0.454
mbc_df[39,11] <- 0.462
mbc_df[16,11] <- 0.362


beg_co2_df <- read_rds(beg_CO2_file_data) 

# recoding factor to fit MBC data before joining----

mbc_df$Moisture <- recode(mbc_df$Moisture, 
                              "low" = "Low water content", "high" = "High water content",
                              .default = levels(mbc_df$Moisture))

beg_co2_df$Treatment <- recode(beg_co2_df$Treatment, 
                           Control = "DW", Ca = "Ca",
                           .default = levels(beg_co2_df$Treatment))

beg_co2_df$Moisture <- recode(beg_co2_df$Moisture, 
                               "40% WFPS" = "low", "60% WFPS" = "high",
                               .default = levels(beg_co2_df$Moisture))

beg_co2_df$sampling_time <- cbind("Beginning")

end_co2_df <- read_rds(end_CO2_file_data)

end_co2_df$Treatment <- recode(end_co2_df$Treatment, 
                               Control = "DW", Ca = "Ca",
                               .default = levels(end_co2_df$Treatment))
end_co2_df$Moisture <- recode(end_co2_df$Moisture, 
                              "40% WFPS" = "low", "60% WFPS" = "high",
                              .default = levels(end_co2_df$Moisture))
end_co2_df$sampling_time <- cbind("End")

co2_df <- full_join(beg_co2_df, end_co2_df, join_by = "jar_ID") %>% 
  rename(day = sampling_time)
co2_df$day <- as.factor(co2_df$day)


#wrangling tibble with qCO2---- 

qco2_data <- 
  inner_join(co2_df, mbc_df, join_by = c("litter", "Treatment", "Moisture", "day")) %>% 
  group_by(litter, Moisture, Treatment, day) %>% 
  select(-c(1,3:6, 9)) %>% 
  mutate(q_co2 = cum_mineralization / MBC_mg_g_soil)
  
qco2_data$litter <- recode(qco2_data$litter, 
                    n = "No litter", y = "Litter added")


# plotting qCO2
qco2_plot <- ggplot(qco2_data, aes(day, q_co2)) + 
  geom_col(aes(fill = Treatment), position = "dodge") + 
  facet_wrap(litter~Moisture, scales = "free") +
  geom_point(aes(day, q_co2, fill = Treatment), 
             position = position_jitterdodge(jitter.width = 0.1), 
             shape = 21, size = 3) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = expression("Sampling time"), 
       y = expression("Metabolic quotient (mg CO"["2"]*"-C/mg MBC)"))
  
qco2_plot


