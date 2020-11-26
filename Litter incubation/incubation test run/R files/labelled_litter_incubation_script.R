#load libraries
library(tidyverse) 
library(readxl)
library(here)

here()
#import wide incubation data of mL CO2, omit day 3 (high conc.)
incubation_data <- read_excel(here("Data", "Litter incubation", "incubation test run",
                                   "incubation data file litter test 2.xlsx"), sheet = 2)
   
  
#import sample information 
sample_data <- read_excel(here("Data", "Litter incubation", "incubation test run",
                               "incubation data file litter test 2.xlsx"), sheet = 3) %>% 
  select(-solution_g, -rep, -jar_weight_g, -soil_jar_g)

#combine sample information with incubation data, sum CO2 mL over time, 
#calculate mineralization and plot
wide_data <- inner_join(sample_data, incubation_data)   
  
wide_data_C_all <- wide_data %>% 
  mutate(cum_CO2 = rowSums(wide_data[8:17], na.rm = TRUE)) %>% 
  mutate(cum_mineralization = cum_CO2/296.15/0.08205*12.01/soil_weight_g*1000) %>%  
  mutate(Sample = recode(soil, 
                         "low" = "Low moisture soil",
                         "high" = "High moisture soil",
                         "litter" = "Litter only")) %>% 
  filter(soil != "blank")

ggplot(wide_data_C_all, aes(solution, cum_CO2, fill = solution, shape = litter),
       size = 3) + 
  geom_jitter(size = 3, width = 0.075) + scale_shape_manual(values = c(21,24)) + 
  labs(y = "Cumulative CO2 (mL)", x = "Solution added") +
  facet_wrap(.~Sample) + guides(fill = FALSE)
ggsave("CO2_cum.png", last_plot())

#compare only soils w/o litter
wide_data_C_soils <- wide_data %>% 
  mutate(cum_CO2 = rowSums(wide_data[8:17], na.rm = TRUE)) %>% 
  mutate(cum_mineralization = cum_CO2/296.15/0.08205*12.01/soil_weight_g,
         mineralizability = cum_mineralization/OC_g) %>%  
  mutate(Sample = recode(soil, 
                         "low" = "Low moisture soil", 
                         "high" = "High moisture soil", 
                         "litter" = "Litter only")) %>% 
  filter(soil != "blank", litter != "n", soil != "litter")

ggplot(wide_data_C_soils, aes(solution, cum_mineralization, fill = solution),
       size = 3, shape = 21) + 
  geom_jitter(size = 3, shape = 21, width = 0.075) +
  labs(y = "Cumulative mineralization (mg CO2-C/g soil)", x = "Solution added") +
  facet_wrap(.~Sample) + theme(legend.position = "none") + 
ggsave("cum_mineralization.png", last_plot()) 

ggplot(wide_data_C_soils, aes(solution, mineralizability, fill = solution)) + 
  geom_jitter(size = 3, shape = 21, width = 0.1) + 
  labs(y = "Mineralizability (mg CO2-C/g C)", x = "Solution added") +
  facet_wrap(.~Sample) + 
ggsave("mineralizability.png", last_plot())

#transform to graphpadprism format
wide_data_C_soils_prism <- wide_data_C_soils %>%
  select(c(2,3,18,19)) %>% 
  gather(c(3:4), key = "Type", value = "value") %>%  
  group_by(soil,solution,Type) %>%    
  mutate(row_id = 1:n()) %>% 
  ungroup %>% 
  spread(row_id, value) %>% 
  group_by(soil, solution, Type)

#write_csv(wide_data_C_soils_prism, 
          path = "C:\\Users\\Itama\\Box\\Data\\SI soils\\Litter incubation\\incubation test run\\prelim_incub.csv")


#Transform to long structure
long_data <- gather(incubation_data, c(2:11), #edit columns to include additional dates
                    key = "Days",value = "CO2_mL") 
long_data$Days <- as.numeric(as.character(long_data$Days))  
#long_data$time <- convertToDateTime(long_data$time, origin = "1900-01-01")

#calculate mg of CO2-C
long_data_C <- long_data %>% 
  mutate(mg_CO2_C = (CO2_mL/(296.15*0.08205))*12.01) %>% 
  filter(jar_ID != 43:45)

# filter out blanks and litter jars
long_data_C_soils <- filter(long_data_C, jar_ID != 46:51)

#combine sample info with incubation data and calculate mineralization (mg CO2-C/kg soil)
#for each jar and mean of triplicates
sample_incubation_data <- inner_join(sample_data, long_data_C_soils) %>% 
  mutate(mg_CO2_C_g_soil = mg_CO2_C / soil_weight_g ) %>% 
  group_by(soil, solution, litter, Days) %>% 
  summarize(mean_mineralization = mean(mg_CO2_C_g_soil)) %>% 
  mutate(moisture = recode(soil,
                           "low" = "Low moisture soil", "high" = "High moisture soil"))  

sample_incubation_data %>% 
  filter(litter == "y") %>% 
  ggplot(aes(Days, mean_mineralization, fill = solution)) + 
  geom_jitter(shape = 21, size = 3, width = 0.1) + ylab("Mineralization mg CO2-C/g soil") + 
  xlim(0,16) + 
  facet_wrap(moisture ~ .) 
  ggsave("mineralization_daily.png", last_plot())


