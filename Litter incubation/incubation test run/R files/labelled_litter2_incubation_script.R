#load libraries
library(tidyverse) 
library(readxl)
library(here)


my_theme_legend <- theme_bw() + theme(legend.position = "right") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(colour = "grey20", size = 15, angle = 45, hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(colour = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0.5, face = "plain"), 
        axis.title.x = element_text(colour = "black", size = 17, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.title.y = element_text(colour = "black", size = 17, angle = 90, hjust = .5, vjust = 1.5, face = "bold")) +
  theme(legend.title = element_text(size = 10, face = "bold")) 

#import wide incubation data of mL CO2, omit day 3 (high conc.)
incubation_data2 <- read_excel(
  here("Data", "Litter incubation", "incubation test run",
       "incubation data file litter test 2.xlsx"), sheet = 2) 

#import sample information 
sample_data2 <- read_excel(
  here("Data", "Litter incubation", "incubation test run",
       "incubation data file litter test 2.xlsx"), sheet = 3) %>% 
  select(-solution_g, -rep, -jar_weight_g, -soil_jar_g)

#combine sample information with incubation data, sum CO2 mL over time, 
#calculate mineralization and plot
wide_data2 <- inner_join(sample_data2, incubation_data2)   
  
wide_data_C_all2 <- wide_data2 %>% 
  mutate(cum_CO2 = rowSums(wide_data2[8:18], na.rm = TRUE)) %>% 
  mutate(cum_mineralization = cum_CO2/296.15/0.08205*12.01/soil_weight_g*1000) %>%  
  mutate(Sample = recode(soil, "low" = "Low moisture soil", "high" = "High moisture soil", "litter" = "Litter only")) %>% 
  filter(soil != "blank")

 ggplot(wide_data_C_all2, aes(solution, cum_CO2, fill = solution, shape = litter), size = 3) + 
  geom_jitter(size = 3, width = 0.075) + scale_shape_manual(values = c(21,24)) + 
  labs(y = "Cumulative CO2 (mL)", x = "Solution added") + my_theme_legend +
  facet_wrap(.~Sample) + guides(fill = FALSE)
ggsave("CO2_cum.png", last_plot())

#compare only soils w/o litter
wide_data_C_soils2 <- wide_data2 %>% 
  mutate(cum_CO2 = rowSums(wide_data2[8:18], na.rm = TRUE)) %>% 
  mutate(cum_mineralization = cum_CO2/296.15/0.08205*12.01/soil_weight_g,
         mineralizability = cum_mineralization/OC_g) %>%  
  mutate(Sample = recode(soil,
                         "low" = "Low moisture soil",
                         "high" = "High moisture soil", 
                         "litter" = "Litter only")) %>% 
  filter(soil != "blank", litter != "n", soil != "litter")

ggplot(wide_data_C_soils2, aes(solution, cum_mineralization, fill = solution),
       size = 3, shape = 21) + 
  geom_jitter(size = 3, shape = 21, width = 0.075) +
  labs(y = "Cumulative mineralization (mg CO2-C/g soil)", x = "Solution added") +
  facet_wrap(.~Sample) + theme(legend.position = "none") + 
ggsave("cum_mineralization.png", last_plot()) 

ggplot(wide_data_C_soils2, aes(solution, mineralizability, fill = solution)) + 
  geom_jitter(size = 3, shape = 21, width = 0.1) + 
  labs(y = "Mineralizability (mg CO2-C/g C)", x = "Solution added") +
  facet_wrap(.~Sample) + 
ggsave("mineralizability.png", last_plot())

#transform to graphpadprism format
wide_data_C_soils_prism2 <- wide_data_C_soils2 %>%
  select(c(2,3,18,19)) %>% 
  gather(c(3:4), key = "Type", value = "value") %>%  
  group_by(soil,solution,Type) %>%    
  mutate(row_id = 1:n()) %>% 
  ungroup %>% 
  spread(row_id, value) %>% 
  group_by(soil, solution, Type)

write_csv(wide_data_C_soils_prism2, 
          path = "C:\\Users\\Itama\\Box\\Data\\SI soils\\Litter incubation\\incubation test run\\prelim_incub.csv")


#Transform to long structure
long_data2 <- gather(incubation_data2, c(2:12), #edit columns to include additional dates
                    key = "Days",value = "CO2_mL") 
long_data2$Days <- as.numeric(as.character(long_data2$Days))  


#calculate mg of CO2-C
long_data_C2 <- long_data2 %>% 
  mutate(mg_CO2_C = (CO2_mL/(296.15*0.08205))*12.01) %>% 
  filter(jar_ID != 43:45)

# filter out blanks and litter jars, replace NA's with 0 to allow cumulative calculation
long_data_C_soils2 <- filter(long_data_C2, jar_ID != 46:51) 
long_data_C_soils2[is.na(long_data_C_soils2)] <- 0

#combine sample info with incubation data and calculate mineralization (mg CO2-C/kg soil)
#for each jar and mean of triplicates
sample_incubation_data2 <- inner_join(sample_data2, long_data_C_soils2) %>% 
  mutate(mg_CO2_C_g_soil = mg_CO2_C / soil_weight_g ) %>% 
  group_by(soil, solution, litter, Days, jar_ID, OC_g) %>% 
  summarize(mean_mineralization = mean(mg_CO2_C_g_soil)) %>% 
  mutate(moisture = recode(soil, "low" = "Low moisture soil", "high" = "High moisture soil")) %>%     
  group_by(jar_ID) %>% 
  mutate(cum_min = cumsum(mean_mineralization)) %>%   
  mutate(cum_min_C = cum_min/OC_g) %>% 
  mutate(leached = "No")



sample_incubation_data2 %>% 
  filter(litter == "y") %>% 
  ggplot(aes(Days, cum_min, fill = solution)) + 
  geom_jitter(shape = 21, size = 3, width = 0.1) + ylab("Mineralizability mg CO2-C/g C") + 
  facet_wrap(moisture ~ .) + my_theme_legend
  ggsave("mineralizability_daily2.png", last_plot())


