#load libraries
library(tidyverse) 
library(readxl)
library(here)
library(agricolae)
library(broom)

my_theme_legend <- theme_bw() + theme(legend.position = "right") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(colour="grey20",size=15,angle=45,hjust=1,vjust=1,face="plain"),
        axis.text.y = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0.5,face="plain"), 
        axis.title.x = element_text(colour="black",size=17,angle=0,hjust=.5,vjust=.5,face="bold"),
        axis.title.y = element_text(colour="black",size=17,angle=90,hjust=.5,vjust=1.5,face="bold")) +
  theme(legend.title = element_text(size=10, face = "bold"))

#import wide incubation data of mL CO2, omit day 3 (high conc.)
incubation_data3 <- read_excel(
  here("Data", "Litter incubation", "incubation test run",
       "incubation data file litter test 3.xlsx"), sheet = 2) 

   #delete this when new data available
  
#import sample information 
sample_data3 <- read_excel(
  here("Data", "Litter incubation", "incubation test run",
       "incubation data file litter test 3.xlsx"), sheet = 3) %>% 
  select(-rep, -jar_weight_g, -soil_jar_g, -soil_weight_salt_incubation_g)

#combine sample information with incubation data, sum CO2 mL over time, 
#calculate mineralization and plot
wide_data3 <- inner_join(sample_data3, incubation_data3) #%>% 
#  select(-c(16:19))
  
wide_data_C_all3 <- wide_data3 %>% 
  mutate(cum_CO2 = rowSums(wide_data3[8:16], na.rm = TRUE)) %>% 
  mutate(cum_mineralization = cum_CO2/296.15/0.08205*12.01/soil_weight_g*1000) %>%  
  mutate(Sample = recode(soil,
                         "low" = "Low moisture soil",
                         "high" = "High moisture soil",
                         "litter" = "Litter only")) %>% 
  filter(soil != "blank")

 ggplot(wide_data_C_all3, aes(solution, cum_CO2, fill = solution, shape = litter),
        size = 3) + 
  geom_jitter(size = 3, width = 0.075) + scale_shape_manual(values = c(24)) + 
  labs(y = "Cumulative CO2 (mL)", x = "Solution added") + ylim(0,70) + my_theme_legend +
  facet_wrap(.~Sample) + guides(fill = FALSE)
ggsave("CO2_cum_3.png", last_plot())

#compare only soils w/o litter
wide_data_C_soils3 <- wide_data3 %>% 
  mutate(cum_CO2 = rowSums(wide_data3[8:16], na.rm = TRUE)) %>% 
  mutate(cum_mineralization = cum_CO2/296.15/0.08205*12.01/soil_weight_g,
         mineralizability = cum_mineralization/OC_g) %>%  
  mutate(Sample = recode(soil,
                         "low" = "Low moisture soil",
                         "high" = "High moisture soil",
                         "litter" = "Litter only")) %>% 
  filter(soil != "blank", litter != "n", soil != "litter")

ggplot(wide_data_C_soils3, aes(solution, cum_mineralization, fill = solution),
       size = 3, shape = 21) + 
  geom_jitter(size = 3, shape = 21, width = 0.075) +
  labs(y = "Cumulative mineralization (mg CO2-C/g soil)", x = "Solution added") +
  facet_wrap(.~Sample) + theme(legend.position = "none")  
ggsave("cum_mineralization3.png", last_plot()) 

ggplot(wide_data_C_soils3, aes(solution, mineralizability, fill = solution)) + 
  geom_jitter(size = 3, shape = 21, width = 0.1) + 
  labs(y = "Mineralizability (mg CO2-C/g C)", x = "Solution added") +
  facet_wrap(.~Sample)  
ggsave("mineralizability3.png", last_plot())

#transform to graphpadprism format
wide_data_C_soils_prism3 <- wide_data_C_soils3 %>%
  select(c(2,3,17,18)) %>% 
  gather(c(3:4), key = "Type", value = "value") %>%  
  group_by(soil,solution,Type) %>%    
  mutate(row_id = 1:n()) %>% 
  ungroup %>% 
  spread(row_id, value) %>% 
  group_by(soil, solution, Type)

write_csv(wide_data_C_soils_prism3, 
          path = "C:\\Users\\Itama\\Box\\Data\\SI soils\\Litter incubation\\incubation test run\\prelim_incub.csv")


#Transform to long structure
long_data3 <- gather(incubation_data3, c(2:10), #edit columns to include additional dates
                    key = "Days",value = "CO2_mL") 
  #select(-c(2:4))
long_data3$Days <- as.numeric(as.character(long_data3$Days))  

#calculate mg of CO2-C
long_data_C3 <- long_data3 %>% 
  mutate(mg_CO2_C = (CO2_mL/(296.15*0.08205))*12.01) %>% 
  filter(jar_ID != 19:21)

# filter out blanks and litter jars
long_data_C_soils3 <- filter(long_data_C3, jar_ID != 19:21)

#combine sample info with incubation data and calculate mineralization (mg CO2-C/kg soil)
#for each jar and mean of triplicates
sample_incubation_data3 <- inner_join(sample_data3, long_data_C_soils3) %>% 
  mutate(mg_CO2_C_g_soil = mg_CO2_C / soil_weight_g ) %>% 
  group_by(soil, solution, litter, OC_g, Days, jar_ID) %>% 
  summarize(mean_mineralization = mean(mg_CO2_C_g_soil)) %>% 
  mutate(moisture = recode(soil, 
                           "low" = "Low moisture soil", 
                           "high" = "High moisture soil")) %>% 
  group_by(jar_ID) %>% 
  mutate(cum_min = cumsum(mean_mineralization)) %>%   
  mutate(cum_min_C = cum_min/OC_g) %>% 
  mutate(leached = "Yes")

sample_incubation_data3 %>% 
  filter(litter == "y") %>% 
  ggplot(aes(Days, cum_min_C, fill = solution)) + 
  geom_jitter(shape = 21, size = 3, width = 0) + ylab("Mineralizability mg CO2-C/g C") + 
  xlim(0,16) + 
  facet_wrap(moisture ~ .) 
  ggsave("mineralizability_daily3.png", last_plot())

  
joined_Data <- rbind(sample_incubation_data2, sample_incubation_data3)
  
joined_Data %>% 
   filter(litter == "y") %>% 
    filter(solution == "dw" |
           solution == "ca200" |
          solution == "k200"
           ) %>% 
    ggplot(aes(Days, cum_min)) + 
    geom_jitter(aes(Days, cum_min_C, color = solution, shape = leached),
                size = 3, width = 0.1) +
    scale_shape_manual(values = c(21, 22)) +
    xlim(0,15) + 
    ylab("Cumulative mineralization mg CO2-C/g C") +
    facet_wrap(moisture ~ .) + my_theme_legend
ggsave("mineralizability_daily2_and_3.png", last_plot())

joined_Data_mean <- joined_Data %>% 
  group_by(soil, solution, litter, moisture, leached, Days) %>%      
  summarize(mean_cum_min = mean(cum_min),
            mean_cum_min_c = mean(cum_min_C),
            sd_cum_min = sd(cum_min),
            se_cum_min = sd_cum_min/sqrt(3),
            sd_cum_min_c = sd(cum_min_C),
            se_cum_min_c = sd_cum_min_c/sqrt(3))
  
joined_Data_mean %>% 
  filter(litter == "y") %>% 
  filter(solution == "dw" |
         solution == "ca200") %>% 
  ggplot(aes(Days, mean_cum_min_c)) + 
  geom_jitter(aes(Days, mean_cum_min_c, color = solution, shape = leached), 
              size = 3, width = 0.1) +
  geom_errorbar(aes(
    ymin = mean_cum_min_c - sd_cum_min_c,
    ymax = mean_cum_min_c + sd_cum_min_c),
    width = .5) +
  scale_shape_manual(values = c(21, 22)) + 
  xlim(0,15) + 
  ylab("Cumulative mineralization\n mg CO2-C/g C") +
  facet_wrap(moisture ~ .) + my_theme_legend
