# load libraries----------------------------------------------------------------
library(tidyverse) 
library(readxl)
library(agricolae)
library(broom)
library(here)
library(emmeans)
library(kableExtra)


# basic plot theme--------------------------------------------------------------
mytheme <- theme_bw() + theme(panel.grid = element_blank()) + 
  theme(axis.text.x = element_text(size = 15)) +  
  theme(axis.text.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 17)) +
  theme(axis.title.y = element_text(size = 17)) +
  theme(legend.text = element_text(size = 17)) +
  theme(legend.title = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 17))
  

# import incubation data CO2 (mL) in wide form----------------------------------
full_incubation_exp_data <- read_excel(
  here("data-raw/full litter incubation experiment.xlsx"), 
  sheet = 3) %>% 
  drop_na(jar_ID)


# set negative CO2 emissions to zero
full_incubation_exp_data[full_incubation_exp_data < 0] <- 0 


# import sample metadata--------------------------------------------------------
sample_metadata <- read_excel(
  here("data-raw/full litter incubation experiment.xlsx"), sheet = 1) %>% 
  select(-soil, -jar_weight) #remove non-essential data


# data manipulation-------------------------------------------------------------

# transform to long structure---------------------------------------------------

long_data <- 
  #edit columns to include additional dates
  pivot_longer(full_incubation_exp_data, c(2:16), 
               names_to = "Days",values_to = "CO2_mL") %>% 
  drop_na(CO2_mL)
#convert $Days to numeric
long_data$Days <- as.numeric(as.character(long_data$Days))  


# calculate mg of CO2 carbon using ideal gas equation---------------------------
# reminder to calculate average temperature during incubation  
long_data_c <- long_data %>% 
  mutate(mg_co2_c = (CO2_mL/(295.15*0.08205))*12.01) %>% 
  filter(jar_ID != 100:102) # remove blank jars


#combine metadata with incubation data------------------------------------------
long_data_joined <- inner_join(sample_metadata, long_data_c, by = "jar_ID") %>% 
  filter(Treatment != "Blank") %>% 
  rename(emission = "mg_co2_c") %>% 
  # mineralization (mg C/g soil) and mineralizability (mg C/g soil C)
  mutate(mineralization = emission / soil_weight,
         mineralizability = mineralization / (carbon_g/soil_weight)) %>% 
 

#calculate cumulative values---------------------------------------------------- 
  group_by(jar_ID) %>% 
  replace_na(list(emission = 0)) %>%
  mutate(cum_mineralization = cumsum(mineralization),
         cum_emission = cumsum(emission),
         cum_mineralizability = cumsum(mineralizability)) 
  
#calculate mean, sd and se of cumulative values---------------------------------
  long_data_joined_mean <- long_data_joined %>% 
  group_by(Treatment, Moisture,  Litter, Days) %>%        
  summarize(mean_cum_emission = mean(cum_emission),
            mean_cum_mineralization = mean(cum_mineralization),
            mean_cum_mineralizability = mean(cum_mineralizability),
            sd_cum_emission = sd(cum_emission),
            se_cum_emission = sd_cum_emission/sqrt(3),
            sd_cum_mineralization = sd(cum_mineralization),
            se_cum_mineralization = sd_cum_mineralization/sqrt(3),
            sd_cum_mineralizability = sd(cum_mineralizability),
            se_cum_mineralizability = sd_cum_mineralizability/sqrt(3))


# reorder factors

long_data_joined_mean$Moisture <- 
  factor(long_data_joined_mean$Moisture, levels = c("Low", "High"), ordered = TRUE)

long_data_joined_mean$Treatment <- 
  factor(long_data_joined_mean$Treatment, levels = c("DW", "Ca", "Litter"))

# write incubation results to rds

write_rds(long_data_joined_mean, here("data/mean_incubation_results.rds"))
write_rds(long_data_joined, here("data/incubation_results.rds"))

# plots-------------------------------------------------------------------------

#plot CO2 emitted from soils and only litter------------------------------------   
emission_plot <- long_data_joined_mean %>% 
  ggplot(aes(Days, mean_cum_emission, shape = Litter, fill = Treatment)) + 
  geom_point(size = 2) + 
  scale_shape_manual(values = c(21,23)) +
  guides(fill = guide_legend(override.aes = list(shape = 22))) +
  geom_errorbar(
    aes(ymin = mean_cum_emission - sd_cum_emission, 
        ymax = mean_cum_emission + sd_cum_emission),
        width = 0.1) +
  ylab(expression("CO2 emitted (mg CO2-C)")) + 
  facet_wrap(Moisture ~ .) +
  mytheme

emission_plot

#plot CO2 emitted from soils w/o litter-----------------------------------------   
control_emission_plot <- filter(long_data_joined_mean, Litter == "N") %>% 
  ggplot(aes(Days, mean_cum_emission, color = Treatment)) + 
  geom_point(size = 2) + 
  guides(fill = guide_legend(override.aes = list(shape = 22))) +
  geom_errorbar(aes(
    ymin = mean_cum_emission - sd_cum_emission,
    ymax = mean_cum_emission + sd_cum_emission),
    width = 0.1) +
  ylab(expression("CO2 emitted (mg CO2-C)")) + 
  facet_wrap(Moisture ~ .) +
  mytheme

control_emission_plot


#plot mineralization------------------------------------------------------------

mineralization_plot <- long_data_joined_mean %>% 
  filter(Treatment != "litter") %>% 
  ggplot(aes(Days, mean_cum_mineralization, fill = Treatment, shape = Litter)) + 
  geom_point(size = 2.5) + 
  scale_shape_manual(values = c(21,23)) +
  geom_errorbar(aes(
    ymin = mean_cum_mineralization - sd_cum_mineralization, 
    ymax = mean_cum_mineralization + sd_cum_mineralization), 
    width = 0.2) +
  labs(y = (expression(bold("Mineralization (mg CO"["2"]*"-C/g soil)"))),
       x = expression(bold("Days of incubation")), fill = "Treatment") + 
  facet_wrap(Moisture ~ .) +
  guides(fill = guide_legend(order = 1, override.aes = list(shape = 22))) +
  mytheme + scale_fill_brewer(palette = "Set1")

mineralization_plot

ggsave(filename = here("plots/final_min.svg"), plot = mineralization_plot, width = 6, height = 4)


#plot mineralizability----------------------------------------------------------

water_content_labels <- c(Low = "Low water content", High = "High water content")

mineralizability_plot <- long_data_joined_mean %>% 
  filter(Treatment != "Litter") %>% 
  ggplot(aes(Days, mean_cum_mineralizability, fill = Treatment, shape = Litter)) + 
  geom_point(size = 2.5) + 
  scale_shape_manual(values = c(21,23)) +
  geom_errorbar(aes(
    ymin = mean_cum_mineralizability - se_cum_mineralizability, 
    ymax = mean_cum_mineralizability + se_cum_mineralizability), 
    width = 0.2) +
  labs(y = expression(bold("Mineralizability (mg CO"["2"]*"-C/g SOC)")),
       x = expression(bold("Days of incubation")), fill = "Treatment") + 
  facet_wrap(. ~ Moisture, labeller = labeller(Moisture = water_content_labels)) + 
  guides(fill = guide_legend(order = 1, override.aes = list(shape = 22))) +
  mytheme +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 15)) 

final_min_c_plot <- mineralizability_plot + scale_fill_brewer(palette = "Set1") + xlim(0,15)

final_min_c_plot

ggsave(filename = here("plots/final_min_c.svg"), plot = final_min_c_plot, width = 8, height = 5)

# statistical analyses----

# 3-way ANOVA with interaction terms (Treatment*moisture*litter) 
# on cumulative mineralizability at 4 day and end of incubation 
# followed by emmeans to test treatment effect at each level of the 2 factors 

# wrangling data for the 4 day ANOVA  

# tibble of cumulative mineralization at days = 4)----

four_day_mineralization <- long_data_joined %>%
  ungroup() %>% 
  drop_na(cum_mineralization) %>% 
  select(1,2,3,4, 5,11,13:18) %>% 
  filter(Days > 4 & Days < 5) 

four_day_mineralization$Treatment <- as.factor(four_day_mineralization$Treatment)
four_day_mineralization$Moisture <- as.factor(four_day_mineralization$Moisture)

write_rds(four_day_mineralization, here("data/4day_min_data.rds"))

# tibble of mean 4day cumulative mineralization----

mean_4day_min <- four_day_mineralization %>% 
  group_by(Litter, Moisture, Treatment) %>% 
  summarize(mean_min = mean(cum_mineralization),
            mean_min_c = mean(cum_mineralizability),
            sd_min = sd(cum_mineralization),
            sd_min_c = sd(cum_mineralizability))

# table of mean 4 day mineralization----

four_day_mineralization_table <- 
  kable(
  mean_4day_min, align = "lllcccc",digits = c(1, 1, 1, 2, 2, 2, 2), 
  col.names = c("Litter", "Moisture", "Treatment", "Mineralization", 
                "Mineralizability", "SD mineralization", "SD mineralizability"),
  caption = "Summary table for incubation after 4 days") %>% 
  kable_styling(full_width = T, position = "center")


four_day_mineralization_table

# 3-way anova on 4 day cum mineralizability----

day_4_aov <- lm(cum_mineralizability ~ Treatment*Moisture*Litter, data = four_day_mineralization)

summary(day_4_aov)

day_4_anova_table <- 
  kable(anova(day_4_aov),
        digits = c(1, 1, 1, 1, 4),
        align = "lccccc",
        caption = "3-way ANOVA for day 4 cumulative mineralizability") %>% 
  kable_styling()

day_4_anova_table

# emmeans on 3-way anova----

em_4_day <- emmeans(day_4_aov, pairwise ~ Treatment | Moisture*Litter)
em_4_day_contrasts <- em_4_day$contrasts %>% 
  summary(infer = TRUE) %>% 
  as_data_frame()

em_4_day_contrasts

day4_contrasts <- kable(em_4_day_contrasts, 
                              align = "lllccccccc",
                              digits = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 4),
                              caption = "Comparison of treatment estimated 
                        marginal means of mineralizability after 4 days of incubation") %>% 
  kable_styling(full_width = T, position = "center")

day4_contrasts

emmip(day_4_aov, Treatment~Moisture) +
  mytheme

# wrangling data for end of incubation ANOVA----  

# tibble of max value (total mineralization at time t)----

total_mineralization <- long_data_joined %>%
  #filter(litter != "n") %>% 
  select(1,2,3,4,5,13:18) %>% 
  drop_na(cum_mineralization) %>% 
  filter(cum_mineralization == max(cum_mineralization))

total_mineralization$Treatment <- as.factor(total_mineralization$Treatment)
total_mineralization$Moisture <- as.factor(total_mineralization$Moisture)

write_rds(total_mineralization, here("data/total_min_data.rds"))

# tibble of mean cumulative mineralization----

mean_tot_min <- total_mineralization %>% 
  group_by(Litter, Moisture, Treatment) %>% 
  summarize(mean_min = mean(cum_mineralization),
            mean_min_c = mean(cum_mineralizability),
            mean_emi = mean(cum_emission),
            sd_min = sd(cum_mineralization),
            sd_min_c = sd(cum_mineralizability),
            sd_emi = sd(cum_emission))

# table of mean total mineralization

total_mineralization_table <- kable(
  mean_tot_min, align = "lllcccc",digits = c(1, 1, 1, 2, 2, 2, 2), 
  col.names = c("Litter", "Moisture", "Treatment", "Mineralization", 
                "Mineralizability", "CO2 emission", "SD mineralization", 
                "SD mineralizability", "SD emission")) %>% 
  kable_styling(full_width = T, position = "center")

total_mineralization_table

#3-way ANOVA of total  mineralization---- 

day_end_aov <- aov(cum_mineralizability ~ Moisture*Treatment*Litter, 
                             data = total_mineralization)

anova(day_end_aov)

# table of total mineralization

options(knitr.kable.NA = '')

day_end_anova_table <- 
  kable(anova(day_end_aov),
        digits = c(1, 1, 1, 1, 4),
        align = "lccccc",
        caption = "3-way ANOVA for cumulative mineralizability") %>% 
  kable_styling()

day_end_anova_table

# emmeans of total mineralization

em_day_end <- emmeans(day_end_aov, pairwise ~ Treatment | Moisture*Litter)

emmean_end_contrasts <- em_day_end$contrasts %>% 
  summary(infer = TRUE) %>% 
  as_data_frame()

end_contrasts <- kable(emmean_end_contrasts, 
                               align = "lllccccccc",
                               digits = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 4),
                               caption = "Comparison of treatment estimated 
                        marginal means of cumulative mineralizability at end of incubation") %>% 
  kable_styling(full_width = T, position = "center")

end_contrasts


emmip(day_end_aov, Moisture~Treatment) +
  mytheme





################## scraps######################################################


#1-way ANOVA of TOTAL cumulative mineralization ~ Treatment---------------------

# filtering out DFs for each ANOVA

# low_moisture_n <-  filter(total_mineralization,
#                           Moisture == "Low" & Litter == "N")
#
# low_moisture_y <- filter(total_mineralization,
#                          Moisture == "Low" & Litter == "Y")
#
# high_moisture_n <- filter(total_mineralization,
#                           Moisture == "High" & Litter == "N")
#
# high_moisture_y <- filter(total_mineralization,
#                           Moisture == "High" & Litter == "Y")
#
# # constructing linear models
#
# aov_low_n <- lm(cum_mineralizability ~ Treatment,
#                 data = low_moisture_n)
# aov_low_y <- lm(cum_mineralizability ~ Treatment,
#                 data = low_moisture_y)
# aov_high_n <- lm(cum_mineralizability ~ Treatment,
#                 data = high_moisture_n)
# aov_high_y <- lm(cum_mineralizability ~ Treatment,
#                 data = high_moisture_y)
#
# # tidy model results
#
# tidy(aov_low_n)
# tidy(aov_low_y)
# tidy(aov_high_n)
# tidy(aov_high_y)
#
# # TukeyHSD differences between treatment
#
# HSD.test(aov_low_n, trt = "Treatment",console = TRUE)
# HSD.test(aov_low_y, trt = "Treatment",console = TRUE)
#
#
# #2-way ANOVA of total cumulative mineralization ~ Treatment * Moisture----------
#
#
# #2-way with no interactions
#
# anova_mineralizability_2way <- lm(
#   cum_mineralizability ~ Treatment + Moisture,
#   data = total_mineralization)
#
# shapiro.test(residuals(anova_mineralizability_2way))
# anova(anova_mineralizability_2way)
# summary(anova_mineralizability_2way)
# tidy(anova_mineralizability_2way)
#
# #2-way with interactions
#
# anova_mineralizability_2way_inter <- lm(
#   cum_mineralizability ~ Treatment * Moisture,
#   data = total_mineralization)
#
# shapiro.test(residuals(anova_mineralizability_2way_inter))
# anova(anova_mineralizability_2way_inter)
# summary(anova_mineralizability_2way_inter)
#tidy(anova_mineralizability_2way_inter)


# ANOVA's are not significant
#test_2_mineralization_2way_moisture <- HSD.test(anova_mineralization_2way,
#trt = 'Moisture')
#test_2_mineralization_2way_moisture
#test_2_mineralization_2way_treatment <- HSD.test(anova_mineralization_2way,
#trt = 'Treatment')
#test_2_mineralization_2way_treatment






# prep data for repeated measures ANOVA-----------------------------------------
#long_data_joined_time <- long_data_joined  %>%
#  ungroup() %>%
#  drop_na(soil_weight)
#long_data_joined_time <- long_data_joined_time %>%
#  mutate(id = 1:nrow(long_data_joined_time)) %>%
#convert_as_factor(Days, id, jar_ID, litter, treatment, water_content)

#write.csv(x = long_data_joined, "repeated_data.csv")


#1-way ANOVA of 4day mineralizability ~ Treatment---------------------------------

# filtering out DFs for each ANOVA

#low_moisture_n_four <-  filter(four_day_mineralization,
#                               Moisture == "Low" & Litter == "N")
#
#low_moisture_y_four <- filter(four_day_mineralization,
#                              Moisture == "Low" & Litter == "Y")
#
#high_moisture_n_four <- filter(four_day_mineralization,
#                               Moisture == "High" & Litter == "N")
#
#high_moisture_y_four <- filter(four_day_mineralization,
#                               Moisture == "High" & Litter == "Y")

# constructing linear models

#aov_low_n_four <- lm(cum_mineralizability ~ Treatment,
#                     data = low_moisture_n_four)

#aov_low_y_four <- lm(cum_mineralizability ~ Treatment,
#                     data = low_moisture_y_four)

#aov_high_n_four <- lm(cum_mineralizability ~ Treatment,
#                      data = high_moisture_n_four)
#
# aov_high_y_four <- lm(cum_mineralizability ~ Treatment,
#                       data = high_moisture_y_four)
#
# # tidy model results
#
# tidy(aov_low_n_four)
# tidy(aov_high_n_four)
# tidy(aov_low_y_four)
# tidy(aov_high_y_four)

# Tukey HSD differences between treatment

# HSD.test(aov_low_n_four, trt = "Treatment",console = TRUE)
# HSD.test(aov_high_n_four, trt = "Treatment",console = TRUE)