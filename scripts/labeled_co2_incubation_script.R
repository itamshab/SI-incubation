#Load libraries----
library(tidyverse)
library(here)
library(kableExtra)

mytheme <- theme_bw() + theme(panel.grid = element_blank()) + 
  theme(axis.text.x = element_text(size = 15)) +  
  theme(axis.text.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 17)) +
  theme(axis.title.y = element_text(size = 17)) +
  theme(legend.text = element_text(size = 17)) +
  theme(legend.title = element_text(size = 17)) +
  theme(strip.text.x = element_text(size = 17))


#import delta13c data----
delta13c_c02 <- 
  read_csv(here("data-raw/delta13c_c02.csv"),
           col_types = cols(
             Treatment = col_factor(levels = c("DW", "Ca", "Litter", "Blank")), 
             Litter = col_factor(levels = c("N","Y")), 
             Moisture = col_factor(levels = c("Low","High")),
             Time = col_factor(levels = c("Beginning", "End")))) %>% 
  select(-1, -3, -5, -9, -11, -13) 


#pivot wider to get side by side litter and control values for mixing model---- 
data_wide <- delta13c_c02 %>% 
  pivot_wider(names_from = Litter, values_from = c(delta18o, delta13c),   
  names_glue = "{.value}_{Litter}") 


#new df with all no litter values taking the average value----
data_wide_avg <- delta13c_c02 %>% 
  pivot_wider(names_from = Litter, values_from = c(delta18o, delta13c),   
              names_glue = "{.value}_{Litter}") 

mean_no_litter <- mean(data_wide_avg$delta13c_N, na.rm = TRUE)
range_no_litter <- range(data_wide$delta13c_N, na.rm = TRUE)
data_wide_avg$delta13c_n <- mean_no_litter


#pull delta13c value for the litter----
delta_c_litter <- filter(data_wide, Time == "End", Treatment == "Litter") %>% 
  summarize(delta13c_NA = mean(delta13c_NA)) %>% 
  pull(delta13c_NA)
delta_c_litter


#calculate fraction of CO2 from litter C----
fraction_table <- data_wide %>% 
  select(-contains("NA")) %>% 
  drop_na(Moisture) %>% 
  mutate(f_c_co2 = (delta13c_Y - delta13c_N)/(delta_c_litter - delta13c_N))
  
write_rds(fraction_table, here("data/incubation_delta_values.rds"))

#calculate fraction of CO2 from litter C using avg delta13c----
 fraction_table_avg <- data_wide_avg %>% 
   select(-contains("NA")) %>% 
   drop_na(Moisture) %>% 
   mutate(f_c_co2 = (delta13c_Y - delta13c_N)/(delta_c_litter - delta13c_N))


#plot and stats for fraction of CO2 from litter----

# facet label rename water content
water_content_labels <- c(Low = "Low water content", High = "High water content")


# plots

delta_p <- ggplot(fraction_table, aes(Moisture, f_c_co2)) + 
  geom_boxplot(aes(fill = Treatment)) +
  facet_wrap(. ~ Time) + 
    geom_point(aes(Moisture, f_c_co2, fill = Treatment), 
             position = position_jitterdodge(jitter.width = 0.1), 
             shape = 21, size = 3) + 
  theme_bw() + 
  labs(y = expression("Fraction of CO"["2"]*" from labeled litter"),
       x = expression("Water content")) +
  scale_fill_brewer(palette = "Set1") + 
  mytheme
delta_p

# anova

aov_frac_litter <- aov(f_c_co2 ~ Moisture*Treatment*Time, 
                      data = fraction_table)

summary(aov_frac_litter)

options(knitr.kable.NA = '')

aov_frac_litter_table <- kable(anova(aov_frac_litter),
                              digits = c(1, 1, 1, 1, 4),
                              align = "lccccc",
                              caption = "3-way ANOVA for fraction of litter C in CO2") %>% 
  kable_styling()

aov_frac_litter_table

# contrasts 

em_litter_frac <- emmeans(aov_frac_litter, pairwise ~ Moisture | Time)

em_litter_frac_contrasts <- em_litter_frac$contrasts %>% 
  summary(infer = TRUE) %>% 
  as_data_frame()

litter_frac_contrasts_table <- kable(em_litter_frac_contrasts, 
                                    align = "lllccccccc",
                                    digits = c(2, 2, 2, 2, 2, 2, 2, 2, 4),
                                    caption = "Comparison of treatment means") %>% 
  kable_styling(full_width = T, position = "center")

litter_frac_contrasts_table


# plot fraction of CO2 from litter for treatment, water content, and days
# using avg delta13C values----

# delta_p_avg <- ggplot(fraction_table_avg, aes(Moisture, f_c_co2)) + 
#   geom_boxplot(aes(fill = Treatment)) +
#   facet_wrap(. ~ Time) + 
#   geom_point(aes(Moisture, f_c_co2, fill = Treatment), 
#              position = position_jitterdodge(jitter.width = 0.1), shape = 21, size = 3) + 
#   theme_bw() + 
#   labs(y = expression("Fraction of CO"["2"]*" from labeled litter"),
#        x = expression("Water content")) +
#   scale_fill_brewer(palette = "Set1") + mytheme
# delta_p_avg
# 
# ggsave(filename = here("plots/fraction_co2_from_litter.svg"), plot = delta_p_avg, width = 6, height = 5)



# plot(fraction_table$f_c_co2, fraction_table_avg$f_c_co2, type = "p")


# prepping mineralization data to calculate priming----
# pivoting to wide data.frames to grab litter vs no litter mineralization
# for beginning of incubation

beg_min_df <- read_rds(here("data/4day_min_data.rds")) %>% 
  select(c(jar_ID, Litter, Treatment, Moisture, rep, cum_mineralization)) %>% 
  pivot_wider(names_from = Litter, values_from = cum_mineralization, 
              id_cols = c(Treatment, Moisture, rep), 
              names_glue = "{.value}_{Litter}")  

Time = "Beginning"  

beg_min_df <- cbind(beg_min_df, Time)

# for end of incubation

end_min_df <- read_rds(here("data/total_min_data.rds")) %>% 
  select(c(jar_ID, Litter, Treatment, Moisture, rep, cum_mineralization)) %>% 
  pivot_wider(names_from = Litter, values_from = cum_mineralization, 
              id_cols = c(Treatment, Moisture, rep), 
              names_glue = "{.value}_{Litter}")

Time = "End"  

end_min_df <- cbind(end_min_df, Time) 

# prepping fraction of co2 that is labeled data

tidy_fraction_table <- fraction_table %>%  
  select(!(c(5:8)))

# calculating CO2 respired in 2nd part of incubation

end_min_df_diff <- rbind(beg_min_df, end_min_df) %>% 
  pivot_wider(names_from = Time, 
              values_from = c(4,5), 
              id_cols = c(Treatment, Moisture, rep)) %>% 
  mutate(cum_mineralization_N_2nd_part = cum_mineralization_N_End - cum_mineralization_N_Beginning,
         cum_mineralization_Y_2nd_part = cum_mineralization_Y_End - cum_mineralization_Y_Beginning)  

# prepping to join beginning data.frame

end_min_df_to_join <- end_min_df_diff %>% 
  select(!c(4:7)) %>% 
  rename(cum_mineralization_Y = cum_mineralization_Y_2nd_part,
         cum_mineralization_N = cum_mineralization_N_2nd_part)

end_min_df_to_join <- cbind(end_min_df_to_join, Time)

# joining beginning and end data and calculate priming

all_min_df <- rbind(beg_min_df, end_min_df_to_join) %>% 
  full_join(tidy_fraction_table) %>% 
  mutate(priming = (1-f_c_co2)*cum_mineralization_Y - cum_mineralization_N,
         CO2_13_C = f_c_co2 * cum_mineralization_Y)
write_rds(all_min_df, file = here("data/co2_mix_model_results.rds"))


# reorder factors
all_min_df$Moisture <- 
  factor(all_min_df$Moisture, levels = c("Low", "High"))

all_min_df$Treatment <- 
  factor(all_min_df$Treatment, levels = c("DW", "Ca", "Litter", "Blank"))

write_rds(all_min_df, here("data/incubation_isotopes_results.rds"))

# plot and stats for priming----

# plot

priming_plot <- 
  ggplot(all_min_df, aes(Time, priming, fill = Treatment)) + 
  geom_boxplot() + facet_wrap(. ~ Moisture) +
  geom_point(aes(Time, priming, fill = Treatment), 
             position = position_jitterdodge(jitter.width = 0.1), shape = 21, size = 3) +
  labs(y = expression(bold("SOC priming (mg CO"["2"]*"-C g"^"-1"*" soil)")),
       x = expression(bold("Sampling time"))) +
  scale_fill_brewer(palette = "Set1") + mytheme

priming_plot

ggsave(filename = here("plots/soc_priming.svg"), plot = priming_plot)

# anova for priming

aov_priming <- aov(priming ~ Moisture*Treatment*Time, 
                             data = all_min_df)

summary(aov_priming)

options(knitr.kable.NA = '')
aov_priming_table <- kable(anova(aov_priming),
                           digits = c(1, 1, 1, 1, 4),
                           align = "lccccc",
                           caption = "3-way ANOVA for priming") %>% 
  kable_styling()

aov_priming_table

# contrasts for priming

em_priming <- emmeans(aov_priming, pairwise ~ Treatment | Moisture*Time)

em_priming_contrasts <- em_priming$contrasts %>% 
  summary(infer = TRUE) %>% 
  as_data_frame()

priming_contrasts_table <- kable(em_priming_contrasts, 
                               align = "lllccccccc",
                               digits = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 4),
                               caption = "Comparison of treatment means") %>% 
  kable_styling(full_width = T, position = "center")

priming_contrasts_table

# plot and stats litter mineralized---- 

# plot

substrate_mineralized_plot <- 
  ggplot(all_min_df, aes(Time, CO2_13_C, fill = Treatment)) + 
  geom_boxplot() + 
  facet_wrap(. ~ Moisture, labeller = labeller(Moisture = water_content_labels)) +
  geom_point(aes(Time, CO2_13_C, fill = Treatment), 
             position = position_jitterdodge(jitter.width = 0.1), 
             shape = 21, size = 3) +
  labs(y = expression(bold("Litter mineralized (mg CO"["2"]*"-C g"^"-1"*" soil)")),
       x = expression(bold("Sampling time"))) +
  scale_fill_brewer(palette = "Set1") + mytheme

substrate_mineralized_plot

ggsave(filename = here("plots/substrate_mineralized.svg"), 
       plot = substrate_mineralized_plot,
       width = 8, height = 6)

# anova for litter mineralized

aov_litter_min <- aov(CO2_13_C ~ Moisture*Treatment*Time, 
                   data = all_min_df)

summary(aov_litter_min)

options(knitr.kable.NA = '')

aov_litter_min_table <- 
  kable(anova(aov_litter_min),
        digits = c(1, 1, 1, 1, 4),
        align = "lccccc",
        caption = "3-way ANOVA for amount of litter-C mineralized") %>% 
  kable_styling()

aov_litter_min_table

# contrasts for priming

em_litter_min <- emmeans(aov_litter_min, pairwise ~ Moisture | Treatment*Time)

em_litter_min_contrasts <- em_litter_min$contrasts %>% 
  summary(infer = TRUE) %>% 
  as_data_frame()

litter_min_contrasts_table <- 
  kable(em_litter_min_contrasts, 
        align = "lllccccccc",
        digits = c(1, 1, 1, 2, 2, 2, 2, 2, 2, 4),
        caption = "Comparison of means for amount of mineralized litter-C") %>% 
  kable_styling(full_width = T, position = "center")

litter_min_contrasts_table
