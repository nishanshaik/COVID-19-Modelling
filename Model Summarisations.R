### India Specific Summarisations =======================

# summary_0  <- summarise_scenario(scenario_0)
# summary_1  <- summarise_scenario(scenario_1)
# summary_2  <- summarise_scenario(scenario_2)
# summary_3  <- summarise_scenario(scenario_3)
# summary_4  <- summarise_scenario(scenario_4)
# summary_5  <- summarise_scenario(scenario_5)
# summary_6  <- summarise_scenario(scenario_6)
# summary_7  <- summarise_scenario(scenario_7)
# summary_8  <- summarise_scenario(scenario_8)
# summary_9  <- summarise_scenario(scenario_9)
# summary_10 <- summarise_scenario(scenario_10)

### Incidence ======================================
incidence_0  <- get_incidence(scenario_0)
incidence_1  <- get_incidence(scenario_1)
incidence_2  <- get_incidence(scenario_2)
# incidence_3  <- get_incidence(scenario_3)
# incidence_4  <- get_incidence(scenario_4)
# incidence_5  <- get_incidence(scenario_5)
# incidence_6  <- get_incidence(scenario_6)
# incidence_7  <- get_incidence(scenario_7)
# incidence_8  <- get_incidence(scenario_8)
# incidence_9  <- get_incidence(scenario_9)
# incidence_10 <- get_incidence(scenario_10)


incidence_0 <- incidence_0 %>% 
  mutate(Scenario = "No lockdown")
incidence_1 <- incidence_1 %>%
 mutate(Scenario = "Lockdown for 40 days with 50% compliance")
incidence_2 <- incidence_2 %>%
 mutate(Scenario = "Lockdown for 80 days with 50% compliance")
# incidence_3 <- incidence_3 %>% 
#   mutate(Scenario = "Lockdown for 80 days with 66% compliance")
# incidence_4 <- incidence_4 %>% 
#  mutate(Scenario = "Lockdown for 40 days with 50% compliance and 50% isolation efficiency")
# incidence_5 <- incidence_5 %>% 
#  mutate(Scenario = "Lockdown for 40 days with 50% compliance and 25% isolation efficiency")
# incidence_6 <- incidence_6 %>% 
#  mutate(Scenario = "Lockdown in July")
# incidence_7 <- incidence_7 %>% 
  # mutate(Scenario = "Scenario 7")
# incidence_8 <- incidence_8 %>% 
  # mutate(Scenario = "Scenario 8")
# incidence_9 <- incidence_9 %>% 
  # mutate(Scenario = "Scenario 9")
# incidence_10 <- incidence_10 %>% 
  # mutate(Scenario = "Scenario 10")


incidence <- bind_rows(incidence_0, 
                       incidence_1,
                       incidence_2,
                       # incidence_3,
                       # incidence_4,
                       # incidence_5,
                       # incidence_6
                       )

incidence <- incidence %>% 
  arrange(Date)

rm(incidence_0, 
   incidence_1,
   incidence_2,
   # incidence_3,
   # incidence_4,
   # incidence_5,
   # incidence_6
   )

incidence$Indicator[incidence$Indicator == "Mean_inc"] <- "Mean Incidence"
incidence$Indicator[incidence$Indicator == "Median_inc"] <- "Median Incidence"
incidence$Indicator[incidence$Indicator == "SD_inc"] <- "Std. Dev Incidence"
incidence$Indicator[incidence$Indicator == "IQR_inc"] <- "IQR Incidence"
incidence$Indicator[incidence$Indicator == "Mean_cuminc"] <- "Mean Cumulative Incidence"
incidence$Indicator[incidence$Indicator == "Median_cuminc"] <- "Median Cumulative Incidence"
incidence$Indicator[incidence$Indicator == "SD_cuminc"] <- "Std. Dev Cumulative Incidence"
incidence$Indicator[incidence$Indicator == "IQR_cuminc"] <- "IQR Cumulative Incidence"

### Prevalence =========================================
prevalence_0  <- get_prevalence(scenario_0)
prevalence_1  <- get_prevalence(scenario_1)
prevalence_2  <- get_prevalence(scenario_2)
# prevalence_3  <- get_prevalence(scenario_3)
# prevalence_4  <- get_prevalence(scenario_4)
# prevalence_5  <- get_prevalence(scenario_5)
# prevalence_6  <- get_prevalence(scenario_6)
# prevalence_7  <- get_prevalence(scenario_7)
# prevalence_8  <- get_prevalence(scenario_8)
# prevalence_9  <- get_prevalence(scenario_9)
# prevalence_10 <- get_prevalence(scenario_10)


prevalence_0 <- prevalence_0 %>% 
  mutate(Scenario = "No lockdown")
prevalence_1 <- prevalence_1 %>%
 mutate(Scenario = "Lockdown for 40 days with 50% compliance")
prevalence_2 <- prevalence_2 %>%
 mutate(Scenario = "Lockdown for 80 days with 50% compliance")
# prevalence_3 <- prevalence_3 %>% 
#   mutate(Scenario = "Lockdown for 80 days with 66% compliance")
# prevalence_4 <- prevalence_4 %>% 
#  mutate(Scenario = "Lockdown for 40 days with 50% compliance and 50% isolation efficiency")
# prevalence_5 <- prevalence_5 %>% 
#  mutate(Scenario = "Lockdown for 40 days with 50% compliance and 25% isolation efficiency")
# prevalence_6 <- prevalence_6 %>% 
 # mutate(Scenario = "Lockdown in July")
# prevalence_7 <- prevalence_7 %>% 
# mutate(Scenario = "Scenario 7")
# prevalence_8 <- prevalence_8 %>% 
# mutate(Scenario = "Scenario 8")
# prevalence_9 <- prevalence_9 %>% 
# mutate(Scenario = "Scenario 9")
# prevalence_10 <- prevalence_10 %>% 
# mutate(Scenario = "Scenario 10")


prevalence <- bind_rows(prevalence_0, 
                        prevalence_1,
                        prevalence_2,
                        # prevalence_3,
                        # prevalence_4,
                        # prevalence_5,
                        # prevalence_6
                        )

prevalence <- prevalence %>% 
  arrange(Date)

rm(prevalence_0, 
   prevalence_1,
   prevalence_2,
   # prevalence_3,
   # prevalence_4,
   # prevalence_5,
   # prevalence_6
   )

prevalence$Indicator[prevalence$Indicator == "Mean_prev"] <- "Mean prevalence"
prevalence$Indicator[prevalence$Indicator == "Median_prev"] <- "Median prevalence"
prevalence$Indicator[prevalence$Indicator == "SD_prev"] <- "Std. Dev prevalence"
prevalence$Indicator[prevalence$Indicator == "IQR_prev"] <- "IQR prevalence"


### Cleaning Up the Environment ===================
rm(contacts, indiapop, R0_dates, initialI, durInfSim, nsim, R0est, r0postCrI, r0posterior, sim)
### Exploratory Plots =========================
## Daily incidence plot with Mean
library(ggrepel)
ggplot() +
  geom_line(data = summary_0, aes(x = Date, y = Mean_inc), colour = "firebrick3", size = 1) +
  geom_line(data = summary_1, aes(x = Date, y = Mean_inc), colour = "hotpink", size = 1) +
  geom_line(data = summary_2, aes(x = Date, y = Mean_inc), colour = "deepskyblue2", size = 1) +
  geom_line(data = summary_3, aes(x = Date, y = Mean_inc), colour = "forestgreen", size = 1) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))

## Cumulative Incidence Plot with Mean
ggplot() +
  geom_line(data = summary_0, aes(x = Date, y = CumInc), colour = "firebrick3", size = 1) +
  geom_line(data = summary_1, aes(x = Date, y = CumInc), colour = "hotpink", size = 1) +
  geom_line(data = summary_2, aes(x = Date, y = CumInc), colour = "deepskyblue2", size = 1) +
  geom_line(data = summary_3, aes(x = Date, y = CumInc), colour = "forestgreen", size = 1) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))

## Daily Incidence with Median
ggplot() +
  geom_line(data = summary_0, aes(x = Date, y = Median_inc), colour = "firebrick3", size = 1) +
  geom_line(data = summary_1, aes(x = Date, y = Median_inc), colour = "hotpink", size = 1) +
  geom_line(data = summary_2, aes(x = Date, y = Median_inc), colour = "deepskyblue2", size = 1) +
  geom_line(data = summary_3, aes(x = Date, y = Median_inc), colour = "forestgreen", size = 1) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))

## Cumulative Incidence Plot with Median
ggplot() +
  geom_line(data = summary_0, aes(x = Date, y = CumInc_median), colour = "firebrick3", size = 1) +
  geom_line(data = summary_1, aes(x = Date, y = CumInc_median), colour = "hotpink", size = 1) +
  geom_line(data = summary_2, aes(x = Date, y = CumInc_median), colour = "deepskyblue2", size = 1) +
  geom_line(data = summary_3, aes(x = Date, y = CumInc_median), colour = "forestgreen", size = 1) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1))

## Plots with Incidence =============================
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)
library(scales)
library(svglite)
library(ggdark)
library(plotly)

daily_incidence <- incidence %>% 
  filter(Indicator %in% c("Mean Incidence")) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Value, colour = Scenario)) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  labs(title = "Mean Daily Incidence in COVID-19 Cases in India",
       subtitle = "CONFIDENTIAL",
       caption = "Based on SEIR dynamic simulation",
       color = "Scenarios") +
  ylab("Daily Incidence of COVID-19 cases") +
  xlab("Date") +
  scale_y_continuous(breaks = round(seq(0, 2000000, by = 100000)), label = comma) +
  dark_theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

daily_incidence

ggplotly(daily_incidence)
  
       




figure_2 <- incidence %>% 
  filter(Indicator %in% c("Mean Incidence") & Scenario != "Lockdown for 80 days with 66% compliance") %>% 
  filter(Date <= dmy("01-03-2023")) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Value, colour = Scenario)) +
  scale_x_date(breaks = breaks_pretty(36)) +
  scale_y_continuous(breaks = round(seq(0, 
                                        2500000, 
                                        by = 100000), 0)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Daily Incidence")

figure_2


figure_3 <- incidence %>% 
  filter(Indicator %in% c("Mean Cumulative Incidence") & Scenario != "Lockdown for 80 days with 66% compliance") %>% 
  filter(Date <= dmy("01-03-2023")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Value / 10000000, colour = Scenario)) +
  scale_x_date(breaks = breaks_pretty(35)) +
  scale_y_continuous(breaks = round(seq(0, 
                                        30, 
                                        by = 1), 0)) + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Cumulative Incidence (in crores)")

figure_3




incidence %>% 
  filter(Indicator %in% c("Median Incidence")) %>%
  filter(Date <= dmy("01-03-2023")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Value, colour = Scenario)) +
  scale_x_date(breaks = breaks_pretty(15)) +
  scale_y_continuous(breaks = round(seq(0, 
                                        6000000, 
                                        by = 250000), 0)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Daily Incidence")



incidence %>% 
  filter(Indicator %in% c("Median Cumulative Incidence")) %>% 
  filter(Date <= dmy("01-03-2021")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Value / 10000000, colour = Scenario)) +
  scale_x_date(breaks = breaks_pretty(15)) +
  scale_y_continuous(breaks = round(seq(0, 
                                        90, 
                                        by = 10), 0))+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Cumulative Incidence in crores")



incidence %>% 
  filter(Indicator %in% c("Median Incidence", "Max_inc", "Min_inc","Q25_inc", "Q75_inc")) %>% 
  spread(Indicator, Value) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = `Median Incidence`, colour = Scenario)) +
  geom_ribbon(aes(ymin = Q25_inc, 
                  ymax = Q75_inc, 
                  fill = Scenario), alpha = 0.2) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Median Incidence")


incidence %>% 
  filter(Indicator %in% c("Median Cumulative Incidence", "IQR Cumulative Incidence") & Scenario != "Lockdown for 80 days with 66% compliance") %>% 
  spread(Indicator, Value) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = `Median Cumulative Incidence`, colour = Scenario)) +
  geom_ribbon(aes(ymin = ifelse(`Median Cumulative Incidence` - `IQR Cumulative Incidence`/2 < 0, 0, `Median Cumulative Incidence` - `IQR Cumulative Incidence`/2),
                  ymax = `Median Cumulative Incidence` + `IQR Cumulative Incidence`/2, 
                  fill = Scenario), alpha = 0.05) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Median Cumulative Incidence")


incidence %>% 
  filter(Indicator %in% c("Mean Cumulative Incidence", "Std. Dev Cumulative Incidence") & Scenario %in% c("Scenario 3", "Scenario 5", "Scenario 7")) %>% 
  spread(Indicator, Value) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = `Mean Cumulative Incidence`, colour = Scenario)) +
  geom_ribbon(aes(ymin = ifelse(`Mean Cumulative Incidence` - `Std. Dev Cumulative Incidence` < 0, 0, `Mean Cumulative Incidence` - `Std. Dev Cumulative Incidence`),
                  ymax = `Mean Cumulative Incidence` + `Std. Dev Cumulative Incidence`, fill = Scenario), alpha = 0.1) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Cumulative Incidence")



incidence %>% 
  filter(Indicator %in% c("Mean Incidence", "Max_inc", "Min_inc","Q25_inc", "Q75_inc")) %>% 
  spread(Indicator, Value) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = `Mean Incidence`, colour = Scenario)) +
  geom_ribbon(aes(ymin = Q25_inc, 
                  ymax = Q75_inc, 
                  fill = Scenario), alpha = 0.2) +
  scale_x_date(breaks = breaks_pretty(35)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Incidence")

## Plots with Prevalence ==========================

figure_4 <- prevalence %>% 
  filter(Indicator %in% c("Mean prevalence") & Scenario != "Lockdown for 80 days with 66% compliance") %>% 
  filter(Date <= dmy("01-03-2023")) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Value / 100000, colour = Scenario)) +
  geom_line(aes(y = bed_count / 100000), colour = "dimgray", linetype = "longdash") +
  scale_x_date(breaks = breaks_pretty(36)) +
  scale_y_continuous(breaks = round(seq(0, 
                                        200, 
                                        by = 10), 0)) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  ylab("Prevalence (in lakhs)") 
  annotate(geom = "text", x = dmy("01-10-2022"), y = 13, label = "Total number of beds", colour = "dimgray")
  
figure_4

### Peaksize and Peaktimes for scenario (Incidence) =================================
scen_0 <- incidence %>% 
  filter(Scenario == "No lockdown" & Indicator == "Mean Incidence")
max(scen_0$Value) ## Peak size
scen_0$Date[scen_0$Value == max(scen_0$Value)] ## Peak time

scen_1 <- incidence %>% 
  filter(Scenario == "Lockdown for 40 days with 50% compliance" & Indicator == "Mean Incidence")
max(scen_1$Value) ## Peak size
scen_1$Date[scen_1$Value == max(scen_1$Value)] ## Peak time

scen_2 <- incidence %>% 
  filter(Scenario == "Lockdown for 80 days with 50% compliance" & Indicator == "Mean Incidence")
max(scen_2$Value) ## Peak size
scen_2$Date[scen_2$Value == max(scen_2$Value)] ## Peak time

scen_3 <- incidence %>% 
  filter(Scenario == "Lockdown for 80 days with 66% compliance" & Indicator == "Mean Incidence")
max(scen_3$Value) ## Peak size
scen_3$Date[scen_3$Value == max(scen_3$Value)] ## Peak time

scen_4 <- incidence %>% 
  filter(Scenario == "Lockdown for 40 days with 50% compliance and 50% isolation efficiency" & Indicator == "Mean Incidence")
max(scen_4$Value) ## Peak size
scen_4$Date[scen_4$Value == max(scen_4$Value)] ## Peak time

scen_5 <- incidence %>% 
  filter(Scenario == "Lockdown for 40 days with 50% compliance and 25% isolation efficiency" & Indicator == "Mean Incidence")
max(scen_5$Value) ## Peak size
scen_5$Date[scen_5$Value == max(scen_5$Value)] ## Peak time

(max(scen_0$Value) - max(scen_4$Value)) * 100 / max(scen_0$Value)

scen_4$Date[scen_4$Value == max(scen_4$Value)] - scen_0$Date[scen_0$Value == max(scen_0$Value)]

### Cumulative Incidence at 4 time points ==============================
scen_01 <- incidence %>% 
  filter(Date %in% c(dmy("01-09-2020"), 
                     dmy("01-03-2021"), 
                     dmy("01-03-2022")) & Scenario != "Lockdown for 80 days with 66% compliance") %>% 
  filter(Indicator == "Mean Cumulative Incidence") %>% 
  select(-Index, -Indicator) %>% 
  pivot_wider(id_cols = Scenario, names_from = Date, values_from = Value) %>% 
  rename(`1 September 2020` = `2020-09-01`,
         `1 March 2021` = `2021-03-01`,
         `1 March 2022` = `2022-03-01`) %>% 
  arrange(desc(`1 March 2022`))
View(scen_01) 
write.csv(scen_01, "~/MeanCumInc.csv")



scen_y <- incidence %>% 
  filter(Date %in% c(dmy("30-06-2020"), dmy("31-12-2020"), dmy("01-03-2021"))) %>% 
  filter(Indicator == "Median Cumulative Incidence") 
View(scen_y) 
write.csv(scen_y, "~/MedianCumInc.csv") 

scen_x <- incidence %>% 
  filter(Date <= dmy("15-04-2020")) %>% 
  filter(Indicator %in% c("Mean Cumulative Incidence", "Mean Incidence", "Median Cumulative Incidence", "Median Incidence")) %>% 
  filter(Scenario == "Three week lockdown with 50% compliance") 
View(scen_x)



### Peaksize and Peaktimes for scenario (Prevalence) =================================

prev_0 <- prevalence %>% 
  filter(Scenario == "No lockdown" & Indicator == "Mean prevalence")
max(prev_0$Value) ## Peak size
prev_0$Date[prev_0$Value == max(prev_0$Value)] ## Peak time

prev_1 <- prevalence %>% 
  filter(Scenario == "Lockdown for 40 days with 50% compliance" & Indicator == "Mean prevalence")
max(prev_1$Value) ## Peak size
prev_1$Date[prev_1$Value == max(prev_1$Value)] ## Peak time

prev_2 <- prevalence %>% 
  filter(Scenario == "Lockdown for 80 days with 50% compliance" & Indicator == "Mean prevalence")
max(prev_2$Value) ## Peak size
prev_2$Date[prev_2$Value == max(prev_2$Value)] ## Peak time

prev_3 <- prevalence %>% 
  filter(Scenario == "Lockdown for 80 days with 66% compliance" & Indicator == "Mean prevalence")
max(prev_3$Value) ## Peak size
prev_3$Date[prev_3$Value == max(prev_3$Value)] ## Peak time

prev_4 <- prevalence %>% 
  filter(Scenario == "Lockdown for 40 days with 50% compliance and 50% isolation efficiency" & Indicator == "Mean prevalence")
max(prev_4$Value) ## Peak size
prev_4$Date[prev_4$Value == max(prev_4$Value)] ## Peak time

prev_5 <- prevalence %>% 
  filter(Scenario == "Lockdown for 40 days with 50% compliance and 25% isolation efficiency" & Indicator == "Mean prevalence")
max(prev_5$Value) ## Peak size
prev_5$Date[prev_5$Value == max(prev_5$Value)] ## Peak time


(max(prev_0$Value) - max(prev_4$Value)) * 100 / max(prev_0$Value)

prev_4$Date[prev_4$Value == max(prev_4$Value)] - prev_0$Date[prev_0$Value == max(prev_0$Value)]

### Prevalence Exceeding Health System Capacity ===========================
overwhelm <- prevalence %>% 
  filter(Indicator == "Mean prevalence") %>% 
  filter(Value >= bed_count) %>% 
  mutate(Value = Value - bed_count) %>% 
  group_by(Scenario) %>% 
  summarise(Start = min(Date),
            End = max(Date),
            AUC = sum(Value)) %>% 
  arrange(Start) %>% 
  mutate(Duration = as.numeric(End - Start),
         AUC_Rank = rank(AUC)) %>% 
  select(-End, -AUC)

overwhelm

write.csv(overwhelm, "~/Overwhelm.csv")

### Percentage Changes ======================================

## Change in peak incidence
(max(scen_0$Value) - max(scen_4$Value)) * 100 / max(scen_0$Value)


## Delay in peak incidence
scen_4$Date[scen_4$Value == max(scen_4$Value)] - scen_0$Date[scen_0$Value == max(scen_0$Value)] 


## Change in peak prevalence
(max(prev_0$Value) - max(prev_4$Value)) * 100 / max(prev_0$Value) 

## Delay in peak prevalence
prev_4$Date[prev_4$Value == max(prev_4$Value)] - prev_0$Date[prev_0$Value == max(prev_0$Value)]

sep1_Max <- scen_01$`1 September 2020`[1]
sep1_Min <- scen_01$`1 September 2020`[length(scen_01$Scenario)]

mar21_Max <- scen_01$`1 March 2021`[1]
mar21_Min <- scen_01$`1 March 2021`[length(scen_01$Scenario)]

mar22_Max <- scen_01$`1 March 2022`[1]
mar22_Min <- scen_01$`1 March 2022`[length(scen_01$Scenario)]

## Change at 6 months cumulatie incidence
(sep1_Max - sep1_Min) * 100 / sep1_Max

## Change at 1 year cumulative incidence
(mar21_Max - mar21_Min) * 100 / mar21_Max

## Change at 2 years cumulative incidence
(mar22_Max - mar22_Min) * 100 / mar22_Max

### Monitoring Dataset ----
monitoring <- incidence %>% filter(Indicator == "Mean Cumulative Incidence" & Date >= dmy("13-04-2020") & Date <= Sys.Date() + 7)
### Generating Output Dataset ----

# oIncidence <- pivot_wider(incidence, names_from = "Indicator", values_from = "Value")
# oIncidence <- oIncidence %>% 
#   unite("id", c("Index", "Scenario"), sep = "/", remove = FALSE)
# 
# 
# oPrevalence <- pivot_wider(prevalence, names_from = "Indicator", values_from = "Value")
# oPrevalence <- oPrevalence %>% 
#   unite("id", c("Index", "Scenario"), sep = "/", remove = FALSE)
# 
# simulations <- left_join(oIncidence, oPrevalence, by = "id")
# simulations <- simulations %>% 
#   select(-id, -Date.y, -Index.y, -Scenario.y) %>% 
#   rename(Index = Index.x,
#          Date = Date.x,
#          Scenario = Scenario.x)
# 
# write.csv(simulations, "~/simulations.csv")
# 
# rm(list = ls())
