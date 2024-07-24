## Regression - MeinLUKS data
## Interrupted time series analysis: Gender groups
## July 2024
## -----------------------------------------------------------------------------


rm(list = ls())
Sys.setenv(LANG = "en")
options(scipen = 999) # remove scientific notation

library(tidyverse)
library(nlme)

setwd("C:/Daten/MSc Health Sciences/10 Internship/3 MeinLUKS/R")
data_gender <- read.csv2("FLPerDay_gender.csv") %>% 
  rename(gender = sex)
str(data_gender)


## Background ------------------------------------------------------------------

# https://ds4ps.org/pe4ps-textbook/docs/p-020-time-series.html

# Y: FL_PerDay
# T = time: FL_AfterRelease_days
# D = treatment: intervention_t1 / intervention_t2 / intervention_t3 / intervention_t4 / intervention_t5
# P = time since treatment: post_intervention_t1 / post_intervention_t2 / post_intervention_t3 / post_intervention_t4 / post_intervention_t5

# b0: baseline level of daily registrations
# b1*T: slope before intervention
# b2*D: immediate effect occurring after the intervention
# b3*P: difference between the slope before and after the intervention (i.e., sustained effect of the intervention)

# counterfactual: Y = b0 + b1*T + e
# b2 = 0, b3 = 0


## Preparation: general --------------------------------------------------------

T1 <- 143
T2 <- 470
T3 <- 652
T4 <- 704
T5 <- 808
T6 <- 974

Date0 <- as.Date("2019-12-01")
Date1 <- as.Date("2020-04-22")
Date2 <- as.Date("2021-03-15")
Date3 <- as.Date("2021-09-13")
Date4 <- as.Date("2021-11-04")
Date5 <- as.Date("2022-02-16")

data_adv <- data_gender %>% 
  select(Date, gender, FL_7dm_gender, TimePeriods) %>% 
  mutate(Date = as.Date(Date), 
         FL_7dm_gender = as.numeric(FL_7dm_gender), 
         TimePeriods = factor(TimePeriods)) %>% 
  mutate(Time_days = as.numeric(Date - Date0)) %>% 
  mutate(intervention_t1 = ifelse(Date < Date1, 0, 1), 
         intervention_t2 = ifelse(Date < Date2, 0, 1), 
         intervention_t3 = ifelse(Date < Date3, 0, 1), 
         intervention_t4 = ifelse(Date < Date4, 0, 1), 
         intervention_t5 = ifelse(Date < Date5, 0, 1)) %>% 
  mutate(post_intervention_t1 = ifelse(intervention_t1 == 0, 0, Time_days - T1 + 1), 
         post_intervention_t2 = ifelse(intervention_t2 == 0, 0, Time_days - T2 + 1),
         post_intervention_t3 = ifelse(intervention_t3 == 0, 0, Time_days - T3 + 1),
         post_intervention_t4 = ifelse(intervention_t4 == 0, 0, Time_days - T4 + 1),
         post_intervention_t5 = ifelse(intervention_t5 == 0, 0, Time_days - T5 + 1))
str(data_adv)


## Preparation: female ---------------------------------------------------------

data_adv_female <- data_adv %>% 
  filter(gender == "female")

plot0 <- ggplot(data_adv_female, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_vline(xintercept = Date1, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date2, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date3, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date4, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date5, color = "firebrick", linetype = "dashed") + 
  labs(title = "Patient portal registrations of female patients over the study period", 
       y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250)

## Model 1 ---------------------------------------------------------------------

data_adv_female1 <- data_adv_female %>% 
  filter(Time_days < T2, 
         gender == "female")
data_adv_female1b <- data_adv_female1 %>% 
  mutate(intervention_t1 = 0, 
         post_intervention_t1 = 0)

model_t1 <- lm(FL_7dm_gender ~ Time_days + intervention_t1 + post_intervention_t1, 
               data = data_adv_female1)
summary(model_t1)
data_adv_female1$predicted <- predict(model_t1, data_adv_female1) # predict actual values
data_adv_female1$counterfactual <- predict(model_t1, data_adv_female1b) # predict counterfactual values

# Prepare data for ggplot
data_adv_female1 <- data_adv_female1 %>%
  mutate(color = case_when(Time_days < T1 ~ "dodgerblue",
                           Time_days >= T1 ~ "darkblue"), 
         counterfactual = if_else(intervention_t1 == 0, NA_real_, counterfactual))
plot1 <- ggplot(data_adv_female1, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t1)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date1, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P1 vs. P2", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))

## Model 2 ---------------------------------------------------------------------

data_adv_female2 <- data_adv_female %>% 
  filter(Time_days > T1,
         Time_days < T3, 
         gender == "female")
data_adv_female2b <- data_adv_female2 %>% 
  mutate(intervention_t2 = 0, 
         post_intervention_t2 = 0)

model_t2 <- lm(FL_7dm_gender ~ Time_days + intervention_t2 + post_intervention_t2, 
               data = data_adv_female2)
summary(model_t2)
data_adv_female2$predicted <- predict(model_t2, data_adv_female2) # predict actual values
data_adv_female2$counterfactual <- predict(model_t2, data_adv_female2b) # predict counterfactual values

# Prepare data for ggplot
data_adv_female2 <- data_adv_female2 %>%
  mutate(color = case_when(Time_days < T2 ~ "dodgerblue",
                           Time_days >= T2 ~ "darkblue"), 
         counterfactual = if_else(intervention_t2 == 0, NA_real_, counterfactual))
plot2 <- ggplot(data_adv_female2, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t2)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date2, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P2 vs. P3", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))


## Model 3 ---------------------------------------------------------------------

data_adv_female3 <- data_adv_female %>% 
  filter(Time_days > T2,
         Time_days < T4, 
         gender == "female")
data_adv_female3b <- data_adv_female3 %>% 
  mutate(intervention_t3 = 0, 
         post_intervention_t3 = 0)

model_t3 <- lm(FL_7dm_gender ~ Time_days + intervention_t3 + post_intervention_t3, 
               data = data_adv_female3)
summary(model_t3)
data_adv_female3$predicted <- predict(model_t3, data_adv_female3) # predict actual values
data_adv_female3$counterfactual <- predict(model_t3, data_adv_female3b) # predict counterfactual values

# Prepare data for ggplot
data_adv_female3 <- data_adv_female3 %>%
  mutate(color = case_when(Time_days < T3 ~ "dodgerblue",
                           Time_days >= T3 ~ "darkblue"), 
         counterfactual = if_else(intervention_t3 == 0, NA_real_, counterfactual))
plot3 <- ggplot(data_adv_female3, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t3)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date3, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P3 vs. P4", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))

## Model 4 ---------------------------------------------------------------------

data_adv_female4 <- data_adv_female %>% 
  filter(Time_days > T3,
         Time_days < T5, 
         gender == "female")
data_adv_female4b <- data_adv_female4 %>% 
  mutate(intervention_t4 = 0, 
         post_intervention_t4 = 0)

model_t4 <- lm(FL_7dm_gender ~ Time_days + intervention_t4 + post_intervention_t4, 
               data = data_adv_female4)
summary(model_t4)
data_adv_female4$predicted <- predict(model_t4, data_adv_female4) # predict actual values
data_adv_female4$counterfactual <- predict(model_t4, data_adv_female4b) # predict counterfactual values

# Prepare data for ggplot
data_adv_female4 <- data_adv_female4 %>%
  mutate(color = case_when(Time_days < T4 ~ "dodgerblue",
                           Time_days >= T4 ~ "darkblue"), 
         counterfactual = if_else(intervention_t4 == 0, NA_real_, counterfactual))
plot4 <- ggplot(data_adv_female4, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t4)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date4, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P4 vs. P5", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))


## Model 5 ---------------------------------------------------------------------

data_adv_female5 <- data_adv_female %>% 
  filter(Time_days > T4, 
         gender == "female")
data_adv_female5b <- data_adv_female5 %>% 
  mutate(intervention_t5 = 0, 
         post_intervention_t5 = 0)

model_t5 <- lm(FL_7dm_gender ~ Time_days + intervention_t5 + post_intervention_t5, 
               data = data_adv_female5)
summary(model_t5)
data_adv_female5$predicted <- predict(model_t5, data_adv_female5) # predict actual values
data_adv_female5$counterfactual <- predict(model_t5, data_adv_female5b) # predict counterfactual values

# Prepare data for ggplot
data_adv_female5 <- data_adv_female5 %>%
  mutate(color = case_when(Time_days < T5 ~ "dodgerblue",
                           Time_days >= T5 ~ "darkblue"), 
         counterfactual = if_else(intervention_t5 == 0, NA_real_, counterfactual))
plot5 <- ggplot(data_adv_female5, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t5)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date5, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P5 vs. P6", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))



## Summary ---------------------------------------------------------------------

library(broom)
library(dplyr)
library(tidyr)
library(kableExtra)  # For a nicely formatted table

# Tidy model outputs
Model1 <- tidy(model_t1, conf.int = TRUE) %>% 
  mutate(model = "P1 vs. P2")
Model2 <- tidy(model_t2, conf.int = TRUE) %>% 
  mutate(model = "P2 vs. P3")
Model3 <- tidy(model_t3, conf.int = TRUE) %>% 
  mutate(model = "P3 vs. P4")
Model4 <- tidy(model_t4, conf.int = TRUE) %>% 
  mutate(model = "P4 vs. P5")
Model5 <- tidy(model_t5, conf.int = TRUE) %>% 
  mutate(model = "P5 vs. P6")

# Combine outputs
combined_tidy <- bind_rows(Model1, Model2, Model3, Model4, Model5)
combined_tidy <- combined_tidy %>% 
  mutate(term = recode(term, 
                       "(Intercept)" = "1_Intercept", 
                       "Time_days" = "2_Slope (pre-intervention trend)", 
                       "intervention_t1" = "3_Level change (immediate effect)", 
                       "intervention_t2" = "3_Level change (immediate effect)", 
                       "intervention_t3" = "3_Level change (immediate effect)", 
                       "intervention_t4" = "3_Level change (immediate effect)", 
                       "intervention_t5" = "3_Level change (immediate effect)", 
                       "post_intervention_t1" = "4_Slope change (sustained effect)",
                       "post_intervention_t2" = "4_Slope change (sustained effect)",
                       "post_intervention_t3" = "4_Slope change (sustained effect)",
                       "post_intervention_t4" = "4_Slope change (sustained effect)",
                       "post_intervention_t5" = "4_Slope change (sustained effect)"), 
         estimate = sprintf("%.2f", round(estimate, 2)),
         conf.int = paste0("[", sprintf("%.2f", round(conf.low, 2)), ", ", 
                           sprintf("%.2f", round(conf.high, 2)), "]"),
         p.value_raw = sprintf("%.6f", round(p.value, 6)), 
         p.value_rounded = sprintf("%.2f", round(p.value, 2)))

# Select and arrange columns
combined_tidy <- combined_tidy %>%
  select(model, term, estimate, conf.int, p.value_rounded, p.value_raw) %>%
  arrange(model, term)

# Create a nicely formatted table
combined_tidy %>%
  kable("html", caption = "Interrupted Time Series Analysis") %>%
  kable_styling(full_width = FALSE)
write.table(combined_tidy, file = "InterruptedTimeSeriesAnalysis_female.csv", row.names = FALSE, sep = ";", na = "")



## All figures combined --------------------------------------------------------

library(gridExtra)

grid.arrange(plot0, plot1, plot2, nrow = 3)
grid.arrange(plot3, plot4, plot5, nrow = 3)











## Preparation: male -----------------------------------------------------------

data_adv_male <- data_adv %>% 
  filter(gender == "male")

plot0 <- ggplot(data_adv_male, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_vline(xintercept = Date1, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date2, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date3, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date4, color = "firebrick", linetype = "dashed") + 
  geom_vline(xintercept = Date5, color = "firebrick", linetype = "dashed") + 
  labs(title = "Patient portal registrations of male patients over the study period", 
       y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250)

## Model 1 ---------------------------------------------------------------------

data_adv_male1 <- data_adv_male %>% 
  filter(Time_days < T2, 
         gender == "male")
data_adv_male1b <- data_adv_male1 %>% 
  mutate(intervention_t1 = 0, 
         post_intervention_t1 = 0)

model_t1 <- lm(FL_7dm_gender ~ Time_days + intervention_t1 + post_intervention_t1, 
               data = data_adv_male1)
summary(model_t1)
data_adv_male1$predicted <- predict(model_t1, data_adv_male1) # predict actual values
data_adv_male1$counterfactual <- predict(model_t1, data_adv_male1b) # predict counterfactual values

# Prepare data for ggplot
data_adv_male1 <- data_adv_male1 %>%
  mutate(color = case_when(Time_days < T1 ~ "dodgerblue",
                           Time_days >= T1 ~ "darkblue"), 
         counterfactual = if_else(intervention_t1 == 0, NA_real_, counterfactual))
plot1 <- ggplot(data_adv_male1, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t1)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date1, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P1 vs. P2", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))

## Model 2 ---------------------------------------------------------------------

data_adv_male2 <- data_adv_male %>% 
  filter(Time_days > T1,
         Time_days < T3, 
         gender == "male")
data_adv_male2b <- data_adv_male2 %>% 
  mutate(intervention_t2 = 0, 
         post_intervention_t2 = 0)

model_t2 <- lm(FL_7dm_gender ~ Time_days + intervention_t2 + post_intervention_t2, 
               data = data_adv_male2)
summary(model_t2)
data_adv_male2$predicted <- predict(model_t2, data_adv_male2) # predict actual values
data_adv_male2$counterfactual <- predict(model_t2, data_adv_male2b) # predict counterfactual values

# Prepare data for ggplot
data_adv_male2 <- data_adv_male2 %>%
  mutate(color = case_when(Time_days < T2 ~ "dodgerblue",
                           Time_days >= T2 ~ "darkblue"), 
         counterfactual = if_else(intervention_t2 == 0, NA_real_, counterfactual))
plot2 <- ggplot(data_adv_male2, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t2)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date2, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P2 vs. P3", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))


## Model 3 ---------------------------------------------------------------------

data_adv_male3 <- data_adv_male %>% 
  filter(Time_days > T2,
         Time_days < T4, 
         gender == "male")
data_adv_male3b <- data_adv_male3 %>% 
  mutate(intervention_t3 = 0, 
         post_intervention_t3 = 0)

model_t3 <- lm(FL_7dm_gender ~ Time_days + intervention_t3 + post_intervention_t3, 
               data = data_adv_male3)
summary(model_t3)
data_adv_male3$predicted <- predict(model_t3, data_adv_male3) # predict actual values
data_adv_male3$counterfactual <- predict(model_t3, data_adv_male3b) # predict counterfactual values

# Prepare data for ggplot
data_adv_male3 <- data_adv_male3 %>%
  mutate(color = case_when(Time_days < T3 ~ "dodgerblue",
                           Time_days >= T3 ~ "darkblue"), 
         counterfactual = if_else(intervention_t3 == 0, NA_real_, counterfactual))
plot3 <- ggplot(data_adv_male3, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t3)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date3, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P3 vs. P4", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))

## Model 4 ---------------------------------------------------------------------

data_adv_male4 <- data_adv_male %>% 
  filter(Time_days > T3,
         Time_days < T5, 
         gender == "male")
data_adv_male4b <- data_adv_male4 %>% 
  mutate(intervention_t4 = 0, 
         post_intervention_t4 = 0)

model_t4 <- lm(FL_7dm_gender ~ Time_days + intervention_t4 + post_intervention_t4, 
               data = data_adv_male4)
summary(model_t4)
data_adv_male4$predicted <- predict(model_t4, data_adv_male4) # predict actual values
data_adv_male4$counterfactual <- predict(model_t4, data_adv_male4b) # predict counterfactual values

# Prepare data for ggplot
data_adv_male4 <- data_adv_male4 %>%
  mutate(color = case_when(Time_days < T4 ~ "dodgerblue",
                           Time_days >= T4 ~ "darkblue"), 
         counterfactual = if_else(intervention_t4 == 0, NA_real_, counterfactual))
plot4 <- ggplot(data_adv_male4, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t4)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date4, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P4 vs. P5", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))


## Model 5 ---------------------------------------------------------------------

data_adv_male5 <- data_adv_male %>% 
  filter(Time_days > T4, 
         gender == "male")
data_adv_male5b <- data_adv_male5 %>% 
  mutate(intervention_t5 = 0, 
         post_intervention_t5 = 0)

model_t5 <- lm(FL_7dm_gender ~ Time_days + intervention_t5 + post_intervention_t5, 
               data = data_adv_male5)
summary(model_t5)
data_adv_male5$predicted <- predict(model_t5, data_adv_male5) # predict actual values
data_adv_male5$counterfactual <- predict(model_t5, data_adv_male5b) # predict counterfactual values

# Prepare data for ggplot
data_adv_male5 <- data_adv_male5 %>%
  mutate(color = case_when(Time_days < T5 ~ "dodgerblue",
                           Time_days >= T5 ~ "darkblue"), 
         counterfactual = if_else(intervention_t5 == 0, NA_real_, counterfactual))
plot5 <- ggplot(data_adv_male5, aes(x = Date, y = FL_7dm_gender)) +
  geom_point(color = "gray50", alpha = 0.5) +  # Actual data points
  geom_line(aes(y = predicted, color = color), size = 1) +  # Predicted values
  geom_line(aes(y = counterfactual, group = interaction(intervention_t5)), 
            color = "darkorange", linetype = "dashed", size = 1) +  # Counterfactual values
  geom_vline(xintercept = Date5, color = "firebrick", linetype = "dashed") +  # Intervention line
  labs(title = "P5 vs. P6", y = "7-day-mean of registrations") +
  scale_color_identity() +  # Use the predefined colors
  theme_minimal() + 
  ylim(-100, 250) + 
  xlim(as.Date("2019-12-01"), as.Date("2022-07-31"))



## Summary ---------------------------------------------------------------------

library(broom)
library(dplyr)
library(tidyr)
library(kableExtra)  # For a nicely formatted table

# Tidy model outputs
Model1 <- tidy(model_t1, conf.int = TRUE) %>% 
  mutate(model = "P1 vs. P2")
Model2 <- tidy(model_t2, conf.int = TRUE) %>% 
  mutate(model = "P2 vs. P3")
Model3 <- tidy(model_t3, conf.int = TRUE) %>% 
  mutate(model = "P3 vs. P4")
Model4 <- tidy(model_t4, conf.int = TRUE) %>% 
  mutate(model = "P4 vs. P5")
Model5 <- tidy(model_t5, conf.int = TRUE) %>% 
  mutate(model = "P5 vs. P6")

# Combine outputs
combined_tidy <- bind_rows(Model1, Model2, Model3, Model4, Model5)
combined_tidy <- combined_tidy %>% 
  mutate(term = recode(term, 
                       "(Intercept)" = "1_Intercept", 
                       "Time_days" = "2_Slope (pre-intervention trend)", 
                       "intervention_t1" = "3_Level change (immediate effect)", 
                       "intervention_t2" = "3_Level change (immediate effect)", 
                       "intervention_t3" = "3_Level change (immediate effect)", 
                       "intervention_t4" = "3_Level change (immediate effect)", 
                       "intervention_t5" = "3_Level change (immediate effect)", 
                       "post_intervention_t1" = "4_Slope change (sustained effect)",
                       "post_intervention_t2" = "4_Slope change (sustained effect)",
                       "post_intervention_t3" = "4_Slope change (sustained effect)",
                       "post_intervention_t4" = "4_Slope change (sustained effect)",
                       "post_intervention_t5" = "4_Slope change (sustained effect)"), 
         estimate = sprintf("%.2f", round(estimate, 2)),
         conf.int = paste0("[", sprintf("%.2f", round(conf.low, 2)), ", ", 
                           sprintf("%.2f", round(conf.high, 2)), "]"),
         p.value_raw = sprintf("%.6f", round(p.value, 6)), 
         p.value_rounded = sprintf("%.2f", round(p.value, 2)))

# Select and arrange columns
combined_tidy <- combined_tidy %>%
  select(model, term, estimate, conf.int, p.value_rounded, p.value_raw) %>%
  arrange(model, term)

# Create a nicely formatted table
combined_tidy %>%
  kable("html", caption = "Interrupted Time Series Analysis") %>%
  kable_styling(full_width = FALSE)
write.table(combined_tidy, file = "InterruptedTimeSeriesAnalysis_male.csv", row.names = FALSE, sep = ";", na = "")



## All figures combined --------------------------------------------------------

library(gridExtra)

grid.arrange(plot0, plot1, plot2, nrow = 3)
grid.arrange(plot3, plot4, plot5, nrow = 3)


