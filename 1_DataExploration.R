## Data Exploration - MeinLUKS data
## Aug 2023
## -----------------------------------------------------------------------------

rm(list = ls())
Sys.setenv(LANG = "en")
library("tidyverse")
library("zoo")
library("lubridate")

setwd("C:/Users/linkean/OneDrive - Luzerner Kantonsspital/MeinLUKS_AmandaAnita/1 MeinLUKS data/MeinLUKS_2_Datenexport_2019-2022_PreparedTables")
t25_vacc <- read.csv2("25_ECheckIn_2019-2022_CovidVacc.csv")
t27 <- read.csv2("27_FirstLogin_2019-2022_v2.csv")
t28 <- read.csv2("28_FirstLogin_FirstAppoint_2019-2022_v2.csv")
t29 <- read.csv2("29_UserEngagement_AllUsers_2019-2022_v2.csv")




#### Table 25 ####

# # Filter for rows containing the word "COVID-IMPF":
# t25_vacc <- t25 %>% 
#   filter(grepl("COVID-IMPF", PRC_NAME))


## Data preparation: 
t25_vacc$Contact_date <- as.Date(t25_vacc$Contact_date)

t25_vacc$VaccType <- ifelse((t25_vacc$PRC_NAME == "COVID-IMPF. 1. JANSSEN") | (t25_vacc$PRC_NAME == "COVID-IMPF. 1. MODERNA") | 
                              (t25_vacc$PRC_NAME == "COVID-IMPF. 1. NOVAVAX") | (t25_vacc$PRC_NAME == "COVID-IMPF. 1. PFIZER" | 
                                                                                   (t25_vacc$PRC_NAME == "COVID-IMPF. KIND 1. PFIZER") | (t25_vacc$PRC_NAME == "COVID-IMPFUNG 1.DOSIS")), "1st Covid-19 vacc", 
                            ifelse((t25_vacc$PRC_NAME == "COVID-IMPF. 2. JANSSEN") | (t25_vacc$PRC_NAME == "COVID-IMPF. 2. MODERNA") | 
                                     (t25_vacc$PRC_NAME == "COVID-IMPF. 2. PFIZER") | (t25_vacc$PRC_NAME == "COVID-IMPF. KIND 2. PFIZER"), "2nd Covid-19 vacc", 
                                   ifelse((t25_vacc$PRC_NAME == "COVID-IMPF. 3. JANSSEN") | (t25_vacc$PRC_NAME == "COVID-IMPF. 3. MODERNA") | 
                                            (t25_vacc$PRC_NAME == "COVID-IMPF. 3. PFIZER"), "3rd Covid-19 vacc", 
                                          ifelse((t25_vacc$PRC_NAME == "COVID-IMPF. 4. MODERNA") | (t25_vacc$PRC_NAME == "COVID-IMPF. 4. PFIZER"), "4th Covid-19 vacc",
                                                 ifelse((t25_vacc$PRC_NAME == "COVID-IMPF. 5. MODERNA") | (t25_vacc$PRC_NAME == "COVID-IMPF. 5. PFIZER"), "5th Covid-19 vacc", NA)))))

## Get overview over appointment types: 
# table(t25$DEPARTMENT_NAME)
# table(t25$Dep_speciality)
# table(t25$PRC_NAME)

table(t25_vacc$VaccType) 


# # How often does each PatID appear for the 1st vaccination?
# t25_FirstVacc <- t25_vacc %>%
#   filter(VaccType == "1st Covid-19 vacc") 
# 
# t25_FirstVacc <- table(t25_FirstVacc$PAT_ID_hash)
# t25_FirstVacc[order(t25_FirstVacc, decreasing = TRUE)] # maximal frequency: 15 - more data cleaning needed!!!
# 
# rm(t25_FirstVacc)


## Data cleaning: To get last appointment of each VaccType for each patient
t25_vacc <- t25_vacc %>% 
  group_by(PAT_ID_hash, VaccType) %>% 
  arrange(Contact_date) %>% 
  filter(row_number() == n()) 
t25_vacc <- data.frame(t25_vacc)

# # To check:
# t25_vaccControl <- t25_vacc %>%
#   filter(VaccType == "1st Covid-19 vacc") 
# t25_vaccControl <- table(t25_vaccControl$PAT_ID_hash)
# t25_vaccControl[order(t25_vaccControl, decreasing = TRUE)] # maximal frequency: 1 - correct.
# rm(t25_vaccControl)






## Vaccinations per day:
t25_vaccPerDay <- data.frame(table(t25_vacc$Contact_date))
t25_vaccPerDay <- t25_vaccPerDay %>% 
  rename(Date = Var1,
         VaccPerDay = Freq)
t25_vaccPerDay <- t25_vaccPerDay %>% 
  arrange(Date) %>% 
  mutate(Vacc_7dm = rollmean(VaccPerDay, k = 7, fill = NA), 
         Vacc_14dm = rollmean(VaccPerDay, k = 14, fill = NA), 
         Vacc_28dm = rollmean(VaccPerDay, k = 28, fill = NA)) %>%
  ungroup()
t25_vaccPerDay$Date <- as.Date(t25_vaccPerDay$Date)



## Vaccinations per day by VaccType:
t25_vaccPerDay_type <- data.frame(table(t25_vacc$Contact_date, t25_vacc$VaccType))
t25_vaccPerDay_type <- t25_vaccPerDay_type %>%
  rename(Date = Var1,
         VaccType = Var2,
         VaccPerDay_vacctype = Freq)
t25_vaccPerDay_type <- t25_vaccPerDay_type %>% 
  group_by(VaccType) %>% 
  mutate(Vacc_7dm_vacctype = rollmean(VaccPerDay_vacctype, k = 7, fill = NA), 
         Vacc_14dm_vacctype = rollmean(VaccPerDay_vacctype, k = 14, fill = NA), 
         Vacc_28dm_vacctype = rollmean(VaccPerDay_vacctype, k = 28, fill = NA)) %>% 
  ungroup()
t25_vaccPerDay_type$Date <- as.Date(t25_vaccPerDay_type$Date) 

ggplot(t25_vaccPerDay_type, aes(x = Date, y = Vacc_7dm_vacctype)) + 
  geom_line()

t25_vaccPerDay_type %>% 
  filter(VaccType != "5th Covid-19 vacc") %>% 
  ggplot(aes(x = Date, y = Vacc_7dm_vacctype, 
             fill = VaccType)) + 
  geom_area()



# # Visualization with 7-day-mean:
# t25_vaccPerDay_daymean <- t25_vaccPerDay %>% 
#   group_by(Contact_date) %>% 
#   summarize(VaccPerDay_Overall = sum(VaccPerDay)) %>% 
#   mutate(VaccPerDay_Overall_7dm = rollmean(VaccPerDay_Overall, k = 7, fill = NA), 
#          VaccPerDay_Overall_14dm = rollmean(VaccPerDay_Overall, k = 14, fill = NA), 
#          VaccPerDay_Overall_28dm = rollmean(VaccPerDay_Overall, k = 28, fill = NA)) %>% 
#   ungroup()
# 
# t25_vaccPerDay_daymean %>% 
#   ggplot(aes(x = Contact_date, y = VaccPerDay_Overall_7dm)) + 
#   geom_line()







#### Table 27 ####

## Data preparation ##
str(t27)

t27$START_DTTM <- strptime(t27$START_DTTM, format = "%Y-%m-%d %H:%M:%OS")
t27$MYC_END_DTTM_NO_TIMEOUT <- strptime(t27$MYC_END_DTTM_NO_TIMEOUT, format = "%Y-%m-%d %H:%M:%OS")
t27$END_DTTM <- strptime(t27$END_DTTM, format = "%Y-%m-%d %H:%M:%OS")

t27$START_DTTM_date <- as.Date(t27$START_DTTM)
t27$MYC_END_DTTM_NO_TIMEOUT_date <- as.Date(t27$MYC_END_DTTM_NO_TIMEOUT)
t27$END_DTTM_date <- as.Date(t27$END_DTTM)

t27$START_DTTM_year <- as.numeric(format(t27$START_DTTM_date, "%Y"))
t27$START_DTTM_weekday <- as.factor(weekdays(t27$START_DTTM_date))
t27$START_DTTM_weekday_group <- as.factor(ifelse((t27$START_DTTM_weekday == "Samstag") | (t27$START_DTTM_weekday == "Sonntag"), 
                                                 "weekend", "weekday"))
t27$START_DTTM_weekday <- as.factor(ifelse(t27$START_DTTM_weekday == "Montag", "1_Monday", 
                                           ifelse(t27$START_DTTM_weekday == "Dienstag", "2_Tuesday", 
                                                  ifelse(t27$START_DTTM_weekday == "Mittwoch", "3_Wednesday", 
                                                         ifelse(t27$START_DTTM_weekday == "Donnerstag", "4_Thursday", 
                                                                ifelse(t27$START_DTTM_weekday == "Freitag", "5_Friday", 
                                                                       ifelse(t27$START_DTTM_weekday == "Samstag", "6_Saturday", "7_Sunday")))))))

t27$age <- t27$START_DTTM_year - t27$Geburtsjahr

t27$age_group <- as.factor(ifelse(t27$age < 16, "1_younger than 16",
                                  ifelse(16 <= t27$age & t27$age < 31, "2_16 to 30",
                                         ifelse(31 <= t27$age & t27$age < 51, "3_31 to 50",
                                                ifelse(51 <= t27$age & t27$age < 65, "4_51 to 64", 
                                                       ifelse(65 <= t27$age, "5_65 and older", NA))))))

t27$risk_group <- as.factor(ifelse(t27$age < 65, "1_under 65", "2_65 and older"))

t27$maritalstatus_group <- as.factor(ifelse(t27$MaritalStatus == "ledig", "1_single", 
                                            ifelse(t27$MaritalStatus == "verheiratet", "2_married", "3_other")))

t27$region <- as.factor(ifelse(grepl("LU", t27$ZIP), "2_Cantone Lucerne", "1_Rest of Switzerland, Lichtenstein, or unknown"))

t27$platform <- as.factor(t27$WEB_VS_MOBILE_DISPLAY)
t27$AccessType <- as.factor(ifelse(t27$PROXY_ACCESS_BOOL == 0, "Own Access", "Proxy Access"))
t27$sex <- as.factor(ifelse(t27$sex == "Männlich", "male", "female"))

t27$MYC_ACTIVE_DURATION_min <- t27$MYC_ACTIVE_DURATION_SECONDS / 60
t27$TOTAL_DURATION_min <- t27$TOTAL_DURATION_SECONDS / 60

t27$ActivePeriod_days <- round(t27$ActivePeriod_sec / (60*60*24), 2)

t01 <- as.Date("2020-04-22") # PCR tests for people with symptoms
t02 <- as.Date("2021-03-15") # Vaccine available for high-risk population
t03 <- as.Date("2021-09-13") # Covid certificate required in public places
t04 <- as.Date("2021-11-04") # Start booster for risk population 
t05 <- as.Date("2022-02-16") # Cancellation of measures 
t27$TimePeriods <- as.factor(ifelse(t27$START_DTTM < t01, "P1", 
                                    ifelse((t01 <= t27$START_DTTM) & (t27$START_DTTM < t02), "P2",
                                           ifelse((t02 <= t27$START_DTTM) & (t27$START_DTTM < t03), "P3", 
                                                  ifelse((t03 <= t27$START_DTTM) & (t27$START_DTTM < t04), "P4", 
                                                         ifelse((t04 <= t27$START_DTTM) & (t27$START_DTTM < t05), "P5", "P6"))))))


str(t27)


## First data exploration:

round(prop.table(table(t27$sex)) * 100, 1)
round(summary(t27$age), 0)

round(prop.table(table(t27$age_group)) * 100, 1)
round(prop.table(table(t27$risk_group)) * 100, 1)
round(prop.table(table(t27$region)) * 100, 1)

round(prop.table(table(t27$maritalstatus_group)) * 100, 1)

round(prop.table(table(t27$WEB_VS_MOBILE_DISPLAY)) * 100, 1)
round(prop.table(table(t27$AccessType)) * 100, 1)

round(prop.table(table(t27$START_DTTM_weekday)) * 100, 1)
round(prop.table(table(t27$START_DTTM_year)) * 100, 1)

summary(t27$ActivePeriod_days)
1034 / 365

round(prop.table(table(t27$TimePeriods)) * 100, 1)



## First Logins (FL):
ggplot(t27, aes(x = START_DTTM_date)) + 
  geom_histogram(binwidth = 7)


# Date: 
t27_FLdate <- data.frame(table(t27$START_DTTM_date))
t27_FLdate <- t27_FLdate %>% 
  rename(Date = Var1, 
         FL_PerDay = Freq)
t27_FLdate$Date <- as.Date(t27_FLdate$Date)


# Calculate 7-day-mean of FLs:
t27_FLdate <- t27_FLdate %>%
  arrange(Date) %>% 
  mutate(FL_7dm = rollmean(FL_PerDay, k = 7, fill = NA), 
         FL_14dm = rollmean(FL_PerDay, k = 14, fill = NA), 
         FL_28dm = rollmean(FL_PerDay, k = 28, fill = NA)) %>%
  ungroup()
t27_FLdate$Date <- as.Date(t27_FLdate$Date)

# Visualisation of FirstLogins:
ggplot(t27_FLdate, aes(x = Date, y = FL_7dm)) + 
  geom_line()

ggplot(t27_FLdate, aes(x = Date, y = FL_14dm)) + 
  geom_line()

ggplot(t27_FLdate, aes(x = Date, y = FL_28dm)) +  # visually most easy to read
  geom_line()   



# Date and gender:
t27_FLDateGender <- data.frame(table(t27$START_DTTM_date, t27$sex))
t27_FLDateGender <- t27_FLDateGender %>% 
  rename(Date = Var1, 
         sex = Var2,
         FL_PerDay_gender = Freq)
t27_FLDateGender <- t27_FLDateGender %>% 
  group_by(sex) %>% 
  mutate(FL_7dm_gender = rollmean(FL_PerDay_gender, k = 7, fill = NA), 
         FL_14dm_gender = rollmean(FL_PerDay_gender, k = 14, fill = NA), 
         FL_28dm_gender = rollmean(FL_PerDay_gender, k = 28, fill = NA)) %>% 
  ungroup()
t27_FLDateGender$Date <- as.Date(t27_FLDateGender$Date)

ggplot(t27_FLDateGender, aes(x = Date, y = FL_7dm_gender, 
                             color = sex)) + 
  geom_line()

t27_FLDateGender_perc <- t27_FLDateGender %>% 
  group_by(Date, sex) %>% 
  summarise(n = sum(FL_7dm_gender)) %>% 
  mutate(FL_7dm_gender_perc = n / sum(n))
ggplot(t27_FLDateGender_perc, aes(x = Date, y = FL_7dm_gender_perc, 
                                  fill = sex)) + 
  geom_area() + 
  geom_hline(yintercept = 0.5)

## Cumulative FLs by gender:
t27_FLDateGender_cum <- t27_FLDateGender %>% 
  group_by(sex) %>% 
  mutate(cumFL_gender = cumsum(FL_PerDay_gender)) %>% 
  ungroup()

# Visualisation of cumulative FirstLogins:
ggplot(t27_FLDateGender_cum, aes(x = Date, y = cumFL_gender, 
                                 color = sex)) + 
  geom_line()






# Date and risk-group:
t27_FLDateRisk <- data.frame(table(t27$START_DTTM_date, t27$risk_group))
t27_FLDateRisk <- t27_FLDateRisk %>% 
  rename(Date = Var1, 
         risk_group = Var2,
         FL_PerDay_risk = Freq) %>% 
  group_by(risk_group) %>% 
  mutate(FL_7dm_risk = rollmean(FL_PerDay_risk, k = 7, fill = NA), 
         FL_14dm_risk = rollmean(FL_PerDay_risk, k = 14, fill = NA), 
         FL_28dm_risk = rollmean(FL_PerDay_risk, k = 28, fill = NA)) %>% 
  ungroup()
t27_FLDateRisk$Date <- as.Date(t27_FLDateRisk$Date)

ggplot(t27_FLDateRisk, aes(x = Date, y = FL_7dm_risk, 
                           fill = risk_group)) + 
  geom_area()

t27_FLDateRisk_perc <- t27_FLDateRisk %>% 
  group_by(Date, risk_group) %>% 
  summarise(n = sum(FL_7dm_risk)) %>% 
  mutate(FL_7dm_risk_perc = n / sum(n))
ggplot(t27_FLDateRisk_perc, aes(x = Date, y = FL_7dm_risk_perc, 
                                fill = risk_group)) + 
  geom_area()




# Date and age-group:
t27_FLDateAge <- data.frame(table(t27$START_DTTM_date, t27$age_group))
t27_FLDateAge <- t27_FLDateAge %>% 
  rename(Date = Var1, 
         age_group = Var2,
         FL_PerDay_age = Freq) %>% 
  group_by(age_group) %>% 
  mutate(FL_7dm_age = rollmean(FL_PerDay_age, k = 7, fill = NA)) %>% 
  ungroup()
t27_FLDateAge$Date <- as.Date(t27_FLDateAge$Date)

ggplot(t27_FLDateAge, aes(x = Date, y = FL_7dm_age, 
                          fill = age_group)) + 
  geom_area()

t27_FLDateAge_perc <- t27_FLDateAge %>% 
  group_by(Date, age_group) %>% 
  summarise(n = sum(FL_7dm_age)) %>% 
  mutate(FL_7dm_age_perc = n / sum(n))
ggplot(t27_FLDateAge_perc, aes(x = Date, y = FL_7dm_age_perc, 
                               fill = age_group)) + 
  geom_area()





# Date, gender and risk-group:
t27_FLDate_GenderRisk <- data.frame(table(t27$START_DTTM_date, t27$sex, t27$risk_group))
t27_FLDate_GenderRisk <- t27_FLDate_GenderRisk %>% 
  rename(Date = Var1, 
         gender = Var2,
         risk_group = Var3,
         FL_PerDay_GenderRisk = Freq) 
t27_FLDate_GenderRisk$gender_risk <- ifelse((t27_FLDate_GenderRisk$gender == "female") & (t27_FLDate_GenderRisk$risk_group == "1_under 65"), "female_under65", 
                                            ifelse((t27_FLDate_GenderRisk$gender == "female") & (t27_FLDate_GenderRisk$risk_group == "2_65 and older"), "female_65plus", 
                                                   ifelse((t27_FLDate_GenderRisk$gender == "male") & (t27_FLDate_GenderRisk$risk_group == "1_under 65"), "male_under65", 
                                                          "male_65plus")))
t27_FLDate_GenderRisk <- t27_FLDate_GenderRisk %>% 
  group_by(gender_risk) %>% 
  mutate(FL_7dm_GenderRisk = rollmean(FL_PerDay_GenderRisk, k = 7, fill = NA)) %>% 
  ungroup()
t27_FLDate_GenderRisk$Date <- as.Date(t27_FLDate_GenderRisk$Date)

ggplot(t27_FLDate_GenderRisk, aes(x = Date, y = FL_7dm_GenderRisk, 
                                  color = gender_risk)) + 
  geom_line()

t27_FLDateGenderRisk_perc <- t27_FLDate_GenderRisk %>% 
  group_by(Date, gender_risk) %>% 
  summarise(n = sum(FL_7dm_GenderRisk)) %>% 
  mutate(FL_7dm_genderrisk_perc = n / sum(n), 
         gender_risk = as.factor(gender_risk)) %>% 
  ungroup()
ggplot(t27_FLDateGenderRisk_perc, aes(x = Date, y = FL_7dm_genderrisk_perc, 
                                fill = gender_risk)) + 
  geom_area()





## Duration of first session:
round(summary(t27$MYC_ACTIVE_DURATION_min), 1)
round(summary(t27$TOTAL_DURATION_min), 1)

t27 %>% 
  ggplot(aes(x = MYC_ACTIVE_DURATION_min)) + 
  geom_histogram()

t27_under100 <- t27 %>% 
  filter(MYC_ACTIVE_DURATION_min < 100)
t27_under100 %>% 
  ggplot(aes(x = MYC_ACTIVE_DURATION_min)) + 
  geom_histogram()


# Session of 0 seconds:
t27_duration0 <- t27 %>% 
  filter(TOTAL_DURATION_SECONDS == 0) 
length(t27_duration0$PAT_ID_hash) / length(t27$PAT_ID_hash) * 100 # 6321 observations (5.0%)









#### Table 28 #### 
# First logins (t27) combined with first appointments after first login (t25)

str(t28)

## Data preparation:

t28$FirstLogin_date <- as.Date(t28$FirstLogin_date)
t28$FirstAppoint_date <- as.Date(t28$FirstAppoint_date)
t28$FirstAppoint_type <- as.factor(ifelse(t28$FirstAppoint_type == "0_No Appointments", "0_No Appointments", 
                                          ifelse((t28$FirstAppoint_type == "") & ((t28$PRC_NAME == "COVID-IMPF. 1. JANSSEN") | (t28$PRC_NAME == "COVID-IMPF. 1. MODERNA") | (t28$PRC_NAME == "COVID-IMPF. 1. NOVAVAX") | 
                                                                                    (t28$PRC_NAME == "COVID-IMPF. 1. PFIZER") | (t28$PRC_NAME == "COVID-IMPF. KIND 1. PFIZER") | (t28$PRC_NAME == "COVID-IMPFUNG 1.DOSIS") | 
                                                                                    (t28$PRC_NAME == "COVID-IMPF. 2. JANSSEN") | (t28$PRC_NAME == "COVID-IMPF. 2. MODERNA") | 
                                                                                    (t28$PRC_NAME == "COVID-IMPF. 2. PFIZER") | (t28$PRC_NAME == "COVID-IMPF. KIND 2. PFIZER")), "1_1st or 2nd Covid-19 vaccination", 
                                                 ifelse((t28$FirstAppoint_type == "") & ((t28$PRC_NAME == "COVID-IMPF. 3. JANSSEN") | (t28$PRC_NAME == "COVID-IMPF. 3. MODERNA") | (t28$PRC_NAME == "COVID-IMPF. 3. PFIZER") | 
                                                                                           (t28$PRC_NAME == "COVID-IMPF. 4. MODERNA") | (t28$PRC_NAME == "COVID-IMPF. 4. PFIZER")), "2_3rd or 4th Covid-19 vaccination", 
                                                        ifelse((t28$FirstAppoint_type == "") & grepl("SPRECHSTUNDE|NACHKONTROLLE", t28$PRC_NAME), "4_Consultation or Follow-up", "6_Other type")))))

t28$DEPARTMENT_NAME <- as.factor(t28$DEPARTMENT_NAME) # 183 levels
t28$Dep_speciality <- as.factor(t28$Dep_speciality) # 60 levels
t28$PRC_NAME <- as.factor(t28$PRC_NAME) # 930 levels




## Explore time between FL and FA:
summary(t28$FL_FA_days)
round(prop.table(table(is.na(t28$FL_FA_days))) * 100, 1)



## Explore DEPARTMENT for FA:
t28_DEP <- table(t28$DEPARTMENT_NAME)
t28_DEP <- data.frame(t28_DEP[order(t28_DEP, decreasing = TRUE)])
t28_DEP$percentage <- round(t28_DEP$Freq / sum(t28_DEP$Freq) * 100, 2)
t28_DEP



## Explore Speciality for FA:
t28_speciality <- table(t28$Dep_speciality)
t28_speciality <- data.frame(t28_speciality[order(t28_speciality, decreasing = TRUE)])
t28_speciality$percentage <- round(t28_speciality$Freq / sum(t28_speciality$Freq) * 100, 2)
t28_speciality

t28_speciality <- t28_speciality %>% 
  rename(Dep_Speciality = Var1, 
         count = Freq)

ggplot(t28_speciality, aes(x = Dep_Speciality, y = count)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))



# Filter for speciality "Infektiologie und Spitalhygiene":
t28_spec_infekt <- t28 %>%
  filter(Dep_speciality == "Infektiologie und Spitalhygiene") 

t28_spec_infekt_DEP <- table(t28_spec_infekt$DEPARTMENT_NAME)
t28_spec_infekt_DEP <- data.frame(t28_spec_infekt_DEP[order(t28_spec_infekt_DEP, decreasing = TRUE)])
t28_spec_infekt_DEP

135 / sum(t28_spec_infekt_DEP$Freq) * 100 
#  0.3% of the appointments at another department
# 99.7% of the appointments at LU/SU/WO Impfzentrum

table(t28_spec_infekt$FirstAppointment_type)



# Filter for speciality "empty":
t28_spec_empty <- t28 %>% 
  filter(Dep_speciality == "")

t28_spec_empty_DEP <- table(t28_spec_empty$DEPARTMENT_NAME)
t28_spec_empty_DEP <- data.frame(t28_spec_empty_DEP[order(t28_spec_empty_DEP, decreasing = TRUE)])
t28_spec_empty_DEP$percentage <- round(t28_spec_empty_DEP$Freq / sum(t28_spec_empty_DEP$Freq) * 100, 2)
t28_spec_empty_DEP
# empty

t28_spec_empty_PRC <- table(t28_spec_empty$PRC_NAME)
t28_spec_empty_PRC <- data.frame(t28_spec_empty_PRC[order(t28_spec_empty_PRC, decreasing = TRUE)])
t28_spec_empty_PRC$percentage <- round(t28_spec_empty_PRC$Freq / sum(t28_spec_empty_PRC$Freq) * 100, 2)
t28_spec_empty_PRC
# empty

table(is.na(t28_spec_empty$FirstAppoint_date)) # TRUE for all rows



# Filter for speciality "NULL":
t28_spec_NULL <- t28 %>% 
  filter(Dep_speciality == "NULL")

t28_spec_NULL_DEP <- table(t28_spec_NULL$DEPARTMENT_NAME)
t28_spec_NULL_DEP <- data.frame(t28_spec_NULL_DEP[order(t28_spec_NULL_DEP, decreasing = TRUE)])
t28_spec_NULL_DEP$percentage <- round(t28_spec_NULL_DEP$Freq / sum(t28_spec_NULL_DEP$Freq) * 100, 2)
t28_spec_NULL_DEP

t28_spec_NULL_PRC <- table(t28_spec_NULL$PRC_NAME)
t28_spec_NULL_PRC <- data.frame(t28_spec_NULL_PRC[order(t28_spec_NULL_PRC, decreasing = TRUE)])
t28_spec_NULL_PRC$percentage <- round(t28_spec_NULL_PRC$Freq / sum(t28_spec_NULL_PRC$Freq) * 100, 2)
t28_spec_NULL_PRC



## Explore PRC_NAME for FA:
t28_prc <- table(t28$PRC_NAME)
t28_prc <- data.frame(t28_prc[order(t28_prc, decreasing = TRUE)])
t28_prc$percentage <- round(t28_prc$Freq / sum(t28_prc$Freq) * 100, 2)
t28_prc

t28_prc_groups <- table(t28$FirstAppointment_type)
t28_prc_groups <- data.frame(t28_prc_groups[order(t28_prc_groups, decreasing = TRUE)])
t28_prc_groups$percentage <- round(t28_prc_groups$Freq / sum(t28_prc_groups$Freq) * 100, 2)
t28_prc_groups




# Date and FA-type:
t28_FLDate_FAType <- data.frame(table(t28$FirstLogin_date, t28$FirstAppoint_type))
t28_FLDate_FAType <- t28_FLDate_FAType %>% 
  rename(Date = Var1, 
         FA_type = Var2,
         FL_PerDay_FAtype = Freq)
t28_FLDate_FAType <- t28_FLDate_FAType %>% 
  group_by(FA_type) %>% 
  mutate(FL_7dm_FAtype = rollmean(FL_PerDay_FAtype, k = 7, fill = NA), 
         FL_14dm_FAtype = rollmean(FL_PerDay_FAtype, k = 14, fill = NA), 
         FL_28dm_FAtype = rollmean(FL_PerDay_FAtype, k = 28, fill = NA)) %>% 
  ungroup()
t28_FLDate_FAType$Date <- as.Date(t28_FLDate_FAType$Date)

t28_FLDate_FAType %>% 
  filter(FA_type == "1st Covid-19 vaccination" | 
           FA_type == "2nd Covid-19 vaccination" | 
           FA_type == "3rd or 4th Covid-19 vaccination") %>% 
  ggplot(aes(x = Date, y = FL_7dm_FAtype, 
             color = FA_type)) + 
  geom_area()

t28_FLDate_FAType_perc <- t28_FLDate_FAType %>% 
  group_by(Date, FA_type) %>% 
  summarise(n = sum(FL_7dm_FAtype)) %>% 
  mutate(FL_PerDay_FAtype_perc = n / sum(n))
ggplot(t28_FLDate_FAType_perc, aes(x = Date, y = FL_PerDay_FAtype_perc, 
                                   fill = FA_type)) + 
  geom_area()







#### Table 29 ####

str(t29)

## Data preparation:

t29$ActivePeriod_months <- as.numeric(t29$ActivePeriod_months)
t29$NonActivePeriod_months <- as.numeric(t29$NonActivePeriod_months)
t29$ActionsPerMonth <- as.numeric(t29$ActionsPerMonth)
t29$SessionsPerMonth <- as.numeric(t29$SessionsPerMonth)
t29$SessionsPerYear <- t29$SessionsPerMonth * 12
t29$ActionsPerSession <- t29$NumberOfActions / t29$NumberOfSessions

t29$AppointmentsPerMonth <- as.numeric(t29$AppointmentsPerMonth)
t29$AppointmentsPerYear <- t29$AppointmentsPerMonth * 12
t29$AppointmentsPerYear_group <- as.factor(ifelse(t29$AppointmentsPerYear == 0, "1_No appointments", 
                                                  ifelse((t29$AppointmentsPerYear > 0) & (t29$AppointmentsPerYear < 5), "2_1 to 4 appointments per year", 
                                                         ifelse(t29$AppointmentsPerYear >= 5, "3_5 and more appointments per year", NA))))


## Active Period per user:

round(summary(t29$ActivePeriod_months), 1)

ggplot(t29, aes(x = ActivePeriod_months)) + 
  geom_histogram() 


## Number of sessions:

round(summary(t29$NumberOfSessions), 1)

t29_Under10000Sessions <- t29 %>% 
  filter(NumberOfSessions < 10000) # removed 300 PatientIDs

ggplot(t29_Under10000Sessions, aes(x = NumberOfSessions)) + 
  geom_histogram() 


round(summary(t29$SessionsPerMonth), 1)

ggplot(t29, aes(x = SessionsPerMonth)) + 
  geom_histogram() 



## Number of Appointments:

round(summary(t29$NumberOfAppointments), 1)

t29_Appoint0 <- t29 %>% 
  filter(NumberOfAppointments == 0)
nrow(t29_Appoint0) / nrow(t29) * 100

ggplot(t29, aes(x = NumberOfAppointments)) + 
  geom_histogram() 



round(summary(t29$AppointmentsPerMonth), 1)

t29_AppointPerMonth_Under1 <- t29 %>% 
  filter(AppointmentsPerMonth < 1)
nrow(t29_AppointPerMonth_Under1) / nrow(t29) * 100 

round(summary(t29$AppointmentsPerYear), 1)

ggplot(t29, aes(x = AppointmentsPerYear)) + 
  geom_histogram() 



# Date and Appointments per year:
t28_ID_FLdate <- t28 %>% 
  select(PAT_ID_hash, FirstLogin_date)
t29_new <- full_join(t29, t28_ID_FLdate,
                     by = "PAT_ID_hash")

t29_FLDate_AppYear <- data.frame(table(t29_new$FirstLogin_date, t29_new$AppointmentsPerYear_group))
t29_FLDate_AppYear <- t29_FLDate_AppYear %>% 
  rename(Date = Var1, 
         AppointmentsPerYear_group = Var2, 
         FL_PerDay_AppYear = Freq)
t29_FLDate_AppYear <- t29_FLDate_AppYear %>% 
  group_by(AppointmentsPerYear_group) %>% 
  mutate(FL_7dm_AppYear = rollmean(FL_PerDay_AppYear, k = 7, fill = NA), 
         FL_14dm_AppYear = rollmean(FL_PerDay_AppYear, k = 14, fill = NA),
         FL_28dm_AppYear = rollmean(FL_PerDay_AppYear, k = 28, fill = NA)) %>% 
  ungroup() %>% 
  mutate(Date = as.Date(Date))

ggplot(t29_FLDate_AppYear, aes(x = Date, y = FL_7dm_AppYear, 
                               color = AppointmentsPerYear_group)) + 
  geom_area()


t29_FLDate_AppYear_percentage <- t29_FLDate_AppYear %>% 
  group_by(Date, AppointmentsPerYear_group) %>% 
  summarise(n = sum(FL_7dm_AppYear)) %>% 
  mutate(FL_PerDay_AppYear_perc = n / sum(n))
ggplot(t29_FLDate_AppYear_percentage, aes(x = Date, y = FL_PerDay_AppYear_perc, 
                                          fill = AppointmentsPerYear_group)) + 
  geom_area()






#### Preparation for Table 1 ####

## Table 27:
t27$FirstLogin_date <- t27$START_DTTM_date
t27$FirstLogin_weekday <- t27$START_DTTM_weekday
t27$FirstLogin_weekday_group <- t27$START_DTTM_weekday_group
t27$Gender <- t27$sex
t27$UserSince_months <- t27$ActivePeriod_months
t27$FirstLoginAfterRelease_days <- t27$NonActivePeriod_days


FinalTable_t27 <- t27 %>% 
  select(PAT_ID_hash, Gender, age, age_group, risk_group, maritalstatus_group, region, 
         WEB_VS_MOBILE_DISPLAY, AccessType, 
         FirstLogin_date, FirstLogin_weekday, FirstLogin_weekday_group, TimePeriods,
         UserSince_months, FirstLoginAfterRelease_days)


## Table 28:
t28$TimePeriod_UntilAppointment_days <- t28$FL_FA_days

FinalTable_t28 <- t28 %>% 
  select(PAT_ID_hash, FirstAppoint_date, FirstAppoint_type, TimePeriod_UntilAppointment_days)


## Table 29:
FinalTable_t29 <- t29 %>% 
  select(PAT_ID_hash, ActionsPerMonth, SessionsPerYear, ActionsPerSession, AppointmentsPerYear, AppointmentsPerYear_group)



## Final Table: 
FinalTable <- inner_join(FinalTable_t27, FinalTable_t28, 
                         by = "PAT_ID_hash")
FinalTable <- inner_join(FinalTable, FinalTable_t29, 
                         by = "PAT_ID_hash")


setwd("C:/Users/linkean/OneDrive - Luzerner Kantonsspital/MeinLUKS_AmandaAnita/3 R codes/MeinLUKS")
write.table(FinalTable, file = "FinalTable.csv", row.names = FALSE, sep = ";", na = "")




#### Preparation for Visualization ####

# Tables to combine:
# t25_vaccPerDay
# t25_vaccPerDay_type
# t27_FLdate
# t27_FLDateRisk
# t27_FLDateGender
# t27_FLDateAge

VizTable <- full_join(t27_FLdate, t27_FLDateRisk, 
                      by = "Date")
VizTable <- full_join(VizTable, t27_FLDateAge, 
                      by = "Date")
VizTable <- full_join(VizTable, t27_FLDateGender, 
                      by = "Date")
VizTable <- full_join(VizTable, t25_vaccPerDay, 
                      by = "Date")
VizTable <- full_join(VizTable, t25_vaccPerDay_type, 
                      by = "Date")

t01 <- as.Date("2020-04-22") # PCR tests for people with symptoms
t02 <- as.Date("2021-03-15") # Vaccine available for high-risk population
t03 <- as.Date("2021-09-13") # Covid certificate required in public places
t04 <- as.Date("2021-11-04") # Start booster for risk population 
t05 <- as.Date("2022-02-16") # Cancellation of measures 
VizTable$TimePeriods <- as.factor(ifelse(VizTable$Date < t01, "P1", 
                                         ifelse((t01 <= VizTable$Date) & (VizTable$Date < t02), "P2",
                                                ifelse((t02 <= VizTable$Date) & (VizTable$Date < t03), "P3", 
                                                       ifelse((t03 <= VizTable$Date) & (VizTable$Date < t04), "P4", 
                                                              ifelse((t04 <= VizTable$Date) & (VizTable$Date < t05), "P5", "P6"))))))



setwd("C:/Users/linkean/OneDrive - Luzerner Kantonsspital/MeinLUKS_AmandaAnita/3 R codes/MeinLUKS")
write.table(VizTable, file = "VizTable.csv", row.names = FALSE, sep = ";", na = "")


## In case we don't need it combined into one table:

t27_FLdate$TimePeriods <- as.factor(ifelse(t27_FLdate$Date < t01, "P1", 
                                           ifelse((t01 <= t27_FLdate$Date) & (t27_FLdate$Date < t02), "P2",
                                                  ifelse((t02 <= t27_FLdate$Date) & (t27_FLdate$Date < t03), "P3", 
                                                         ifelse((t03 <= t27_FLdate$Date) & (t27_FLdate$Date < t04), "P4", 
                                                                ifelse((t04 <= t27_FLdate$Date) & (t27_FLdate$Date < t05), "P5", "P6"))))))
t27_FLDateGender$TimePeriods <- as.factor(ifelse(t27_FLDateGender$Date < t01, "P1", 
                                                 ifelse((t01 <= t27_FLDateGender$Date) & (t27_FLDateGender$Date < t02), "P2",
                                                        ifelse((t02 <= t27_FLDateGender$Date) & (t27_FLDateGender$Date < t03), "P3", 
                                                               ifelse((t03 <= t27_FLDateGender$Date) & (t27_FLDateGender$Date < t04), "P4", 
                                                                      ifelse((t04 <= t27_FLDateGender$Date) & (t27_FLDateGender$Date < t05), "P5", "P6"))))))
t27_FLDateRisk$TimePeriods <- as.factor(ifelse(t27_FLDateRisk$Date < t01, "P1", 
                                               ifelse((t01 <= t27_FLDateRisk$Date) & (t27_FLDateRisk$Date < t02), "P2",
                                                      ifelse((t02 <= t27_FLDateRisk$Date) & (t27_FLDateRisk$Date < t03), "P3", 
                                                             ifelse((t03 <= t27_FLDateRisk$Date) & (t27_FLDateRisk$Date < t04), "P4", 
                                                                    ifelse((t04 <= t27_FLDateRisk$Date) & (t27_FLDateRisk$Date < t05), "P5", "P6"))))))
t25_vaccPerDay$TimePeriods <- as.factor(ifelse(t25_vaccPerDay$Date < t01, "P1", 
                                               ifelse((t01 <= t25_vaccPerDay$Date) & (t25_vaccPerDay$Date < t02), "P2",
                                                      ifelse((t02 <= t25_vaccPerDay$Date) & (t25_vaccPerDay$Date < t03), "P3", 
                                                             ifelse((t03 <= t25_vaccPerDay$Date) & (t25_vaccPerDay$Date < t04), "P4", 
                                                                    ifelse((t04 <= t25_vaccPerDay$Date) & (t25_vaccPerDay$Date < t05), "P5", "P6"))))))
t25_vaccPerDay_type$TimePeriods <- as.factor(ifelse(t25_vaccPerDay_type$Date < t01, "P1", 
                                                    ifelse((t01 <= t25_vaccPerDay_type$Date) & (t25_vaccPerDay_type$Date < t02), "P2",
                                                           ifelse((t02 <= t25_vaccPerDay_type$Date) & (t25_vaccPerDay_type$Date < t03), "P3", 
                                                                  ifelse((t03 <= t25_vaccPerDay_type$Date) & (t25_vaccPerDay_type$Date < t04), "P4", 
                                                                         ifelse((t04 <= t25_vaccPerDay_type$Date) & (t25_vaccPerDay_type$Date < t05), "P5", "P6"))))))
t27_FLDateAge$TimePeriods <- as.factor(ifelse(t27_FLDateAge$Date < t01, "P1", 
                                              ifelse((t01 <= t27_FLDateAge$Date) & (t27_FLDateAge$Date < t02), "P2",
                                                     ifelse((t02 <= t27_FLDateAge$Date) & (t27_FLDateAge$Date < t03), "P3", 
                                                            ifelse((t03 <= t27_FLDateAge$Date) & (t27_FLDateAge$Date < t04), "P4", 
                                                                   ifelse((t04 <= t27_FLDateAge$Date) & (t27_FLDateAge$Date < t05), "P5", "P6"))))))
t27_FLDate_GenderRisk$TimePeriods <- as.factor(ifelse(t27_FLDate_GenderRisk$Date < t01, "P1", 
                                              ifelse((t01 <= t27_FLDate_GenderRisk$Date) & (t27_FLDate_GenderRisk$Date < t02), "P2",
                                                     ifelse((t02 <= t27_FLDate_GenderRisk$Date) & (t27_FLDate_GenderRisk$Date < t03), "P3", 
                                                            ifelse((t03 <= t27_FLDate_GenderRisk$Date) & (t27_FLDate_GenderRisk$Date < t04), "P4", 
                                                                   ifelse((t04 <= t27_FLDate_GenderRisk$Date) & (t27_FLDate_GenderRisk$Date < t05), "P5", "P6"))))))

setwd("C:/Users/linkean/OneDrive - Luzerner Kantonsspital/MeinLUKS_AmandaAnita/3 R codes/MeinLUKS")
write.table(t27_FLdate, file = "FLPerDay_overall.csv", row.names = FALSE, sep = ";", na = "")
write.table(t27_FLDateGender, file = "FLPerDay_gender.csv", row.names = FALSE, sep = ";", na = "")
write.table(t27_FLDateRisk, file = "FLPerDay_risk.csv", row.names = FALSE, sep = ";", na = "")
write.table(t25_vaccPerDay, file = "VaccPerDay_overall.csv", row.names = FALSE, sep = ";", na = "")
write.table(t25_vaccPerDay_type, file = "VaccPerDay_type.csv", row.names = FALSE, sep = ";", na = "")
write.table(t27_FLDateAge, file = "FLPerDay_age.csv", row.names = FALSE, sep = ";", na = "")
write.table(t27_FLDate_GenderRisk, file = "FLPerDay_GenderRisk.csv", row.names = FALSE, sep = ";", na = "")




## Combination of tables: 

# t25_vacc: PAT_ID_hash, Contact_Date
# t27: PAT_ID_hash, age_group, risk_group

prep_t25 <- t25_vacc %>% 
  select(PAT_ID_hash, Contact_date) %>% 
  rename(Date = Contact_date)
prep_t27 <- t27 %>% 
  select(PAT_ID_hash, age_group, risk_group)
VaccPerPerson <- full_join(prep_t25, prep_t27, by = "PAT_ID_hash")


# Vaccination per day by risk-group:
VaccPerDay_RiskGroup <- data.frame(table(VaccPerPerson$Date, VaccPerPerson$risk_group))
VaccPerDay_RiskGroup <- VaccPerDay_RiskGroup %>%
  rename(Date = Var1,
         risk_group = Var2,
         VaccPerDay_RiskGroup = Freq)
VaccPerDay_RiskGroup <- VaccPerDay_RiskGroup %>% 
  group_by(risk_group) %>% 
  mutate(Vacc_7dm_risk = rollmean(VaccPerDay_RiskGroup, k = 7, fill = NA)) %>% 
  ungroup()
VaccPerDay_RiskGroup$Date <- as.Date(VaccPerDay_RiskGroup$Date) 

ggplot(VaccPerDay_RiskGroup, aes(x = Date, y = Vacc_7dm_risk, 
                                 fill = risk_group)) + 
  geom_area()

VaccPerDay_RiskGroup$TimePeriods <- as.factor(ifelse(VaccPerDay_RiskGroup$Date < t01, "P1", 
                                                      ifelse((t01 <= VaccPerDay_RiskGroup$Date) & (VaccPerDay_RiskGroup$Date < t02), "P2",
                                                             ifelse((t02 <= VaccPerDay_RiskGroup$Date) & (VaccPerDay_RiskGroup$Date < t03), "P3", 
                                                                    ifelse((t03 <= VaccPerDay_RiskGroup$Date) & (VaccPerDay_RiskGroup$Date < t04), "P4", 
                                                                           ifelse((t04 <= VaccPerDay_RiskGroup$Date) & (VaccPerDay_RiskGroup$Date < t05), "P5", "P6"))))))

setwd("C:/Users/linkean/OneDrive - Luzerner Kantonsspital/MeinLUKS_AmandaAnita/3 R codes/MeinLUKS")
write.table(VaccPerDay_RiskGroup, file = "VaccPerDay_RiskGroup.csv", row.names = FALSE, sep = ";", na = "")



# Vaccination per day by age-group:
VaccPerDay_AgeGroup <- data.frame(table(VaccPerPerson$Date, VaccPerPerson$age_group))
VaccPerDay_AgeGroup <- VaccPerDay_AgeGroup %>%
  rename(Date = Var1,
         age_group = Var2,
         VaccPerDay_AgeGroup = Freq)
VaccPerDay_AgeGroup <- VaccPerDay_AgeGroup %>% 
  group_by(age_group) %>% 
  mutate(Vacc_7dm_risk = rollmean(VaccPerDay_AgeGroup, k = 7, fill = NA)) %>% 
  ungroup()
VaccPerDay_AgeGroup$Date <- as.Date(VaccPerDay_AgeGroup$Date) 

ggplot(VaccPerDay_AgeGroup, aes(x = Date, y = Vacc_7dm_risk, 
                                 fill = age_group)) + 
  geom_area()

VaccPerDay_AgeGroup$TimePeriods <- as.factor(ifelse(VaccPerDay_AgeGroup$Date < t01, "P1", 
                                                     ifelse((t01 <= VaccPerDay_AgeGroup$Date) & (VaccPerDay_AgeGroup$Date < t02), "P2",
                                                            ifelse((t02 <= VaccPerDay_AgeGroup$Date) & (VaccPerDay_AgeGroup$Date < t03), "P3", 
                                                                   ifelse((t03 <= VaccPerDay_AgeGroup$Date) & (VaccPerDay_AgeGroup$Date < t04), "P4", 
                                                                          ifelse((t04 <= VaccPerDay_AgeGroup$Date) & (VaccPerDay_AgeGroup$Date < t05), "P5", "P6"))))))

setwd("C:/Users/linkean/OneDrive - Luzerner Kantonsspital/MeinLUKS_AmandaAnita/3 R codes/MeinLUKS")
write.table(VaccPerDay_AgeGroup, file = "VaccPerDay_AgeGroup.csv", row.names = FALSE, sep = ";", na = "")



