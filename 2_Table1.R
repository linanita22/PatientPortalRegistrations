## Table 1 - MeinLUKS data
## Sept 2023
## -----------------------------------------------------------------------------


rm(list = ls())
Sys.setenv(LANG = "en")

library(tidyverse)
library(tableone)

data <- read.csv2("FinalTable.csv")


#### Preparation ####

str(data)

data$UserSince_months <- as.numeric(data$UserSince_months)
data$ActionsPerMonth <- as.numeric(data$ActionsPerMonth)
data$SessionsPerYear <- as.numeric(data$SessionsPerYear)
data$ActionsPerSession <- as.numeric(data$ActionsPerSession)
data$AppointmentsPerYear <- as.numeric(data$AppointmentsPerYear)

## Variables for Table 1:
variabs <- c("Gender", "age", "age_group", "risk_group", "region", 
             "AccessType", "FirstLogin_weekday", "TimePeriods", 
             "SessionsPerYear", "ActionsPerSession",
             "FirstAppoint_type", "TimePeriod_UntilAppointment_days", "AppointmentsPerYear_group")

## To identify categorical data:
catvars <- c("Gender", "age_group", "region", 
             "AccessType", "FirstLogin_weekday", "TimePeriods",
             "FirstAppoint_type", "AppointmentsPerYear_group")


## To identify numeric variables with non-normal distribution:
hist(data$age)
ks.test(data$age, "pnorm") # significant = non-normal distribution

hist(data$UserSince_months)
ks.test(data$UserSince_months, "pnorm") # significant = non-normal distribution

hist(data$TimePeriod_UntilAppointment_days)
ks.test(data$TimePeriod_UntilAppointment_days, "pnorm") # significant = non-normal distribution

hist(data$SessionsPerYear)
ks.test(data$SessionsPerYear, "pnorm") # significant = non-normal distribution

hist(data$AppointmentsPerYear)
ks.test(data$AppointmentsPerYear, "pnorm") # significant = non-normal distribution


# Non-normal distributed variables:
varnonnorm <- c("age", "TimePeriod_UntilAppointment_days", "ActionsPerSession", "SessionsPerYear")



#### Create Table 1 ####

table1_created <- CreateTableOne(data = data,
                                 vars = variabs,
                                 factorVars = catvars)
table1_for_export <- print(table1_created,
                           nonnormal = varnonnorm,
                           quote = FALSE, noSpaces = TRUE)

## Save table
write.table(table1_for_export, file="Table1_Overall.csv", sep=";", na="")





#### Create Table 1 stratified ####


table1_created <- CreateTableOne(data = data,
                                 vars = variabs, factorVars = catvars,
                                 strata = c('risk_group')) ## change strata for Tables B, C, D
table1_for_export = print(table1_created,
                          nonnormal = varnonnorm,
                          quote = FALSE, noSpaces = TRUE)


## Save table
write.table(table1_for_export, file="Table1_Stratified.csv", sep=";", na="")

