
#install.packages("covid19.analytics")
library(covid19.analytics)
library(dplyr)

dir("data")
data <- "data/covid_mexico_comorbidity_dataset_100k.csv"
covid19A <- read.table(data, header = T, sep = ",")

#covid19A <- covid19.data("ts-confirmed")
#covid19.data("ts-deaths")
#covid19.data("ts-recovered")
#covid19.data("ts-ALL") 

#Create Table: COVID 19 Deaths Without Commorbidity

table_no_commorb <- covid19A %>%
  filter(COVID_Death == 1,
         Number_of_Comorbidities == 0)

table_no_commorb

table_no_comorb_summary <- table_no_commorb %>%
  summarise(
    total_cases = n(),
    avg_age = mean(Age),
    gender_counts = list(table(Gender))
  )

table_no_comorb_summary

table(covid19A$COVID_Death)
table(covid19A$COVID_Death, covid19A$Number_of_Comorbidities)

#Create Table: COVID 19 Deaths with Commorbidity
table_with_comorb <- covid19A %>%
  filter(COVID_Death == 1,
         Number_of_Comorbidities > 0)

table_with_comorb

table_with_comorb_summary <- table_with_comorb %>%
  summarise(
    total_cases = n(),
    avg_age = mean(Age),
    gender_counts = list(table(Gender)),
    comorb_distribution = list(table(Number_of_Comorbidities))
  )

table_with_comorb_summary
