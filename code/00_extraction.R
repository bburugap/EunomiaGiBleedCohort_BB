################################################################################
#Program created by: bburugap
#Program created date: 08/04/2024
#Program name: 00_extraction.R
#Purpose: Extraction of cohort and performing checks
#Data source: OMOP CDM
################################################################################

#import package
library(dplyr)

#checking table cohort_definition for type of subject_id & cohort_definition_id
sql <- "SELECT * FROM main.cohort_definition limit 10 ;"
chrt_d <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(chrt_d)
#no data 
#assuming all subject_id are persons and that the cohort_definition_id = 4 implies patients with NSAIDs

#extracting cohort4 data
sql <- "SELECT * FROM main.cohort WHERE COHORT_DEFINITION_ID = 4;"
chrt <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(chrt)

#checks
summary(chrt)
#COHORT_START_DATE   1941-12-05 to 2019-06-26
#COHORT_END_DATE     1941-12-05 to 2019-06-26

nrow(chrt)
#2694

#check the distinct count of patients
n_distinct(chrt$SUBJECT_ID)
#2694 no duplicate records

#check for missing values in SUBJECT_ID
chrt %>%
filter(is.na(chrt$SUBJECT_ID))
#no missing values

#check for missing values in COHORT_START_DATE
chrt %>%
filter(is.na(chrt$COHORT_START_DATE))
#no missing values

#check for missing values in COHORT_END_DATE
chrt %>%
filter(is.na(chrt$COHORT_END_DATE))
#no missing values
