################################################################################
#Program created by: bburugap
#Program created date: 08/04/2024
#Program name: 02_conditions.R
#Purpose: Conditions at baseline
#Data source: OMOP CDM
################################################################################

#import package
library(dplyr)

#Explore the condition table
sql <- "SELECT distinct a.* ,b.CONCEPT_NAME FROM main.condition_occurrence as a
left join
main.concept as b
on a.condition_concept_id=b.concept_id
;"
cond <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(cond)

#check for missing values in CONDITION_START_DATE
cond %>%
  filter(is.na(cond$CONDITION_START_DATE))
#no missing values

#check for missing values in CONDITION_END_DATE
cond %>%
  filter(is.na(cond$CONDITION_END_DATE))
#records have missing values 

#check for records where CONDITION_END_DATE is null
missing_cond <- cond %>%
  filter(is.na(cond$CONDITION_END_DATE))

#check for records where CONDITION_END_DATE is not null
nonmissing_cond <- cond %>%
  filter(!is.na(cond$CONDITION_END_DATE))

#From eyeballing the data, it looks like acute conditions have condition_end_date and chronic conditions do not have condition_end_date

# I am applying the following assumptions using dates
# assumption 1: since we are defining conditions at baseline, condition_start_date needs to be on or before cohort_start_date
# assumption 2: since some of these conditions were acute conditions like sore throat, condition_end_date needs to be on or after cohort_start_date
# assumption 3: since majority of the condition_end_date are missing, records with missing end dates are assumed to be ongoing condition (chronic condition)

#extracting conditions at baseline for the cohort of interest
sql <- "SELECT distinct a.subject_id,a.cohort_start_date,b.condition_concept_id,b.condition_start_date,
b.condition_end_date,b.condition_source_value,
c.concept_name as CONDITION,c.domain_id,c.vocabulary_id,c.concept_code ,a.COHORT_DEFINITION_ID
FROM main.cohort as a
left join
main.condition_occurrence as b
on a.subject_id =b.person_id
and a.COHORT_DEFINITION_ID = 4
left join
main.concept as c
on b.condition_concept_id=c.concept_id
where b.condition_start_date <= a.cohort_start_date
and (b.condition_end_date >= a.cohort_start_date or b.condition_end_date is null)
;"
cond_at_base <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(cond_at_base)

nrow(cond_at_base)
#6752

n_distinct(cond_at_base$SUBJECT_ID)
#2694



#check for missing values in CONDITION_START_DATE
cond_at_base %>%
  filter(is.na(cond_at_base$CONDITION_START_DATE))
#no missing values

#check for missing values in CONDITION_END_DATE
cond_at_base %>%
  filter(is.na(cond_at_base$CONDITION_END_DATE))
#6689 records have missing values out of 6752 records

################################################################################
#Summary Statistics
################################################################################

#assigning value to denominator
denominator <- n_distinct(cond_at_base$SUBJECT_ID)
print(denominator)


#distribution of conditions 
cond_distribution <- cond_at_base %>%
  group_by(CONDITION) %>%
  summarize(patient_count = n_distinct(SUBJECT_ID),Percentage = round(patient_count/denominator*100,digits=2)) %>%
  arrange(desc(patient_count))
View(cond_distribution)


#Get top 10 records 
top_10_cond <- cond_distribution %>% 
  slice_head(n = 10)

#Plotting top 10 conditions
text(
  x = barplot(top_10_cond$Percentage, ylab = 'Percentage', las= 2, main="Top 10 baseline conditions",names.arg = top_10_cond$CONDITION, col = "steelblue", ylim = c(0, max(top_10_cond$Percentage) * 1.1)),
  y = top_10_cond$Percentage + 1, labels = top_10_cond$patient_count, pos = 3, cex=0.8, col = "black"
)
