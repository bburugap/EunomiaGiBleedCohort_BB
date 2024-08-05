################################################################################
#Program created by: bburugap
#Program created date: 08/04/2024
#Program name: 04_outcomes.R
#Purpose: Outcomes during post-index
#Data source: OMOP CDM
################################################################################

#import package
install.packages("survival") 
install.packages("survminer") 
install.packages("ggsurvfit") 

library(survival) 
library(survminer)
library(ggsurvfit)
library(dplyr)

#extracting conditions of interest, i.e, Gastrointestinal hemorrhage (Condition Concept Id 192671)
# since outcomes of interest is measured during post-index, condition_start_date needs to be after cohort_start_date
sql <- "SELECT distinct coh.* ,t2.outcome_flag,
case when t2.outcome_flag=1 then 1 else 0 end as event, t2.outcome_date 
from 
(select distinct  subject_id, cohort_start_date, COHORT_DEFINITION_ID
from main.cohort
where COHORT_DEFINITION_ID =4 ) as coh

left join   

(SELECT distinct a.*,b.condition_concept_id,b.condition_start_date as outcome_date,
b.condition_end_date,b.condition_source_value,
c.concept_name,c.domain_id,c.vocabulary_id,c.concept_code , 1 as outcome_flag
FROM main.cohort as a
inner join
main.condition_occurrence as b
on a.subject_id =b.person_id
and a.COHORT_DEFINITION_ID = 4
and b.condition_concept_id = 192671
left join
main.concept as c
on b.condition_concept_id=c.concept_id
where condition_start_date > cohort_start_date) as t2

on coh.subject_id = t2.subject_id
where coh.COHORT_DEFINITION_ID = 4
;"
outcm <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(outcm)

nrow(outcm)
#2694

n_distinct(outcm$SUBJECT_ID)
#2694

outcm_flg <- outcm %>%
  filter(outcm$OUTCOME_FLAG==1)



################################################################################
#Summary Statistics
################################################################################

#assigning value to denominator
denominator <- n_distinct(outcm$SUBJECT_ID)
print(denominator)


#n subjects with outcome 
outcm_counts <- outcm_flg %>%
  summarize(patient_count = n_distinct(SUBJECT_ID),Percentage = round(patient_count/denominator*100,digits=2)) %>%
  arrange(desc(patient_count))
View(outcm_counts)

#Survival Analysis
#calcualting number of days between COHORT_START_DATE & CONDITION_START_DATE. If CONDITION_START_DATE(OUTCOME_DATE) is empty 180 days is assigned
outcm_surv <- outcm %>%
  mutate(
    days_diff = ifelse(
      !is.na(OUTCOME_DATE),
      as.numeric(difftime(OUTCOME_DATE, COHORT_START_DATE, units = "days")),
      as.numeric(difftime(COHORT_START_DATE + days(180), COHORT_START_DATE, units = "days"))
    )
  )
View(outcm_surv)


# Create survival object
s1 <- survfit(Surv(outcm_surv$days_diff, outcm_surv$EVENT) ~ 1, data = outcm_surv)
str(s1)

survfit2(Surv(days_diff, EVENT) ~ 1, data = outcm_surv) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )