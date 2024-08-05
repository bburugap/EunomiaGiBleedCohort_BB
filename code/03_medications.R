################################################################################
#Program created by: bburugap
#Program created date: 08/04/2024
#Program name: 03_medications.R
#Purpose: Medications at baseline
#Data source: OMOP CDM
################################################################################

#import package
library(dplyr)

#Explore the drug exposure table
sql <- "SELECT distinct a.* ,b.CONCEPT_NAME FROM main.drug_exposure as a
left join
main.concept as b
on a.drug_concept_id=b.concept_id
;"
drgs <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(drgs)

#check for missing values in DRUG_EXPOSURE_START_DATE
drgs %>%
  filter(is.na(drgs$DRUG_EXPOSURE_START_DATE))
#no missing values

#check for missing values in DRUG_EXPOSURE_END_DATE
drgs %>%
  filter(is.na(drgs$DRUG_EXPOSURE_END_DATE))
#no missing values

#check for missing values in VERBATIM_END_DATE
drgs %>%
  filter(is.na(drgs$VERBATIM_END_DATE))
# missing values are present


# I am applying the following assumptions using dates
# assumption 1: since we are defining medications at baseline, drug_exposure_start_date needs to be on or before cohort_start_date
# assumption 2: since drug exposure table has individual records corresponding to the source when Drug was delivered to the Person, 
# drug_exposure_end_date needs to be on or after cohort_start_date

#extracting medications at baseline for the cohort of interest
sql <- "SELECT distinct a.subject_id,a.cohort_start_date,b.drug_concept_id,b.drug_exposure_start_date,
b.drug_exposure_end_date,b.drug_source_value,
c.concept_name as DRUG_NAME,c.domain_id,c.vocabulary_id,c.concept_code ,a.COHORT_DEFINITION_ID
FROM main.cohort as a
inner join
main.drug_exposure as b
on a.subject_id =b.person_id
and a.COHORT_DEFINITION_ID = 4
left join
main.concept as c
on b.drug_concept_id=c.concept_id
where b.drug_exposure_start_date <= a.cohort_start_date
and b.drug_exposure_end_date >= a.cohort_start_date
;"
drg_at_base <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(drg_at_base)


nrow(drg_at_base)
#2744

n_distinct(drg_at_base$SUBJECT_ID)
#2694

################################################################################
#Summary Statistics
################################################################################

#assigning value to denominator
denominator <- n_distinct(drg_at_base$SUBJECT_ID)
print(denominator)

#distribution of drugs 
drug_distribution <- drg_at_base %>%
  group_by(DRUG_NAME) %>%
  summarize(patient_count = n_distinct(SUBJECT_ID),Percentage = round(patient_count/denominator*100,digits=2)) %>%
  arrange(desc(patient_count))
View(drug_distribution)


#Get top 10 records 
top_10_drgs <- drug_distribution %>% 
slice_head(n = 10)

#Plotting top 10 conditions
text(
  x = barplot(top_10_drgs$Percentage,ylab = 'Percentage', las=2,main="Top 10 drugs at baseline",names.arg = top_10_drgs$DRUG_NAME, col = "steelblue", ylim = c(0, max(top_10_drgs$Percentage) * 1.2)),
  y = top_10_drgs$Percentage + 1, labels = top_10_drgs$patient_count, pos = 3, cex=0.8, col = "black"
)


