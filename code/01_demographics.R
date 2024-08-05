################################################################################
#Program created by: bburugap
#Program created date: 08/04/2024
#Program name: 01_demographics.R
#Purpose: Demographics
#Data source: OMOP CDM
################################################################################

#import package
library(dplyr)
install.packages("ggplot2")
install.packages("lubridate")
library("ggplot2")
library(lubridate)


#Explore the person table
sql <- "SELECT * FROM main.person limit 10 ;"
prsn <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(prsn)

#creating demographics cohort
sql <- "SELECT distinct a.subject_id,a.cohort_start_date,b.BIRTH_DATETIME,
case when b.GENDER_SOURCE_VALUE= 'F' then 'Female' 
when b.GENDER_SOURCE_VALUE= 'M' then 'Male' end as GENDER,a.COHORT_DEFINITION_ID
FROM main.cohort as a
left join
main.person as b
on a.subject_id =b.person_id
where a.COHORT_DEFINITION_ID = 4;"
dem <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()
View(dem)

nrow(dem)
#2694 (1 record per patient)

#creating age as of COHORT_START_DATE
dem$age <- round(lubridate::time_length(difftime(dem$COHORT_START_DATE, dem$BIRTH_DATETIME), "years"),digits=0)
View(dem)


################################################################################
#Summary Statistics
################################################################################

#assigning value to denominator
denominator <- n_distinct(dem$SUBJECT_ID)
print(denominator)

#age summary
age_distribution <- dem %>%
  group_by(age) %>%
  summarize(patient_count = n_distinct(SUBJECT_ID),Percentage = round(patient_count/denominator*100,digits=2)) %>%
  arrange(age)
View(age_distribution)

# Fig 1 - Age distribution plot
text(
  x = barplot(age_distribution$Percentage,  xlab="Age",ylab = 'Percentage', main="Age Distribution",names.arg = age_distribution$age, col = "steelblue", ylim = c(0, max(age_distribution$Percentage) * 1.2)),
  y = age_distribution$Percentage + 1, labels = age_distribution$patient_count, pos = 1, cex=0.8, col = "black"
)


#gender summary
gender_distribution <- dem %>%
  group_by(GENDER) %>%
  summarize(patient_count = n_distinct(SUBJECT_ID),Percentage = round(patient_count/denominator*100,digits=2))
View(gender_distribution)


# Fig 2 - Gender distribution chart
ggplot(gender_distribution, aes(x = "", y = Percentage, fill = GENDER)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Gender Distribution") +
  scale_fill_manual(values = c("steelblue", "green"))+
geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5))

