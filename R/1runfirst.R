library(dplyr)
library(tidyverse)
library(magrittr)   # For the %$% composition pipe.
library(lubridate) # For creating baseline age
library(stringr) # for renaming several columns at once

ukb_dataset <- data

# removing participants who did not complete 2 or more diet questionnaires
data <- data %>%
  filter(p20077 >= 2)

# renaming variables to appropriate names
data <- data %>%
    rename(sex = p31,
           birth_year = p34,
           wc = p48_i0,
           month_of_birth = p52,
           l2fu_r = p190,
           l2fu_d = p191,
           spouse = p709_i0,
           education = p6138_i0,
           ques_comp_n = p20077,
           smoking = p20116_i0,
           ethnicity = p21000_i0,
           bmi = p21001_i0,
           age_recruit = p21022,
           phys_acti = p22040_i0,
           tdi = p22189,
           dead_date = p40000_i0, # p40000_i1 is empty
           dead_cause = p40001_i0, # p40001_i1 is empty
           cancer_date0 = p40005_i0,
           cancer_date1 = p40005_i1,
           cancer_date2 = p40005_i2,
           cancer_date3 = p40005_i3, # summary of data shows that p40005_i4 to p40005_i21 do not contain any data points
           cancer_diag0 = p40006_i0,
           cancer_diag1 = p40006_i1,
           cancer_diag2 = p40006_i2,
           cancer_diag3 = p40006_i3, # same as p40005
           age_dead = p40007_i0, # p40007_i1 is empty
           cancer_age0 = p40008_i0,
           cancer_age1 = p40008_i1,
           cancer_age2 = p40008_i2,
           cancer_age3 = p40008_i3, # same as p40005
           icd10 = p41270,
           icd9 = p41271,
           opcs4 = p41272,
           opcs3 = p41273,
           ques_comp_t0 = p105010_i0,
           ques_comp_t1 = p105010_i1,
           ques_comp_t2 = p105010_i2,
           ques_comp_t3 = p105010_i3,
           ques_comp_t4 = p105010_i4
    )

data <- data %>%
  rename_with(~str_replace(., "p41280", "icd10d"), starts_with("p41280_a"))

# Removing columns where all rows = NA:

data <- data %>%
    select(where(~ !all(is.na(.))))

  # creating id column:
data <- data %>%
  mutate(id = 1:nrow(data))

# Creating baseline start date:
  # Removing specific time stamp from date of completed questionnaires:

data <- data %>%
  mutate(ques_comp_t0 = substr(ques_comp_t0, 1, 10),
         ques_comp_t1 = substr(ques_comp_t1, 1, 10),
         ques_comp_t2 = substr(ques_comp_t2, 1, 10),
         ques_comp_t3 = substr(ques_comp_t3, 1, 10),
         ques_comp_t4 = substr(ques_comp_t4, 1, 10)
  )

  # Create a new column with the baseline start date

data <- data %>%
  # Gather questionnaire dates into long format
  pivot_longer(cols = starts_with("ques_comp_t"),
               names_to = "questionnaire",
               values_to = "completion_date") %>%
  # Remove rows with NA completion dates
  filter(!is.na(completion_date)) %>%
  # Group by participant_id
  group_by(id) %>%
  # Arrange by completion date within each participant
  arrange(completion_date) %>%
  # Create a lagged column to find the last completed questionnaire
  mutate(last_questionnaire_date = lag(completion_date)) %>%
  # Filter participants who completed >= 2 questionnaires
  # (may be irrelevant as data are already filtered for this)
  filter(!is.na(last_questionnaire_date)) %>%
  # Keep only the last completed questionnaire for each participant
  filter(is.na(lead(completion_date))) %>%
  # Rename the columns to match the desired output
  rename(baseline_start_date = completion_date) %>%
  select(-starts_with("ques_comp_t"))

# Removing all participants who have had liver cancer before baseline :

data <- data %>%
    filter(!(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date0) < as.Date(baseline_start_date)) &
               !(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date1) < as.Date(baseline_start_date)) &
               !(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date2) < as.Date(baseline_start_date)) &
               !(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date3) < as.Date(baseline_start_date)))


# creating dataset with only liver cancer diagnoses after baseline:

data_liver <- data %>%
    filter(cancer_diag0 == 'C22.0 Liver cell carcinoma' | cancer_diag0 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag1 == 'C22.0 Liver cell carcinoma' | cancer_diag1 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag2 == 'C22.0 Liver cell carcinoma' | cancer_diag2 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag3 == 'C22.0 Liver cell carcinoma' | cancer_diag3 == 'C22.1 Intrahepatic bile duct carcinoma'
    )

