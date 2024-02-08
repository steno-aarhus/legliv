
library(dplyr)
library(tidyverse)
library(magrittr)   # For the %$% composition pipe.

ukb_dataset <- data

# renaming variables to appropriate names
data <- data %>%
    rename(sex = p31,
           birth_year = p34,
           wc = p48_i0,
           month_of_birth = p52,
           l2fu_r = p190,
           l2fu_d = p190,
           number_in_household = p709_i0,
           education = p6138_i0,
           ques_comp_n = p20077,
           smoking = p20116_i0,
           ethnicity = p21000_i0,
           bmi = p21001_i0,
           age_recruit = p21022,
           phys_acti = p22040_i0,
           tdi = p22189,
           alcohol0 = p26030_i0,
           alcohol1 = p26030_i1,
           alcohol2 = p26030_i2,
           alcohol3 = p26030_i3,
           alcohol4 = p26030_i4,
           beef0 = p26066_i0,
           beef1 = p26066_i1,
           beef2 = p26066_i2,
           beef3 = p26066_i3,
           beef4 = p26066_i4,
           lamb0 = p26100_i0,
           lamb1 = p26100_i1,
           lamb2 = p26100_i2,
           lamb3 = p26100_i3,
           lamb4 = p26100_i4,
           pulses0 = p26101_i0,
           pulses1 = p26101_i1,
           pulses2 = p26101_i2,
           pulses3 = p26101_i3,
           pulses4 = p26101_i4,
           offal0 = p26104_i0,
           offal1 = p26104_i1,
           offal2 = p26104_i2,
           offal3 = p26104_i3,
           offal4 = p26104_i4,
           peas_corn0 = p26115_i0,
           peas_corn1 = p26115_i1,
           peas_corn2 = p26115_i2,
           peas_corn3 = p26115_i3,
           peas_corn4 = p26115_i4,
           pork0 = p26117_i0,
           pork1 = p26117_i1,
           pork2 = p26117_i2,
           pork3 = p26117_i3,
           pork4 = p26117_i4,
           proc_meat0 = p26122_i0,
           proc_meat1 = p26122_i1,
           proc_meat2 = p26122_i2,
           proc_meat3 = p26122_i3,
           proc_meat4 = p26122_i4,
           dead_date = p40000_i0, # p40000_i1 is empty
           dead_cause = p40001_i0, # p40001_i1 is empty
           cancer_date0 = p40005_i0,
           cancer_date1 = p40005_i1,
           cancer_date2 = p40005_i2,
           cancer_date3 = p40005_i3, # summary of data shows that p40005_i4 to p40005_i21 do not contain any data points
           cancer0 = p40006_i0,
           cancer1 = p40006_i1,
           cancer2 = p40006_i2,
           cancer3 = p40006_i3, # same as p40005
           age_dead = p40007_i0, # p40007_i1 is empty
           cancer_age0 = p40008_i0,
           cancer_age1 = p40008_i1,
           cancer_age2 = p40008_i2,
           cancer_age3 = p40008_i3, # same as p40005
           icd10 = p41270,
           icd9 = p41271,
           opcs4 = p41272,
           opcs3 = p41273,
           food_weig0 = p100001_i0,
           food_weig1 = p100001_i1,
           food_weig2 = p100001_i2,
           food_weig3 = p100001_i3,
           food_weig4 = p100001_i4,
           food_ener0 = p100002_i0,
           food_ener1 = p100002_i1,
           food_ener2 = p100002_i2,
           food_ener3 = p100002_i3,
           food_ener4 = p100002_i4,
           ques_comp_t0 = p105010_i0,
           ques_comp_t1 = p105010_i1,
           ques_comp_t2 = p105010_i2,
           ques_comp_t3 = p105010_i3,
           ques_comp_t4 = p105010_i4
    )

# removing participants who did not complete 2 or more diet questionnaires
data <- data %>%
    filter(ques_comp_n >= 2)

# excluding participants who have had liver cancer before recruitment:
# when running this code, I only end up with 15 events :'(

# data <- data %>%
#    filter(!grepl("C22", icd10))

# Removing follow-up values where only baseline values are needed:

data <- data %>%
    select(-contains('_i'))

# Removing columns where all rows = NA:

data <- data %>%
    select(where(~ !all(is.na(.))))

# creating food group variables:

data <- data %>%
    mutate(
        red_meat0 = beef0 + lamb0 + pork0 + offal0,
        red_meat1 = beef1 + lamb1 + pork1 + offal1,
        red_meat2 = beef2 + lamb2 + pork2 + offal2,
        red_meat3 = beef3 + lamb3 + pork3 + offal3,
        red_meat4 = beef4 + lamb4 + pork4 + offal4,
        legumes0 = pulses0 + peas_corn0*0.5, #assuming equal amounts of peas and corn
        legumes1 = pulses1 + peas_corn1*0.5,
        legumes2 = pulses2 + peas_corn2*0.5,
        legumes3 = pulses3 + peas_corn3*0.5,
        legumes4 = pulses4 + peas_corn4*0.5
    )

# Removing redundant variables:
data <- data %>%
    select(-matches("^beef\\d+$"),
           -matches("^lamb\\d+$"),
           -matches("^pork\\d+$"),
           -matches("^offal\\d+$"),
           -matches("^peas\\d+$"),
           -matches("^pulses\\d+$")
           )

# Merging birth year and month of birth into one column:

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

data <- data %>%
    mutate(month_of_birth_num = sprintf("%02d", match(month_of_birth, month_names)))

data <- data %>%
    unite(birth, birth_year, month_of_birth_num, sep = "-")

remove(month_names)

data <- data %>%
    select(-month_of_birth)

# Removing specific time stamp from date of completed questionnaires:

data <- data %>%
    mutate(quest_comp_t0 = substr(ques_comp_t0, 1, 10),
           quest_comp_t1 = substr(ques_comp_t1, 1, 10),
           quest_comp_t2 = substr(ques_comp_t2, 1, 10),
           quest_comp_t3 = substr(ques_comp_t3, 1, 10),
           quest_comp_t4 = substr(ques_comp_t4, 1, 10)
    )

# creating dataset with only liver cancer diagnoses:

data_liver <- data %>%
    filter(cancer0 == 'C22.0 Liver cell carcinoma' | cancer0 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer1 == 'C22.0 Liver cell carcinoma' | cancer1 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer2 == 'C22.0 Liver cell carcinoma' | cancer2 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer3 == 'C22.0 Liver cell carcinoma' | cancer3 == 'C22.1 Intrahepatic bile duct carcinoma'
    )
