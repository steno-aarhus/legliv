
install.packages('dplyr')
install.packages('tidyverse')
install.packages('magrittr')   # For the %$% composition pipe.
install.packages('Hmisc')      # smean.cl.normal, smean.sdl.
install.packages('confintr')   # ci_median, ci_sd.
install.packages('blandr')     # blandr.output.text, blandr.statistics
install.packages('parameters') # parameters.
install.packages('psych')      # describe

library(dplyr)
library(tidyverse)
library(magrittr)   # For the %$% composition pipe.
library(ggplot2)
library(Hmisc)      # smean.cl.normal, smean.sdl.
library(confintr)   # ci_median, ci_sd.
library(blandr)     # blandr.output.text, blandr.statistics
library(parameters) # parameters.
library(psych)      # describe

# removing participants who did not complete 2 or more diet questionnaires
data <- data %>%
    filter(p20077 >= 2)

# renaming variables to appropriate names
data <- data %>%
    rename(sex = p31,
           birth_year = p34,
           wc = p48_i0,
           month_of_birth = p52,
           number_in_household = p709_i0,
           education = p6138_i0,
           ques_comp_n = p20077,
           ques_comp_req0 = p20078_i0,
           ques_comp_req1 = p20078_i1,
           ques_comp_req2 = p20078_i2,
           ques_comp_req3 = p20078_i3,
           ques_comp_req4 = p20078_i4,
           smoking = p20116_i0,
           ethnicity = p21000_i0,
           bmi = p21001_i0,
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
           lamb4 = p26100_i4,
           legumes0 = p26101_i0,
           legumes1 = p26101_i1,
           legumes2 = p26101_i2,
           legumes3 = p26101_i3,
           legumes4 = p26101_i4,
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
           age_dead = p40007_i0, # p40007_01 is empty
           icd10 = p41270,
           icd9 = p41271,
           cancer_age0 = p40008_i0,
           cancer_age1 = p40008_i1,
           cancer_age2 = p40008_i2,
           cancer_age3 = p40008_i3, # same as p40005
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
           ques_comp_t4 = p105010_i4,
           ques_star_t0 = p105030_i0,
           ques_star_t1 = p105030_i1,
           ques_star_t2 = p105030_i2,
           ques_star_t3 = p105030_i3,
           ques_star_t4 = p105030_i4
    )

# Removing follow-up values where only baseline values are needed:

data <- data %>%
    select(-contains('_i'))

# creating dataset with only liver cancer diagnoses:

data_liver <- data %>%
    filter(cancer0 == 'C22.0 Liver cell carcinoma' | cancer0 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer1 == 'C22.0 Liver cell carcinoma' | cancer1 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer2 == 'C22.0 Liver cell carcinoma' | cancer2 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer3 == 'C22.0 Liver cell carcinoma' | cancer3 == 'C22.1 Intrahepatic bile duct carcinoma'
    )

