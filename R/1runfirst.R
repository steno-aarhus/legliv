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
           age_cancer0 = p40008_i0,
           age_cancer1 = p40008_i1,
           age_cancer2 = p40008_i2,
           age_cancer3 = p40008_i3, # same as p40005
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


# Function with food group variables:
# Average dietary intake of food groups -----------------------------------
calculate_food_intake <- function(data) {
  # estimating average daily and weekly intakes of food groups in g
  data <- data %>%
    # creating food groups from UKB Aurora Perez
    mutate(
      # refined cereals
      cereal_refined_total = rowSums(select(., starts_with("p26113") | starts_with("p26079") |
                                              starts_with("p26071") | starts_with("p26072") |
                                              starts_with("p26073") | starts_with("p26075") |
                                              starts_with("p26068") | starts_with("p26083")), na.rm = TRUE),
      cereal_refined_daily = cereal_refined_total/ques_comp_n,
      cereal_refined_weekly = cereal_refined_daily * 7,
      # whole-grain cereals
      whole_grain_total = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
                                           starts_with("p26077") | starts_with("p26078") |
                                           starts_with("p26105") | starts_with("p26114")), na.rm = TRUE),
      whole_grain_daily = whole_grain_total/ques_comp_n,
      whole_grain_weekly = whole_grain_daily * 7,
      # mixed dishes
      mixed_dish_total = rowSums(select(., starts_with("p26128") | starts_with("p26097") |
                                          starts_with("p26116") | starts_with("p26135") |
                                          starts_with("p26139")), na.rm = TRUE),
      mixed_dish_daily = mixed_dish_total/ques_comp_n,
      mixed_dish_weekly = mixed_dish_daily * 7,
      # dairy
      dairy_total = rowSums(select(., starts_with("p26154") | starts_with("p26087") |
                                     starts_with("p26096") | starts_with("p26102") |
                                     starts_with("p26103") | starts_with("p26099") |
                                     starts_with("p26131") | starts_with("p26133") |
                                     starts_with("p26150")), na.rm = TRUE),
      dairy_daily = dairy_total/ques_comp_n,
      dairy_weekly = dairy_daily * 7,
      # fats and spread
      fats_total = rowSums(select(., starts_with("p26112") | starts_with("p26062") |
                                    starts_with("p26063") | starts_with("p26155") |
                                    starts_with("p26110") | starts_with("p26111")), na.rm = TRUE),
      fats_daily = fats_total/ques_comp_n,
      fats_weekly = fats_daily * 7,
      # fruit
      fruit_total = rowSums(select(., starts_with("p26089") | starts_with("p26090") |
                                     starts_with("p26091") | starts_with("p26092") |
                                     starts_with("p26093") | starts_with("p26094")), na.rm = TRUE),
      fruit_daily = fruit_total/ques_comp_n,
      fruit_weekly = fruit_daily * 7,
      # nuts and seeds
      nut_total = rowSums(select(., starts_with("p26107") | starts_with("p26108")), na.rm = TRUE),
      nut_daily = nut_total/ques_comp_n,
      nut_weekly = nut_daily*7,
      # vegetables
      veggie_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                      starts_with("p26115") | starts_with("p26123") |
                                      starts_with("p26125") | starts_with("p26143") |
                                      starts_with("p26146") | starts_with("p26147")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + #assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE), #assuming half peas half corn
      veggie_daily = veggie_total/ques_comp_n,
      veggie_weekly = veggie_daily * 7,
      # potatoes
      potato_total = rowSums(select(., starts_with("p26118") | starts_with("p26119") |
                                      starts_with("p26120")), na.rm = TRUE),
      potato_daily = potato_total/ques_comp_n,
      potato_weekly = potato_daily * 7,
      # eggs
      egg_total = rowSums(select(., starts_with("p26088")), na.rm = TRUE),
      egg_daily = egg_total/ques_comp_n,
      egg_weekly = egg_daily * 7,
      # meat substitutes
      meat_sub_total = rowSums(select(., starts_with("p26145")), na.rm = TRUE),
      meat_sub_daily = meat_sub_total/ques_comp_n,
      meat_sub_weekly = meat_sub_daily * 7,
      # non-alcoholic beverages
      non_alc_beverage_total = rowSums(select(., starts_with("p26124") | starts_with("p26141") |
                                                starts_with("p26142") | starts_with("p26148") |
                                                starts_with("p26081") | starts_with("p26082") |
                                                starts_with("p26095") | starts_with("p26126") |
                                                starts_with("p26127")), na.rm = TRUE),
      non_alc_beverage_daily = non_alc_beverage_total/ques_comp_n,
      non_alc_beverages_weekly = non_alc_beverage_daily * 7,
      # alcoholic beverages
      alc_beverage_total = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
                                            starts_with("p26153") | starts_with("p26067") |
                                            starts_with("p26138")), na.rm = TRUE),
      alc_beverage_daily = alc_beverage_total/ques_comp_n,
      alc_beverage_weekly = alc_beverage_daily * 7,
      # sugar, preserves, cakes & confectionery, snacks
      snack_total = rowSums(select(., starts_with("p26106") | starts_with("p26140") |
                                     starts_with("p26134") | starts_with("p26084") |
                                     starts_with("p26085") | starts_with("p26064") |
                                     starts_with("p26080")), na.rm = TRUE),
      snack_daily = snack_total/ques_comp_n,
      Snack_weekly = snack_daily * 7,
      # Sauces & condiments
      sauce_total = rowSums(select(., starts_with("p26129") | starts_with("p26130")), na.rm = TRUE),
      sauce_daily = sauce_total/ques_comp_n,
      sauce_weekly = sauce_daily * 7,
      # legumes
      legume_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                      starts_with("p26136") | starts_with("p26137")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + #assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE), #assuming half peas half corn
      legume_daily = legume_total/ques_comp_n,
      legume_weekly = legume_daily * 7,
      # red meats
      red_meat_total = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
                                        starts_with("p26104") | starts_with("p26117")), na.rm = TRUE),
      red_meat_daily = red_meat_total/ques_comp_n,
      red_meat_weekly = red_meat_daily * 7,
      # processed meat
      proc_meat_total = rowSums(select(., starts_with("p26122")), na.rm = TRUE),
      proc_meat_daily = proc_meat_total/ques_comp_n,
      proc_meat_weekly = proc_meat_daily * 7,
      # Red & processed meat:
      red_proc_meat_total = red_meat_total + proc_meat_total,
      red_proc_meat_daily = red_proc_meat_total/ques_comp_n,
      # poultry
      poultry_total = rowSums(select(., starts_with("p26121") | starts_with("p26069")), na.rm = TRUE),
      poultry_daily = poultry_total/ques_comp_n,
      poultry_weekly = poultry_daily * 7,
      # fish
      fish_total = rowSums(select(., starts_with("p26070") | starts_with("p26109") |
                                    starts_with("p26132") | starts_with("p26149")), na.rm = TRUE),
      fish_daily = fish_total/ques_comp_n,
      fish_weekly = fish_daily * 7,
      # total weight of all foods
      total_weight_food = rowSums(select(., starts_with("p26000")), na.rm = TRUE),
      total_weight_food_daily = total_weight_food/ques_comp_n
    )
  data <- data %>%
    select(-starts_with("p26"), -ends_with("weekly"), -ends_with("total"), -ends_with("_n"))
  return(data)
}

data <- calculate_food_intake(data)

data <- data %>%
  rename_with(~str_replace(., "p41280", "icd10d"), starts_with("p41280_a"))

# Removing follow-up varibles where they are not needed.
data <- data %>%
  select(-contains("_i"))

# Removing columns where all rows = NA:

data <- data %>%
    select(where(~ !all(is.na(.))))

# Merging birth year and month of birth into one column:

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

data <- data %>%
  mutate(month_of_birth_num = sprintf("%02d", match(month_of_birth, month_names)))

data <- data %>%
  unite(date_birth, birth_year, month_of_birth_num, sep = "-")

remove(month_names)

data <- data %>%
  select(-month_of_birth)

# adding 15 as DD for all participants:

data$date_birth <- as.Date(paste0(data$date_birth, "-15"))

# Removing specific time stamp from date of completed questionnaires:
# (May be irrelevant)

data <- data %>%
  mutate(ques_comp_t0 = substr(ques_comp_t0, 1, 10),
         ques_comp_t1 = substr(ques_comp_t1, 1, 10),
         ques_comp_t2 = substr(ques_comp_t2, 1, 10),
         ques_comp_t3 = substr(ques_comp_t3, 1, 10),
         ques_comp_t4 = substr(ques_comp_t4, 1, 10)
  )


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
  ungroup()

# Creating age at baseline:
data <- data %>%
  mutate(age_at_baseline = year(baseline_start_date) - year(date_birth) -
           ifelse(month(baseline_start_date) < month(date_birth) |
                    (month(baseline_start_date) == month(date_birth) &
                       day(baseline_start_date) < day(date_birth)), 1, 0))

# Creating age af loss to follow-up:

data <- data %>%
  mutate(age_l2fu = as.numeric(difftime(l2fu_d, date_birth, units = "days")) / 365.25)

# Removing all participants who have had liver cancer before baseline :

data <- data %>%
    filter(!(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date0) < as.Date(baseline_start_date)) &
               !(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date1) < as.Date(baseline_start_date)) &
               !(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date2) < as.Date(baseline_start_date)) &
               !(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date3) < as.Date(baseline_start_date)))


# Converting other cancer and corresponding diagnosis date to NA's:

data <- data %>%
  mutate(
    cancer_diag0 = if_else(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag0, NA),
    cancer_diag1 = if_else(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag1, NA),
    cancer_diag2 = if_else(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag2, NA),
    cancer_diag3 = if_else(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag3, NA),
    cancer_date0 = if_else(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date0, NA),
    cancer_date1 = if_else(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date1, NA),
    cancer_date2 = if_else(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date2, NA),
    cancer_date3 = if_else(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date3, NA)
  )

# creating dataset with only liver cancer diagnoses after baseline:

data_liver <- data %>%
    filter(cancer_diag0 == 'C22.0 Liver cell carcinoma' | cancer_diag0 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag1 == 'C22.0 Liver cell carcinoma' | cancer_diag1 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag2 == 'C22.0 Liver cell carcinoma' | cancer_diag2 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag3 == 'C22.0 Liver cell carcinoma' | cancer_diag3 == 'C22.1 Intrahepatic bile duct carcinoma'
    )
