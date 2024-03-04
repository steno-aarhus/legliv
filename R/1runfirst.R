library(dplyr)
library(tidyverse)
library(magrittr)   # For the %$% composition pipe.
library(lubridate) # For creating baseline age
library(stringr) # for renaming several columns at once



# removing participants who did not complete 2 or more diet questionnaires
data <- ukb_dataset %>%
  filter(p20077 >= 2)

covariates <- function(data) {
# renaming variables to appropriate names and mutating covariates:
  data <- data %>%
    mutate(sex = p31,
           birth_year = p34,
           wc = p48_i0,
           month_of_birth = p52,
           l2fu_r = p190,
           l2fu_d = p191,
           spouse = p709_i0,
           education = p6138_i0,
           ques_comp_n = p20077,
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
           ques_comp_t4 = p105010_i4,
           spouse = ifelse(spouse == 1, "No", "Yes"),
           smoking = ifelse(p20116_i0 == "Prefer not to answer", "Never", p20116_i0),
           education = case_when(
             grepl("College", education, ignore.case = TRUE) ~ "high",
             grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "intermediate",
             grepl("O levels", education, ignore.case = TRUE) ~ "intermediate",
             grepl("CSEs", education, ignore.case = TRUE) ~ "low",
             grepl("NVQ or HND", education, ignore.case = TRUE) ~ "low",
             grepl("Other professional", education, ignore.case = TRUE) ~ "low",
             grepl("None of the above", education, ignore.case = TRUE) ~ "low",
             grepl("Prefer not to answer", education, ignore.case = TRUE) ~ "low",
             TRUE ~ as.character(education)
             ),
           ethnicity = case_when(p21000_i0 == "African" ~ "Other", #check the layers to the variable
                                 p21000_i0 == "Any other Black background" ~ "Other",
                                 p21000_i0 == "Asian or Asian British" ~ "Other",
                                 p21000_i0 == "Bangladeshi" ~ "Other",
                                 p21000_i0 == "Chinese" ~ "Other",
                                 p21000_i0 == "Indian" ~ "Other",
                                 p21000_i0 == "Pakistani" ~ "Other",
                                 p21000_i0 == "Any other Asian background" ~ "Other",
                                 p21000_i0 ==  "British" ~ "White",
                                 p21000_i0 ==  "Any other white background" ~ "White",
                                 p21000_i0 ==  "Irish" ~ "White",
                                 p21000_i0 ==  "White" ~ "White",
                                 p21000_i0 ==  "White and Asian" ~ "Other",
                                 p21000_i0 ==  "White and Black African" ~ "Other",
                                 p21000_i0 ==  "White and Black Caribbean" ~ "Other",
                                 p21000_i0 ==  "Any other mixed background" ~ "Other",
                                 p21000_i0 ==  "Caribbean" ~ "Other",
                                 p21000_i0 ==  "Do not know" ~ "Other",
                                 p21000_i0 ==  "Other ethnic group" ~ "Other",
                                 p21000_i0 ==  "Prefer not to answer" ~ "Other"
                                 ),
           bmi_category = case_when(bmi < 25 ~ "Normal",
                                    bmi >= 25 & bmi <30 ~ "Overweight",
                                    bmi >= 30 ~ "Obese"
                                    ),
           exercise = case_when(p22040_i0 < 600 ~ "Low",
                                p22040_i0 >= 600 & p22040_i0 < 3000 ~ "Moderate",
                                p22040_i0 > 3000 ~ "High"
                                )
           )
  return(data)
}
data <- covariates(data)

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
      cereal_refined_daily_25 = (cereal_refined_total/ques_comp_n)/25,
      # whole-grain cereals
      whole_grain_total = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
                                           starts_with("p26077") | starts_with("p26078") |
                                           starts_with("p26105") | starts_with("p26114")), na.rm = TRUE),
      whole_grain_daily = whole_grain_total/ques_comp_n,
      whole_grain_daily_25 = (whole_grain_total/ques_comp_n)/25,
      # mixed dishes
      mixed_dish_total = rowSums(select(., starts_with("p26128") | starts_with("p26097") |
                                          starts_with("p26116") | starts_with("p26135") |
                                          starts_with("p26139")), na.rm = TRUE),
      mixed_dish_daily = mixed_dish_total/ques_comp_n,
      mixed_dish_daily_25 = (mixed_dish_total/ques_comp_n)/25,
      # dairy
      dairy_total = rowSums(select(., starts_with("p26154") | starts_with("p26087") |
                                     starts_with("p26096") | starts_with("p26102") |
                                     starts_with("p26103") | starts_with("p26099") |
                                     starts_with("p26131") | starts_with("p26133") |
                                     starts_with("p26150")), na.rm = TRUE),
      dairy_daily = dairy_total/ques_comp_n,
      dairy_daily_25 = (dairy_total/ques_comp_n)/25,
      # fats and spread
      fats_total = rowSums(select(., starts_with("p26112") | starts_with("p26062") |
                                    starts_with("p26063") | starts_with("p26155") |
                                    starts_with("p26110") | starts_with("p26111")), na.rm = TRUE),
      fats_daily = fats_total/ques_comp_n,
      fats_daily_25 = (fats_total/ques_comp_n)/25,
      # fruit
      fruit_total = rowSums(select(., starts_with("p26089") | starts_with("p26090") |
                                     starts_with("p26091") | starts_with("p26092") |
                                     starts_with("p26093") | starts_with("p26094")), na.rm = TRUE),
      fruit_daily = fruit_total/ques_comp_n,
      fruit_daily_25 = (fruit_total/ques_comp_n)/25,
      # nuts and seeds
      nut_total = rowSums(select(., starts_with("p26107") | starts_with("p26108")), na.rm = TRUE),
      nut_daily = nut_total/ques_comp_n,
      nut_daily_25 = (nut_total/ques_comp_n)/25,
      # vegetables
      veggie_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                      starts_with("p26115") | starts_with("p26123") |
                                      starts_with("p26125") | starts_with("p26143") |
                                      starts_with("p26146") | starts_with("p26147")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + #assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE), #assuming half peas half corn
      veggie_daily = veggie_total/ques_comp_n,
      veggie_daily_25 = (veggie_total/ques_comp_n)/25,
      # potatoes
      potato_total = rowSums(select(., starts_with("p26118") | starts_with("p26119") |
                                      starts_with("p26120")), na.rm = TRUE),
      potato_daily = potato_total/ques_comp_n,
      potato_daily_25 = (potato_total/ques_comp_n)/25,
      # eggs
      egg_total = rowSums(select(., starts_with("p26088")), na.rm = TRUE),
      egg_daily = egg_total/ques_comp_n,
      egg_daily_25 = (egg_total/ques_comp_n)/25,
      # meat substitutes
      meat_sub_total = rowSums(select(., starts_with("p26145")), na.rm = TRUE),
      meat_sub_daily = meat_sub_total/ques_comp_n,
      meat_sub_daily_25 = (meat_sub_total/ques_comp_n)/25,
      # non-alcoholic beverages
      non_alc_beverage_total = rowSums(select(., starts_with("p26124") | starts_with("p26141") |
                                                starts_with("p26142") | starts_with("p26148") |
                                                starts_with("p26081") | starts_with("p26082") |
                                                starts_with("p26095") | starts_with("p26126") |
                                                starts_with("p26127")), na.rm = TRUE),
      non_alc_beverage_daily = non_alc_beverage_total/ques_comp_n,
      non_alc_beverage_daily_25 = (non_alc_beverage_total/ques_comp_n)/25,
      # alcoholic beverages
      alc_beverage_total = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
                                            starts_with("p26153") | starts_with("p26067") |
                                            starts_with("p26138")), na.rm = TRUE),
      alc_beverage_daily = alc_beverage_total/ques_comp_n,
      alc_beverage_daily_25 = (alc_beverage_total/ques_comp_n)/25,
      # sugar, preserves, cakes & confectionery, snacks
      snack_total = rowSums(select(., starts_with("p26106") | starts_with("p26140") |
                                     starts_with("p26134") | starts_with("p26084") |
                                     starts_with("p26085") | starts_with("p26064") |
                                     starts_with("p26080")), na.rm = TRUE),
      snack_daily = snack_total/ques_comp_n,
      snack_daily_25 = (snack_total/ques_comp_n)/25,
      # Sauces & condiments
      sauce_total = rowSums(select(., starts_with("p26129") | starts_with("p26130")), na.rm = TRUE),
      sauce_daily = sauce_total/ques_comp_n,
      sauce_daily_25 = (sauce_total/ques_comp_n)/25,
      # legumes
      legume_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                      starts_with("p26136") | starts_with("p26137")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + #assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE), #assuming half peas half corn
      legume_daily = legume_total/ques_comp_n,
      legume_daily_25 = (legume_total/ques_comp_n)/25,
      pulse_total = rowSums(select(., starts_with("p26101")), na.rm = TRUE),
      pulse_daily = pulse_total/ques_comp_n,
      pulse_daily_25 = (pulse_total/ques_comp_n)/25,
      legume_other_total = rowSums(select(., starts_with("p26086") |
                                      starts_with("p26136") | starts_with("p26137")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + #assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE), #assuming half peas half corn
      legume_other_daily = legume_other_total/ques_comp_n,
      legume_other_daily_25 = (legume_other_total/ques_comp_n)/25,
      # red meats
      red_meat_total = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
                                        starts_with("p26104") | starts_with("p26117")), na.rm = TRUE),
      red_meat_daily = red_meat_total/ques_comp_n,
      red_meat_daily_25 = (red_meat_total/ques_comp_n)/25,
      # processed meat
      proc_meat_total = rowSums(select(., starts_with("p26122")), na.rm = TRUE),
      proc_meat_daily = proc_meat_total/ques_comp_n,
      proc_meat_daily_25 = (proc_meat_total/ques_comp_n)/25,
      # Red & processed meat:
      red_proc_meat_total = red_meat_total + proc_meat_total,
      red_proc_meat_daily = red_proc_meat_total/ques_comp_n,
      red_proc_meat_daily_25 = (red_proc_meat_total/ques_comp_n)/25,
      # poultry
      poultry_total = rowSums(select(., starts_with("p26121") | starts_with("p26069")), na.rm = TRUE),
      poultry_daily = poultry_total/ques_comp_n,
      poultry_daily_25 = (poultry_total/ques_comp_n)/25,
      # fish
      fish_total = rowSums(select(., starts_with("p26070") | starts_with("p26109") |
                                    starts_with("p26132") | starts_with("p26149")), na.rm = TRUE),
      fish_daily = fish_total/ques_comp_n,
      fish_daily_25 = (fish_total/ques_comp_n)/25,
      # total weight of all foods
      total_weight_food_daily = legume_daily + red_meat_daily + proc_meat_daily +
        poultry_daily + fish_daily + dairy_daily + egg_daily + cereal_refined_daily +
        whole_grain_daily + veggie_daily + potato_daily + fruit_daily + nut_daily +
        meat_sub_daily + snack_daily + mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily,
      # total energy of all foods and beverages
      total_energy_food = rowSums(select(., starts_with("p26002")), na.rm = TRUE),
      total_energy_food_daily = total_energy_food/ques_comp_n,
    )
  data <- data %>%
    select(-starts_with("p26"), -ends_with("weekly"), -ends_with("total"), -ends_with("_n"))
  return(data)
}

data <- calculate_food_intake(data)

# Excluding potential misreporters
data <- data %>%
  filter(
    (sex == "Male" & total_energy_food_daily >= 3200 & total_energy_food_daily <= 16800) |
      (sex == "Female" & total_energy_food_daily >= 2000 & total_energy_food_daily <= 14000)
  )

data <- data %>%
  rename_with(~str_replace(., "p41280", "icd10d"), starts_with("p41280_a"))

# Function to separate multiple ICD codes in a cell
separate_icd <- function(df, column_name, new_column_prefix, max_columns = 131, sep = "|") {
  # Replace NA values in the column with an empty string
  df <- df %>%
    mutate(!!sym(column_name) := ifelse(is.na(!!sym(column_name)), "", !!sym(column_name)))

  # Get maximum number of ICD codes in a single cell
  max_icd_count <- df %>%
    mutate(icd_count = str_count(!!sym(column_name), sep) + 1) %>%
    pull(icd_count) %>%
    max()

  # Determine the number of columns to create
  num_columns <- min(max_icd_count, max_columns)

  # Create a sequence for the maximum number of ICD codes
  icd_columns <- paste0(new_column_prefix, "_a", seq_len(num_columns - 1))

  # Separate ICD codes into separate columns
  df %>%
    separate(!!sym(column_name), into = c("icd10_a0", icd_columns), sep = "\\|", fill = "right") %>%
    mutate(across(starts_with(new_column_prefix), str_trim))
}

# Apply the function to your dataframe
data <- separate_icd(df = data, column_name = "icd10", new_column_prefix = "icd10", max_columns = 131)

# Function to separate multiple OPCS4 codes in a cell
separate_opcs4 <- function(df, column_name, new_column_prefix, max_columns = 73, sep = "|") {
  # Replace NA values in the column with an empty string
  df <- df %>%
    mutate(!!sym(column_name) := ifelse(is.na(!!sym(column_name)), "", !!sym(column_name)))

  # Get maximum number of ICD codes in a single cell
  max_icd_count <- df %>%
    mutate(icd_count = str_count(!!sym(column_name), sep) + 1) %>%
    pull(icd_count) %>%
    max()

  # Determine the number of columns to create
  num_columns <- min(max_icd_count, max_columns)

  # Create a sequence for the maximum number of ICD codes
  icd_columns <- paste0(new_column_prefix, "_a", seq_len(num_columns - 1))

  # Separate ICD codes into separate columns
  df %>%
    separate(!!sym(column_name), into = c("opcs4_a0", icd_columns), sep = "\\|", fill = "right") %>%
    mutate(across(starts_with(new_column_prefix), str_trim))

}

# Apply the function to your dataframe
data <- separate_opcs4(df = data, column_name = "opcs4", new_column_prefix = "opcs4", max_columns = 73)


# Removing follow-up variables where they are not needed.
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

# Creating age at loss to follow-up:

data <- data %>%
  mutate(age_l2fu = as.numeric(difftime(l2fu_d, date_birth, units = "days")) / 365.25)

# Removing all participants who have had liver cancer before baseline :

data <- data %>%
    filter(!(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date0) < as.Date(baseline_start_date)) &
               !(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date1) < as.Date(baseline_start_date)) &
               !(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date2) < as.Date(baseline_start_date)) &
               !(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date3) < as.Date(baseline_start_date)))


# Removing participants who were lost to follow-up before baseline:
data <- data %>%
  filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date)


# Converting other cancer and corresponding diagnosis date and age at cancer to NA's:

data <- data %>%
  mutate(
    cancer_diag0 = if_else(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag0, NA),
    cancer_diag1 = if_else(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag1, NA),
    cancer_diag2 = if_else(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag2, NA),
    cancer_diag3 = if_else(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_diag3, NA),
    cancer_date0 = if_else(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date0, NA),
    cancer_date1 = if_else(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date1, NA),
    cancer_date2 = if_else(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date2, NA),
    cancer_date3 = if_else(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), cancer_date3, NA),
    age_cancer0 = if_else(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), age_cancer0, NA),
    age_cancer1 = if_else(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), age_cancer1, NA),
    age_cancer2 = if_else(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), age_cancer2, NA),
    age_cancer3 = if_else(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma"), age_cancer3, NA)
  )

# Creating status, status date and status age:
data <- data %>%
  mutate(
    earliest_date = pmin(dead_date, cancer_date0, cancer_date1, cancer_date2, cancer_date3, l2fu_d, na.rm = TRUE) %>%
      coalesce(as.Date("2022-12-31")),
    status = case_when(
      earliest_date == cancer_date0 & earliest_date > baseline_start_date ~ "liver cancer",
      earliest_date == cancer_date1 & earliest_date > baseline_start_date ~ "liver cancer",
      earliest_date == cancer_date2 & earliest_date > baseline_start_date ~ "liver cancer",
      earliest_date == cancer_date3 & earliest_date > baseline_start_date ~ "liver cancer",
      earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "censured",
      earliest_date == dead_date ~ "censured",
      TRUE ~ "censured"
    ),
    status_date = earliest_date,
    status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
    status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25,  # Calculating age in years
    time = status_age - age_at_baseline
  ) %>%
  select(-earliest_date)

data <- data %>%
  mutate(
    diabetes = case_when(
      grepl("E11", icd10_a0) & icd10d_a0 < baseline_start_date|
        grepl("E11", icd10_a1) & icd10d_a1 < baseline_start_date|
        grepl("E11", icd10_a2) & icd10d_a2 < baseline_start_date|
        grepl("E11", icd10_a3) & icd10d_a3 < baseline_start_date|
        grepl("E11", icd10_a4) & icd10d_a4 < baseline_start_date|
        grepl("E11", icd10_a5) & icd10d_a5 < baseline_start_date|
        grepl("E11", icd10_a6) & icd10d_a6 < baseline_start_date|
        grepl("E11", icd10_a7) & icd10d_a7 < baseline_start_date|
        grepl("E11", icd10_a8) & icd10d_a8 < baseline_start_date|
        grepl("E11", icd10_a9) & icd10d_a9 < baseline_start_date|
        grepl("E11", icd10_a10) & icd10d_a10 < baseline_start_date|
        grepl("E11", icd10_a11) & icd10d_a11 < baseline_start_date|
        grepl("E11", icd10_a12) & icd10d_a12 < baseline_start_date|
        grepl("E11", icd10_a13) & icd10d_a13 < baseline_start_date|
        grepl("E11", icd10_a14) & icd10d_a14 < baseline_start_date|
        grepl("E11", icd10_a15) & icd10d_a15 < baseline_start_date|
        grepl("E11", icd10_a16) & icd10d_a16 < baseline_start_date|
        grepl("E11", icd10_a17) & icd10d_a17 < baseline_start_date|
        grepl("E11", icd10_a18) & icd10d_a18 < baseline_start_date|
        grepl("E11", icd10_a19) & icd10d_a19 < baseline_start_date|
        grepl("E11", icd10_a20) & icd10d_a20 < baseline_start_date|
        grepl("E11", icd10_a21) & icd10d_a21 < baseline_start_date|
        grepl("E11", icd10_a22) & icd10d_a22 < baseline_start_date|
        grepl("E11", icd10_a23) & icd10d_a23 < baseline_start_date|
        grepl("E11", icd10_a24) & icd10d_a24 < baseline_start_date|
        grepl("E11", icd10_a25) & icd10d_a25 < baseline_start_date|
        grepl("E11", icd10_a26) & icd10d_a26 < baseline_start_date|
        grepl("E11", icd10_a27) & icd10d_a27 < baseline_start_date|
        grepl("E11", icd10_a28) & icd10d_a28 < baseline_start_date|
        grepl("E11", icd10_a29) & icd10d_a29 < baseline_start_date|
        grepl("E11", icd10_a30) & icd10d_a30 < baseline_start_date|
        grepl("E11", icd10_a31) & icd10d_a31 < baseline_start_date|
        grepl("E11", icd10_a32) & icd10d_a32 < baseline_start_date|
        grepl("E11", icd10_a33) & icd10d_a33 < baseline_start_date|
        grepl("E11", icd10_a34) & icd10d_a34 < baseline_start_date|
        grepl("E11", icd10_a35) & icd10d_a35 < baseline_start_date|
        grepl("E11", icd10_a36) & icd10d_a36 < baseline_start_date|
        grepl("E11", icd10_a37) & icd10d_a37 < baseline_start_date|
        grepl("E11", icd10_a38) & icd10d_a38 < baseline_start_date|
        grepl("E11", icd10_a39) & icd10d_a39 < baseline_start_date|
        grepl("E11", icd10_a40) & icd10d_a40 < baseline_start_date|
        grepl("E11", icd10_a41) & icd10d_a41 < baseline_start_date|
        grepl("E11", icd10_a42) & icd10d_a42 < baseline_start_date|
        grepl("E11", icd10_a43) & icd10d_a43 < baseline_start_date|
        grepl("E11", icd10_a44) & icd10d_a44 < baseline_start_date|
        grepl("E11", icd10_a45) & icd10d_a45 < baseline_start_date|
        grepl("E11", icd10_a46) & icd10d_a46 < baseline_start_date|
        grepl("E11", icd10_a47) & icd10d_a47 < baseline_start_date|
        grepl("E11", icd10_a48) & icd10d_a48 < baseline_start_date|
        grepl("E11", icd10_a49) & icd10d_a49 < baseline_start_date|
        grepl("E11", icd10_a50) & icd10d_a50 < baseline_start_date|
        grepl("E11", icd10_a51) & icd10d_a51 < baseline_start_date|
        grepl("E11", icd10_a52) & icd10d_a52 < baseline_start_date|
        grepl("E11", icd10_a53) & icd10d_a53 < baseline_start_date|
        grepl("E11", icd10_a54) & icd10d_a54 < baseline_start_date|
        grepl("E11", icd10_a55) & icd10d_a55 < baseline_start_date|
        grepl("E11", icd10_a56) & icd10d_a56 < baseline_start_date|
        grepl("E11", icd10_a57) & icd10d_a57 < baseline_start_date|
        grepl("E11", icd10_a58) & icd10d_a58 < baseline_start_date|
        grepl("E11", icd10_a59) & icd10d_a59 < baseline_start_date|
        grepl("E11", icd10_a60) & icd10d_a60 < baseline_start_date|
        grepl("E11", icd10_a61) & icd10d_a61 < baseline_start_date|
        grepl("E11", icd10_a62) & icd10d_a62 < baseline_start_date|
        grepl("E11", icd10_a63) & icd10d_a63 < baseline_start_date|
        grepl("E11", icd10_a64) & icd10d_a64 < baseline_start_date|
        grepl("E11", icd10_a65) & icd10d_a65 < baseline_start_date|
        grepl("E11", icd10_a66) & icd10d_a66 < baseline_start_date|
        grepl("E11", icd10_a67) & icd10d_a67 < baseline_start_date|
        grepl("E11", icd10_a68) & icd10d_a68 < baseline_start_date|
        grepl("E11", icd10_a69) & icd10d_a69 < baseline_start_date|
        grepl("E11", icd10_a70) & icd10d_a70 < baseline_start_date|
        grepl("E11", icd10_a71) & icd10d_a71 < baseline_start_date|
        grepl("E11", icd10_a72) & icd10d_a72 < baseline_start_date|
        grepl("E11", icd10_a73) & icd10d_a73 < baseline_start_date|
        grepl("E11", icd10_a74) & icd10d_a74 < baseline_start_date|
        grepl("E11", icd10_a75) & icd10d_a75 < baseline_start_date|
        grepl("E11", icd10_a76) & icd10d_a76 < baseline_start_date|
        grepl("E11", icd10_a77) & icd10d_a77 < baseline_start_date|
        grepl("E11", icd10_a78) & icd10d_a78 < baseline_start_date|
        grepl("E11", icd10_a79) & icd10d_a79 < baseline_start_date|
        grepl("E11", icd10_a80) & icd10d_a80 < baseline_start_date|
        grepl("E11", icd10_a81) & icd10d_a81 < baseline_start_date|
        grepl("E11", icd10_a82) & icd10d_a82 < baseline_start_date|
        grepl("E11", icd10_a83) & icd10d_a83 < baseline_start_date|
        grepl("E11", icd10_a84) & icd10d_a84 < baseline_start_date|
        grepl("E11", icd10_a85) & icd10d_a85 < baseline_start_date|
        grepl("E11", icd10_a86) & icd10d_a86 < baseline_start_date|
        grepl("E11", icd10_a87) & icd10d_a87 < baseline_start_date|
        grepl("E11", icd10_a88) & icd10d_a88 < baseline_start_date|
        grepl("E11", icd10_a89) & icd10d_a89 < baseline_start_date|
        grepl("E11", icd10_a90) & icd10d_a90 < baseline_start_date|
        grepl("E11", icd10_a91) & icd10d_a91 < baseline_start_date|
        grepl("E11", icd10_a92) & icd10d_a92 < baseline_start_date|
        grepl("E11", icd10_a93) & icd10d_a93 < baseline_start_date|
        grepl("E11", icd10_a94) & icd10d_a94 < baseline_start_date|
        grepl("E11", icd10_a95) & icd10d_a95 < baseline_start_date|
        grepl("E11", icd10_a96) & icd10d_a96 < baseline_start_date|
        grepl("E11", icd10_a97) & icd10d_a97 < baseline_start_date|
        grepl("E11", icd10_a98) & icd10d_a98 < baseline_start_date|
        grepl("E11", icd10_a99) & icd10d_a99 < baseline_start_date|
        grepl("E11", icd10_a100) & icd10d_a100 < baseline_start_date ~ "yes",
      TRUE ~ "no"
    ),
    cholelith = case_when(
      grepl("K80", icd10_a0) & icd10d_a0 < baseline_start_date|
        grepl("K80", icd10_a1) & icd10d_a1 < baseline_start_date|
        grepl("K80", icd10_a2) & icd10d_a2 < baseline_start_date|
        grepl("K80", icd10_a3) & icd10d_a3 < baseline_start_date|
        grepl("K80", icd10_a4) & icd10d_a4 < baseline_start_date|
        grepl("K80", icd10_a5) & icd10d_a5 < baseline_start_date|
        grepl("K80", icd10_a6) & icd10d_a6 < baseline_start_date|
        grepl("K80", icd10_a7) & icd10d_a7 < baseline_start_date|
        grepl("K80", icd10_a8) & icd10d_a8 < baseline_start_date|
        grepl("K80", icd10_a9) & icd10d_a9 < baseline_start_date|
        grepl("K80", icd10_a10) & icd10d_a10 < baseline_start_date|
        grepl("K80", icd10_a11) & icd10d_a11 < baseline_start_date|
        grepl("K80", icd10_a12) & icd10d_a12 < baseline_start_date|
        grepl("K80", icd10_a13) & icd10d_a13 < baseline_start_date|
        grepl("K80", icd10_a14) & icd10d_a14 < baseline_start_date|
        grepl("K80", icd10_a15) & icd10d_a15 < baseline_start_date|
        grepl("K80", icd10_a16) & icd10d_a16 < baseline_start_date|
        grepl("K80", icd10_a17) & icd10d_a17 < baseline_start_date|
        grepl("K80", icd10_a18) & icd10d_a18 < baseline_start_date|
        grepl("K80", icd10_a19) & icd10d_a19 < baseline_start_date|
        grepl("K80", icd10_a20) & icd10d_a20 < baseline_start_date|
        grepl("K80", icd10_a21) & icd10d_a21 < baseline_start_date|
        grepl("K80", icd10_a22) & icd10d_a22 < baseline_start_date|
        grepl("K80", icd10_a23) & icd10d_a23 < baseline_start_date|
        grepl("K80", icd10_a24) & icd10d_a24 < baseline_start_date|
        grepl("K80", icd10_a25) & icd10d_a25 < baseline_start_date|
        grepl("K80", icd10_a26) & icd10d_a26 < baseline_start_date|
        grepl("K80", icd10_a27) & icd10d_a27 < baseline_start_date|
        grepl("K80", icd10_a28) & icd10d_a28 < baseline_start_date|
        grepl("K80", icd10_a29) & icd10d_a29 < baseline_start_date|
        grepl("K80", icd10_a30) & icd10d_a30 < baseline_start_date|
        grepl("K80", icd10_a31) & icd10d_a31 < baseline_start_date|
        grepl("K80", icd10_a32) & icd10d_a32 < baseline_start_date|
        grepl("K80", icd10_a33) & icd10d_a33 < baseline_start_date|
        grepl("K80", icd10_a34) & icd10d_a34 < baseline_start_date|
        grepl("K80", icd10_a35) & icd10d_a35 < baseline_start_date|
        grepl("K80", icd10_a36) & icd10d_a36 < baseline_start_date|
        grepl("K80", icd10_a37) & icd10d_a37 < baseline_start_date|
        grepl("K80", icd10_a38) & icd10d_a38 < baseline_start_date|
        grepl("K80", icd10_a39) & icd10d_a39 < baseline_start_date|
        grepl("K80", icd10_a40) & icd10d_a40 < baseline_start_date|
        grepl("K80", icd10_a41) & icd10d_a41 < baseline_start_date|
        grepl("K80", icd10_a42) & icd10d_a42 < baseline_start_date|
        grepl("K80", icd10_a43) & icd10d_a43 < baseline_start_date|
        grepl("K80", icd10_a44) & icd10d_a44 < baseline_start_date|
        grepl("K80", icd10_a45) & icd10d_a45 < baseline_start_date|
        grepl("K80", icd10_a46) & icd10d_a46 < baseline_start_date|
        grepl("K80", icd10_a47) & icd10d_a47 < baseline_start_date|
        grepl("K80", icd10_a48) & icd10d_a48 < baseline_start_date|
        grepl("K80", icd10_a49) & icd10d_a49 < baseline_start_date|
        grepl("K80", icd10_a50) & icd10d_a50 < baseline_start_date|
        grepl("K80", icd10_a51) & icd10d_a51 < baseline_start_date|
        grepl("K80", icd10_a52) & icd10d_a52 < baseline_start_date|
        grepl("K80", icd10_a53) & icd10d_a53 < baseline_start_date|
        grepl("K80", icd10_a54) & icd10d_a54 < baseline_start_date|
        grepl("K80", icd10_a55) & icd10d_a55 < baseline_start_date|
        grepl("K80", icd10_a56) & icd10d_a56 < baseline_start_date|
        grepl("K80", icd10_a57) & icd10d_a57 < baseline_start_date|
        grepl("K80", icd10_a58) & icd10d_a58 < baseline_start_date|
        grepl("K80", icd10_a59) & icd10d_a59 < baseline_start_date|
        grepl("K80", icd10_a60) & icd10d_a60 < baseline_start_date|
        grepl("K80", icd10_a61) & icd10d_a61 < baseline_start_date|
        grepl("K80", icd10_a62) & icd10d_a62 < baseline_start_date|
        grepl("K80", icd10_a63) & icd10d_a63 < baseline_start_date|
        grepl("K80", icd10_a64) & icd10d_a64 < baseline_start_date|
        grepl("K80", icd10_a65) & icd10d_a65 < baseline_start_date|
        grepl("K80", icd10_a66) & icd10d_a66 < baseline_start_date|
        grepl("K80", icd10_a67) & icd10d_a67 < baseline_start_date|
        grepl("K80", icd10_a68) & icd10d_a68 < baseline_start_date|
        grepl("K80", icd10_a69) & icd10d_a69 < baseline_start_date|
        grepl("K80", icd10_a70) & icd10d_a70 < baseline_start_date|
        grepl("K80", icd10_a71) & icd10d_a71 < baseline_start_date|
        grepl("K80", icd10_a72) & icd10d_a72 < baseline_start_date|
        grepl("K80", icd10_a73) & icd10d_a73 < baseline_start_date|
        grepl("K80", icd10_a74) & icd10d_a74 < baseline_start_date|
        grepl("K80", icd10_a75) & icd10d_a75 < baseline_start_date|
        grepl("K80", icd10_a76) & icd10d_a76 < baseline_start_date|
        grepl("K80", icd10_a77) & icd10d_a77 < baseline_start_date|
        grepl("K80", icd10_a78) & icd10d_a78 < baseline_start_date|
        grepl("K80", icd10_a79) & icd10d_a79 < baseline_start_date|
        grepl("K80", icd10_a80) & icd10d_a80 < baseline_start_date|
        grepl("K80", icd10_a81) & icd10d_a81 < baseline_start_date|
        grepl("K80", icd10_a82) & icd10d_a82 < baseline_start_date|
        grepl("K80", icd10_a83) & icd10d_a83 < baseline_start_date|
        grepl("K80", icd10_a84) & icd10d_a84 < baseline_start_date|
        grepl("K80", icd10_a85) & icd10d_a85 < baseline_start_date|
        grepl("K80", icd10_a86) & icd10d_a86 < baseline_start_date|
        grepl("K80", icd10_a87) & icd10d_a87 < baseline_start_date|
        grepl("K80", icd10_a88) & icd10d_a88 < baseline_start_date|
        grepl("K80", icd10_a89) & icd10d_a89 < baseline_start_date|
        grepl("K80", icd10_a90) & icd10d_a90 < baseline_start_date|
        grepl("K80", icd10_a91) & icd10d_a91 < baseline_start_date|
        grepl("K80", icd10_a92) & icd10d_a92 < baseline_start_date|
        grepl("K80", icd10_a93) & icd10d_a93 < baseline_start_date|
        grepl("K80", icd10_a94) & icd10d_a94 < baseline_start_date|
        grepl("K80", icd10_a95) & icd10d_a95 < baseline_start_date|
        grepl("K80", icd10_a96) & icd10d_a96 < baseline_start_date|
        grepl("K80", icd10_a97) & icd10d_a97 < baseline_start_date|
        grepl("K80", icd10_a98) & icd10d_a98 < baseline_start_date|
        grepl("K80", icd10_a99) & icd10d_a99 < baseline_start_date|
        grepl("K80", icd10_a100) & icd10d_a100 < baseline_start_date ~ "yes",
      TRUE ~ "no"
    ),
    alc_liver = case_when(
      grepl("K70", icd10_a0) & icd10d_a0 < baseline_start_date|
        grepl("K70", icd10_a1) & icd10d_a1 < baseline_start_date|
        grepl("K70", icd10_a2) & icd10d_a2 < baseline_start_date|
        grepl("K70", icd10_a3) & icd10d_a3 < baseline_start_date|
        grepl("K70", icd10_a4) & icd10d_a4 < baseline_start_date|
        grepl("K70", icd10_a5) & icd10d_a5 < baseline_start_date|
        grepl("K70", icd10_a6) & icd10d_a6 < baseline_start_date|
        grepl("K70", icd10_a7) & icd10d_a7 < baseline_start_date|
        grepl("K70", icd10_a8) & icd10d_a8 < baseline_start_date|
        grepl("K70", icd10_a9) & icd10d_a9 < baseline_start_date|
        grepl("K70", icd10_a10) & icd10d_a10 < baseline_start_date|
        grepl("K70", icd10_a11) & icd10d_a11 < baseline_start_date|
        grepl("K70", icd10_a12) & icd10d_a12 < baseline_start_date|
        grepl("K70", icd10_a13) & icd10d_a13 < baseline_start_date|
        grepl("K70", icd10_a14) & icd10d_a14 < baseline_start_date|
        grepl("K70", icd10_a15) & icd10d_a15 < baseline_start_date|
        grepl("K70", icd10_a16) & icd10d_a16 < baseline_start_date|
        grepl("K70", icd10_a17) & icd10d_a17 < baseline_start_date|
        grepl("K70", icd10_a18) & icd10d_a18 < baseline_start_date|
        grepl("K70", icd10_a19) & icd10d_a19 < baseline_start_date|
        grepl("K70", icd10_a20) & icd10d_a20 < baseline_start_date|
        grepl("K70", icd10_a21) & icd10d_a21 < baseline_start_date|
        grepl("K70", icd10_a22) & icd10d_a22 < baseline_start_date|
        grepl("K70", icd10_a23) & icd10d_a23 < baseline_start_date|
        grepl("K70", icd10_a24) & icd10d_a24 < baseline_start_date|
        grepl("K70", icd10_a25) & icd10d_a25 < baseline_start_date|
        grepl("K70", icd10_a26) & icd10d_a26 < baseline_start_date|
        grepl("K70", icd10_a27) & icd10d_a27 < baseline_start_date|
        grepl("K70", icd10_a28) & icd10d_a28 < baseline_start_date|
        grepl("K70", icd10_a29) & icd10d_a29 < baseline_start_date|
        grepl("K70", icd10_a30) & icd10d_a30 < baseline_start_date|
        grepl("K70", icd10_a31) & icd10d_a31 < baseline_start_date|
        grepl("K70", icd10_a32) & icd10d_a32 < baseline_start_date|
        grepl("K70", icd10_a33) & icd10d_a33 < baseline_start_date|
        grepl("K70", icd10_a34) & icd10d_a34 < baseline_start_date|
        grepl("K70", icd10_a35) & icd10d_a35 < baseline_start_date|
        grepl("K70", icd10_a36) & icd10d_a36 < baseline_start_date|
        grepl("K70", icd10_a37) & icd10d_a37 < baseline_start_date|
        grepl("K70", icd10_a38) & icd10d_a38 < baseline_start_date|
        grepl("K70", icd10_a39) & icd10d_a39 < baseline_start_date|
        grepl("K70", icd10_a40) & icd10d_a40 < baseline_start_date|
        grepl("K70", icd10_a41) & icd10d_a41 < baseline_start_date|
        grepl("K70", icd10_a42) & icd10d_a42 < baseline_start_date|
        grepl("K70", icd10_a43) & icd10d_a43 < baseline_start_date|
        grepl("K70", icd10_a44) & icd10d_a44 < baseline_start_date|
        grepl("K70", icd10_a45) & icd10d_a45 < baseline_start_date|
        grepl("K70", icd10_a46) & icd10d_a46 < baseline_start_date|
        grepl("K70", icd10_a47) & icd10d_a47 < baseline_start_date|
        grepl("K70", icd10_a48) & icd10d_a48 < baseline_start_date|
        grepl("K70", icd10_a49) & icd10d_a49 < baseline_start_date|
        grepl("K70", icd10_a50) & icd10d_a50 < baseline_start_date|
        grepl("K70", icd10_a51) & icd10d_a51 < baseline_start_date|
        grepl("K70", icd10_a52) & icd10d_a52 < baseline_start_date|
        grepl("K70", icd10_a53) & icd10d_a53 < baseline_start_date|
        grepl("K70", icd10_a54) & icd10d_a54 < baseline_start_date|
        grepl("K70", icd10_a55) & icd10d_a55 < baseline_start_date|
        grepl("K70", icd10_a56) & icd10d_a56 < baseline_start_date|
        grepl("K70", icd10_a57) & icd10d_a57 < baseline_start_date|
        grepl("K70", icd10_a58) & icd10d_a58 < baseline_start_date|
        grepl("K70", icd10_a59) & icd10d_a59 < baseline_start_date|
        grepl("K70", icd10_a60) & icd10d_a60 < baseline_start_date|
        grepl("K70", icd10_a61) & icd10d_a61 < baseline_start_date|
        grepl("K70", icd10_a62) & icd10d_a62 < baseline_start_date|
        grepl("K70", icd10_a63) & icd10d_a63 < baseline_start_date|
        grepl("K70", icd10_a64) & icd10d_a64 < baseline_start_date|
        grepl("K70", icd10_a65) & icd10d_a65 < baseline_start_date|
        grepl("K70", icd10_a66) & icd10d_a66 < baseline_start_date|
        grepl("K70", icd10_a67) & icd10d_a67 < baseline_start_date|
        grepl("K70", icd10_a68) & icd10d_a68 < baseline_start_date|
        grepl("K70", icd10_a69) & icd10d_a69 < baseline_start_date|
        grepl("K70", icd10_a70) & icd10d_a70 < baseline_start_date|
        grepl("K70", icd10_a71) & icd10d_a71 < baseline_start_date|
        grepl("K70", icd10_a72) & icd10d_a72 < baseline_start_date|
        grepl("K70", icd10_a73) & icd10d_a73 < baseline_start_date|
        grepl("K70", icd10_a74) & icd10d_a74 < baseline_start_date|
        grepl("K70", icd10_a75) & icd10d_a75 < baseline_start_date|
        grepl("K70", icd10_a76) & icd10d_a76 < baseline_start_date|
        grepl("K70", icd10_a77) & icd10d_a77 < baseline_start_date|
        grepl("K70", icd10_a78) & icd10d_a78 < baseline_start_date|
        grepl("K70", icd10_a79) & icd10d_a79 < baseline_start_date|
        grepl("K70", icd10_a80) & icd10d_a80 < baseline_start_date|
        grepl("K70", icd10_a81) & icd10d_a81 < baseline_start_date|
        grepl("K70", icd10_a82) & icd10d_a82 < baseline_start_date|
        grepl("K70", icd10_a83) & icd10d_a83 < baseline_start_date|
        grepl("K70", icd10_a84) & icd10d_a84 < baseline_start_date|
        grepl("K70", icd10_a85) & icd10d_a85 < baseline_start_date|
        grepl("K70", icd10_a86) & icd10d_a86 < baseline_start_date|
        grepl("K70", icd10_a87) & icd10d_a87 < baseline_start_date|
        grepl("K70", icd10_a88) & icd10d_a88 < baseline_start_date|
        grepl("K70", icd10_a89) & icd10d_a89 < baseline_start_date|
        grepl("K70", icd10_a90) & icd10d_a90 < baseline_start_date|
        grepl("K70", icd10_a91) & icd10d_a91 < baseline_start_date|
        grepl("K70", icd10_a92) & icd10d_a92 < baseline_start_date|
        grepl("K70", icd10_a93) & icd10d_a93 < baseline_start_date|
        grepl("K70", icd10_a94) & icd10d_a94 < baseline_start_date|
        grepl("K70", icd10_a95) & icd10d_a95 < baseline_start_date|
        grepl("K70", icd10_a96) & icd10d_a96 < baseline_start_date|
        grepl("K70", icd10_a97) & icd10d_a97 < baseline_start_date|
        grepl("K70", icd10_a98) & icd10d_a98 < baseline_start_date|
        grepl("K70", icd10_a99) & icd10d_a99 < baseline_start_date|
        grepl("K70", icd10_a100) & icd10d_a100 < baseline_start_date ~ "yes",
      TRUE ~ "no"
    ),
    nafld = case_when(
      grepl("76.0", icd10_a0) & icd10d_a0 < baseline_start_date|
        grepl("76.0", icd10_a1) & icd10d_a1 < baseline_start_date|
        grepl("76.0", icd10_a2) & icd10d_a2 < baseline_start_date|
        grepl("76.0", icd10_a3) & icd10d_a3 < baseline_start_date|
        grepl("76.0", icd10_a4) & icd10d_a4 < baseline_start_date|
        grepl("76.0", icd10_a5) & icd10d_a5 < baseline_start_date|
        grepl("76.0", icd10_a6) & icd10d_a6 < baseline_start_date|
        grepl("76.0", icd10_a7) & icd10d_a7 < baseline_start_date|
        grepl("76.0", icd10_a8) & icd10d_a8 < baseline_start_date|
        grepl("76.0", icd10_a9) & icd10d_a9 < baseline_start_date|
        grepl("76.0", icd10_a10) & icd10d_a10 < baseline_start_date|
        grepl("76.0", icd10_a11) & icd10d_a11 < baseline_start_date|
        grepl("76.0", icd10_a12) & icd10d_a12 < baseline_start_date|
        grepl("76.0", icd10_a13) & icd10d_a13 < baseline_start_date|
        grepl("76.0", icd10_a14) & icd10d_a14 < baseline_start_date|
        grepl("76.0", icd10_a15) & icd10d_a15 < baseline_start_date|
        grepl("76.0", icd10_a16) & icd10d_a16 < baseline_start_date|
        grepl("76.0", icd10_a17) & icd10d_a17 < baseline_start_date|
        grepl("76.0", icd10_a18) & icd10d_a18 < baseline_start_date|
        grepl("76.0", icd10_a19) & icd10d_a19 < baseline_start_date|
        grepl("76.0", icd10_a20) & icd10d_a20 < baseline_start_date|
        grepl("76.0", icd10_a21) & icd10d_a21 < baseline_start_date|
        grepl("76.0", icd10_a22) & icd10d_a22 < baseline_start_date|
        grepl("76.0", icd10_a23) & icd10d_a23 < baseline_start_date|
        grepl("76.0", icd10_a24) & icd10d_a24 < baseline_start_date|
        grepl("76.0", icd10_a25) & icd10d_a25 < baseline_start_date|
        grepl("76.0", icd10_a26) & icd10d_a26 < baseline_start_date|
        grepl("76.0", icd10_a27) & icd10d_a27 < baseline_start_date|
        grepl("76.0", icd10_a28) & icd10d_a28 < baseline_start_date|
        grepl("76.0", icd10_a29) & icd10d_a29 < baseline_start_date|
        grepl("76.0", icd10_a30) & icd10d_a30 < baseline_start_date|
        grepl("76.0", icd10_a31) & icd10d_a31 < baseline_start_date|
        grepl("76.0", icd10_a32) & icd10d_a32 < baseline_start_date|
        grepl("76.0", icd10_a33) & icd10d_a33 < baseline_start_date|
        grepl("76.0", icd10_a34) & icd10d_a34 < baseline_start_date|
        grepl("76.0", icd10_a35) & icd10d_a35 < baseline_start_date|
        grepl("76.0", icd10_a36) & icd10d_a36 < baseline_start_date|
        grepl("76.0", icd10_a37) & icd10d_a37 < baseline_start_date|
        grepl("76.0", icd10_a38) & icd10d_a38 < baseline_start_date|
        grepl("76.0", icd10_a39) & icd10d_a39 < baseline_start_date|
        grepl("76.0", icd10_a40) & icd10d_a40 < baseline_start_date|
        grepl("76.0", icd10_a41) & icd10d_a41 < baseline_start_date|
        grepl("76.0", icd10_a42) & icd10d_a42 < baseline_start_date|
        grepl("76.0", icd10_a43) & icd10d_a43 < baseline_start_date|
        grepl("76.0", icd10_a44) & icd10d_a44 < baseline_start_date|
        grepl("76.0", icd10_a45) & icd10d_a45 < baseline_start_date|
        grepl("76.0", icd10_a46) & icd10d_a46 < baseline_start_date|
        grepl("76.0", icd10_a47) & icd10d_a47 < baseline_start_date|
        grepl("76.0", icd10_a48) & icd10d_a48 < baseline_start_date|
        grepl("76.0", icd10_a49) & icd10d_a49 < baseline_start_date|
        grepl("76.0", icd10_a50) & icd10d_a50 < baseline_start_date|
        grepl("76.0", icd10_a51) & icd10d_a51 < baseline_start_date|
        grepl("76.0", icd10_a52) & icd10d_a52 < baseline_start_date|
        grepl("76.0", icd10_a53) & icd10d_a53 < baseline_start_date|
        grepl("76.0", icd10_a54) & icd10d_a54 < baseline_start_date|
        grepl("76.0", icd10_a55) & icd10d_a55 < baseline_start_date|
        grepl("76.0", icd10_a56) & icd10d_a56 < baseline_start_date|
        grepl("76.0", icd10_a57) & icd10d_a57 < baseline_start_date|
        grepl("76.0", icd10_a58) & icd10d_a58 < baseline_start_date|
        grepl("76.0", icd10_a59) & icd10d_a59 < baseline_start_date|
        grepl("76.0", icd10_a60) & icd10d_a60 < baseline_start_date|
        grepl("76.0", icd10_a61) & icd10d_a61 < baseline_start_date|
        grepl("76.0", icd10_a62) & icd10d_a62 < baseline_start_date|
        grepl("76.0", icd10_a63) & icd10d_a63 < baseline_start_date|
        grepl("76.0", icd10_a64) & icd10d_a64 < baseline_start_date|
        grepl("76.0", icd10_a65) & icd10d_a65 < baseline_start_date|
        grepl("76.0", icd10_a66) & icd10d_a66 < baseline_start_date|
        grepl("76.0", icd10_a67) & icd10d_a67 < baseline_start_date|
        grepl("76.0", icd10_a68) & icd10d_a68 < baseline_start_date|
        grepl("76.0", icd10_a69) & icd10d_a69 < baseline_start_date|
        grepl("76.0", icd10_a70) & icd10d_a70 < baseline_start_date|
        grepl("76.0", icd10_a71) & icd10d_a71 < baseline_start_date|
        grepl("76.0", icd10_a72) & icd10d_a72 < baseline_start_date|
        grepl("76.0", icd10_a73) & icd10d_a73 < baseline_start_date|
        grepl("76.0", icd10_a74) & icd10d_a74 < baseline_start_date|
        grepl("76.0", icd10_a75) & icd10d_a75 < baseline_start_date|
        grepl("76.0", icd10_a76) & icd10d_a76 < baseline_start_date|
        grepl("76.0", icd10_a77) & icd10d_a77 < baseline_start_date|
        grepl("76.0", icd10_a78) & icd10d_a78 < baseline_start_date|
        grepl("76.0", icd10_a79) & icd10d_a79 < baseline_start_date|
        grepl("76.0", icd10_a80) & icd10d_a80 < baseline_start_date|
        grepl("76.0", icd10_a81) & icd10d_a81 < baseline_start_date|
        grepl("76.0", icd10_a82) & icd10d_a82 < baseline_start_date|
        grepl("76.0", icd10_a83) & icd10d_a83 < baseline_start_date|
        grepl("76.0", icd10_a84) & icd10d_a84 < baseline_start_date|
        grepl("76.0", icd10_a85) & icd10d_a85 < baseline_start_date|
        grepl("76.0", icd10_a86) & icd10d_a86 < baseline_start_date|
        grepl("76.0", icd10_a87) & icd10d_a87 < baseline_start_date|
        grepl("76.0", icd10_a88) & icd10d_a88 < baseline_start_date|
        grepl("76.0", icd10_a89) & icd10d_a89 < baseline_start_date|
        grepl("76.0", icd10_a90) & icd10d_a90 < baseline_start_date|
        grepl("76.0", icd10_a91) & icd10d_a91 < baseline_start_date|
        grepl("76.0", icd10_a92) & icd10d_a92 < baseline_start_date|
        grepl("76.0", icd10_a93) & icd10d_a93 < baseline_start_date|
        grepl("76.0", icd10_a94) & icd10d_a94 < baseline_start_date|
        grepl("76.0", icd10_a95) & icd10d_a95 < baseline_start_date|
        grepl("76.0", icd10_a96) & icd10d_a96 < baseline_start_date|
        grepl("76.0", icd10_a97) & icd10d_a97 < baseline_start_date|
        grepl("76.0", icd10_a98) & icd10d_a98 < baseline_start_date|
        grepl("76.0", icd10_a99) & icd10d_a99 < baseline_start_date|
        grepl("76.0", icd10_a100) & icd10d_a100 < baseline_start_date ~ "yes",
      TRUE ~ "no"
    )
  )

data <- data %>%
  mutate(
    cystectomy = case_when(
      grepl("J18", opcs4_a0) & p41282_a0 < baseline_start_date|
        grepl("J18", opcs4_a1) & p41282_a1 < baseline_start_date|
        grepl("J18", opcs4_a2) & p41282_a2 < baseline_start_date|
        grepl("J18", opcs4_a3) & p41282_a3 < baseline_start_date|
        grepl("J18", opcs4_a4) & p41282_a4 < baseline_start_date|
        grepl("J18", opcs4_a5) & p41282_a5 < baseline_start_date|
        grepl("J18", opcs4_a6) & p41282_a6 < baseline_start_date|
        grepl("J18", opcs4_a7) & p41282_a7 < baseline_start_date|
        grepl("J18", opcs4_a8) & p41282_a8 < baseline_start_date|
        grepl("J18", opcs4_a9) & p41282_a9 < baseline_start_date|
        grepl("J18", opcs4_a10) & p41282_a10 < baseline_start_date|
        grepl("J18", opcs4_a11) & p41282_a11 < baseline_start_date|
        grepl("J18", opcs4_a12) & p41282_a12 < baseline_start_date|
        grepl("J18", opcs4_a13) & p41282_a13 < baseline_start_date|
        grepl("J18", opcs4_a14) & p41282_a14 < baseline_start_date|
        grepl("J18", opcs4_a15) & p41282_a15 < baseline_start_date|
        grepl("J18", opcs4_a16) & p41282_a16 < baseline_start_date|
        grepl("J18", opcs4_a17) & p41282_a17 < baseline_start_date|
        grepl("J18", opcs4_a18) & p41282_a18 < baseline_start_date|
        grepl("J18", opcs4_a19) & p41282_a19 < baseline_start_date|
        grepl("J18", opcs4_a20) & p41282_a20 < baseline_start_date|
        grepl("J18", opcs4_a21) & p41282_a21 < baseline_start_date|
        grepl("J18", opcs4_a22) & p41282_a22 < baseline_start_date|
        grepl("J18", opcs4_a23) & p41282_a23 < baseline_start_date|
        grepl("J18", opcs4_a24) & p41282_a24 < baseline_start_date|
        grepl("J18", opcs4_a25) & p41282_a25 < baseline_start_date|
        grepl("J18", opcs4_a26) & p41282_a26 < baseline_start_date|
        grepl("J18", opcs4_a27) & p41282_a27 < baseline_start_date|
        grepl("J18", opcs4_a28) & p41282_a28 < baseline_start_date|
        grepl("J18", opcs4_a29) & p41282_a29 < baseline_start_date|
        grepl("J18", opcs4_a30) & p41282_a30 < baseline_start_date|
        grepl("J18", opcs4_a31) & p41282_a31 < baseline_start_date|
        grepl("J18", opcs4_a32) & p41282_a32 < baseline_start_date|
        grepl("J18", opcs4_a33) & p41282_a33 < baseline_start_date|
        grepl("J18", opcs4_a34) & p41282_a34 < baseline_start_date|
        grepl("J18", opcs4_a35) & p41282_a35 < baseline_start_date|
        grepl("J18", opcs4_a36) & p41282_a36 < baseline_start_date|
        grepl("J18", opcs4_a37) & p41282_a37 < baseline_start_date|
        grepl("J18", opcs4_a38) & p41282_a38 < baseline_start_date|
        grepl("J18", opcs4_a39) & p41282_a39 < baseline_start_date|
        grepl("J18", opcs4_a40) & p41282_a40 < baseline_start_date|
        grepl("J18", opcs4_a41) & p41282_a41 < baseline_start_date|
        grepl("J18", opcs4_a42) & p41282_a42 < baseline_start_date|
        grepl("J18", opcs4_a43) & p41282_a43 < baseline_start_date|
        grepl("J18", opcs4_a44) & p41282_a44 < baseline_start_date|
        grepl("J18", opcs4_a45) & p41282_a45 < baseline_start_date|
        grepl("J18", opcs4_a46) & p41282_a46 < baseline_start_date|
        grepl("J18", opcs4_a47) & p41282_a47 < baseline_start_date|
        grepl("J18", opcs4_a48) & p41282_a48 < baseline_start_date|
        grepl("J18", opcs4_a49) & p41282_a49 < baseline_start_date|
        grepl("J18", opcs4_a50) & p41282_a50 < baseline_start_date|
        grepl("J18", opcs4_a51) & p41282_a51 < baseline_start_date|
        grepl("J18", opcs4_a52) & p41282_a52 < baseline_start_date|
        grepl("J18", opcs4_a53) & p41282_a53 < baseline_start_date|
        grepl("J18", opcs4_a54) & p41282_a54 < baseline_start_date|
        grepl("J18", opcs4_a55) & p41282_a55 < baseline_start_date|
        grepl("J18", opcs4_a56) & p41282_a56 < baseline_start_date|
        grepl("J18", opcs4_a57) & p41282_a57 < baseline_start_date|
        grepl("J18", opcs4_a58) & p41282_a58 < baseline_start_date|
        grepl("J18", opcs4_a59) & p41282_a59 < baseline_start_date|
        grepl("J18", opcs4_a60) & p41282_a60 < baseline_start_date|
        grepl("J18", opcs4_a61) & p41282_a61 < baseline_start_date|
        grepl("J18", opcs4_a62) & p41282_a62 < baseline_start_date|
        grepl("J18", opcs4_a63) & p41282_a63 < baseline_start_date|
        grepl("J18", opcs4_a64) & p41282_a64 < baseline_start_date|
        grepl("J18", opcs4_a65) & p41282_a65 < baseline_start_date|
        grepl("J18", opcs4_a66) & p41282_a66 < baseline_start_date|
        grepl("J18", opcs4_a67) & p41282_a67 < baseline_start_date|
        grepl("J18", opcs4_a68) & p41282_a68 < baseline_start_date|
        grepl("J18", opcs4_a69) & p41282_a69 < baseline_start_date|
        grepl("J18", opcs4_a70) & p41282_a70 < baseline_start_date|
        grepl("J18", opcs4_a71) & p41282_a71 < baseline_start_date|
        grepl("J18", opcs4_a72) & p41282_a72 < baseline_start_date ~ "yes",
      TRUE ~ "no"
    )
  )

data %>%
  group_by(cystectomy) %>%
  count()
# creating dataset with only liver cancer diagnoses after baseline:

data_liver <- data %>%
    filter(cancer_diag0 == 'C22.0 Liver cell carcinoma' | cancer_diag0 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag1 == 'C22.0 Liver cell carcinoma' | cancer_diag1 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag2 == 'C22.0 Liver cell carcinoma' | cancer_diag2 == 'C22.1 Intrahepatic bile duct carcinoma' |
               cancer_diag3 == 'C22.0 Liver cell carcinoma' | cancer_diag3 == 'C22.1 Intrahepatic bile duct carcinoma'
    )
