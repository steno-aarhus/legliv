library(tidyverse)
library(lubridate) # For creating baseline age
library(stringr) # for renaming several columns at once

data <- read_csv("data/data.csv")
ukb_dataset <- data

ready_data <- function(data) {
  # Removing participants who did not complete 2 or more diet questionnaires
  data <- data %>%
    filter(p20077 >= 2) %>%
    mutate(id = 1:n(), .before = everything())
  # Split the diagnosis-variable into separate columns based on delimiter "|" (ICD10 codes)
  data <- data %>%
    separate_wider_delim(p41270,
      delim = "|",
      names = paste0("p41270var_a", 0:258), too_few = "debug"
    )
  # Split the diagnosis-variable into separate columns based on delimiter "|" (OPCS4 codes)
  data <- data %>%
    separate_wider_delim(p41272,
      delim = "|",
      names = paste0("p41272var_a", 0:125), too_few = "debug"
    )
  return(data)
}
data <- ready_data(data)

cancer_longer <- function(data) {
  icd10_subset <- data %>%
    select(matches("p41270|p41280|id")) %>%
    pivot_longer(
      cols = matches("_a[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    )
  cancer_subset <- data %>%
    select(matches("p40006|p40005|id")) %>%
    pivot_longer(
      cols = matches("_i[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    )
  return(list(
    icd10_subset = icd10_subset,
    cancer_subset = cancer_subset
  ))
}
data_list_longer <- cancer_longer(data)
icd10_subset <- data_list_longer$icd10_subset
cancer_subset <- data_list_longer$cancer_subset

icd10_hcc <- function(data) {
  icd10_hcc <- icd10_subset %>%
    mutate(
      icd10_hcc_date = ifelse(str_detect(p41270var, "C22.0"),
        as.character(c_across(starts_with("p41280"))),
        NA
      ),
      icd10_hcc_date = as.Date(icd10_hcc_date, format = "%Y-%m-%d")
    )
  first_non_na_hcc <- icd10_hcc %>%
    filter(!is.na(icd10_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  data <- data %>%
    left_join(first_non_na_hcc %>% select(id, icd10_hcc_date), by = "id")
  return(data)
}
data <- icd10_hcc(data)

icd10_icc <- function(data) {
  icd10_icc <- icd10_subset %>%
    mutate(
      icd10_icc_date = ifelse(str_detect(p41270var, "C22.1"),
        as.character(c_across(starts_with("p41280"))),
        NA
      ),
      icd10_icc_date = as.Date(icd10_icc_date, format = "%Y-%m-%d")
    )
  first_non_na_icc <- icd10_icc %>%
    filter(!is.na(icd10_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  data <- data %>%
    left_join(first_non_na_icc %>% select(id, icd10_icc_date), by = "id")
  return(data)
}
data <- icd10_icc(data)

cancer_hcc <- function(data) {
  cancer_hcc <- cancer_subset %>%
    mutate(
      cancer_hcc_date = ifelse(str_detect(p40006, "C22.0"),
        as.character(c_across(starts_with("p40005"))),
        NA
      ),
      cancer_hcc_date = as.Date(cancer_hcc_date, format = "%Y-%m-%d")
    )
  first_non_na_hcc_c <- cancer_hcc %>%
    filter(!is.na(cancer_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  data <- data %>%
    left_join(first_non_na_hcc_c %>% select(id, cancer_hcc_date), by = "id")
  return(data)
}
data <- cancer_hcc(data)

cancer_icc <- function(data) {
  cancer_icc <- cancer_subset %>%
    mutate(
      cancer_icc_date = ifelse(str_detect(p40006, "C22.1"),
        as.character(c_across(starts_with("p40005"))),
        NA
      ),
      cancer_icc_date = as.Date(cancer_icc_date, format = "%Y-%m-%d")
    )
  first_non_na_icc_c <- cancer_icc %>%
    filter(!is.na(cancer_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  data <- data %>%
    left_join(first_non_na_icc_c %>% select(id, cancer_icc_date), by = "id")
  return(data)
}
data <- cancer_icc(data)

remove_timestamp <- function(data) {
  # Removing specific time stamp from date of completed questionnaires:
  data <- data %>%
    mutate(
      p105010_i0 = substr(p105010_i0, 1, 10),
      p105010_i1 = substr(p105010_i1, 1, 10),
      p105010_i2 = substr(p105010_i2, 1, 10),
      p105010_i3 = substr(p105010_i3, 1, 10),
      p105010_i4 = substr(p105010_i4, 1, 10)
    )
  return(data)
}
data <- remove_timestamp(data)

baseline_date <- function(data) {
  baseline_start_date <- data %>%
    select(starts_with("p105010_i"), id) %>%
    pivot_longer(
      cols = starts_with("p105010_i"),
      names_to = "questionnaire",
      values_to = "completion_date"
    ) %>%
    filter(!is.na(completion_date)) %>%
    group_by(id) %>%
    arrange(completion_date) %>%
    mutate(last_questionnaire_date = lag(completion_date)) %>%
    filter(!is.na(last_questionnaire_date)) %>%
    filter(is.na(lead(completion_date))) %>%
    rename(baseline_start_date = completion_date) %>%
    ungroup()
  data <- data %>%
    left_join(baseline_start_date %>% select(id, baseline_start_date), by = "id")
  return(data)
}
data <- baseline_date(data)

icd10_diabetes <- function(data) {
  diabetes <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      diabetes_ins_non = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("E11", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      ),
      diabetes_ins = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("E10", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(diabetes %>% select(id, diabetes_ins_non, diabetes_ins), by = "id")
  return(data)
}
data <- icd10_diabetes(data)

icd10_cholelith <- function(data) {
  cholelith <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      cholelith = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K80", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(cholelith %>% select(id, cholelith), by = "id")
  return(data)
}
data <- icd10_cholelith(data)

icd10_nafld <- function(data) {
  nafld <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      nafld = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K76.0", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(nafld %>% select(id, nafld), by = "id")
  return(data)
}
data <- icd10_nafld(data)

icd10_inflam_liver <- function(data) {
  inflam_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      inflam_liver = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K75", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(inflam_liver %>% select(id, inflam_liver), by = "id")
  return(data)
}
data <- icd10_inflam_liver(data)

icd10_alc_liver <- function(data) {
  alc_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      alc_liver = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K70", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(alc_liver %>% select(id, alc_liver), by = "id")
  return(data)
}
data <- icd10_alc_liver(data)

icd10_cirr_liver <- function(data) {
  cirr_liver <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      cirr_liver = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("K74", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(cirr_liver %>% select(id, cirr_liver), by = "id")
  return(data)
}
data <- icd10_cirr_liver(data)

icd10_viral_hepatitis <- function(data) {
  viral_hepatitis <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), baseline_start_date, id) %>%
    mutate(
      viral_hepatitis = if_else(
        rowSums(across(starts_with("p41270var_a"), ~ grepl("B18", .x)) &
          across(starts_with("p41280_a"), ~ .x < baseline_start_date)) > 0,
        "Yes",
        "No"
      )
    )
  data <- data %>%
    left_join(viral_hepatitis %>% select(id, viral_hepatitis), by = "id")
  return(data)
}
data <- icd10_viral_hepatitis(data)

cystectomy <- function(data) {
  cystect <- data %>%
    select(starts_with("p41272"), starts_with("p41282"), baseline_start_date, id) %>%
    mutate(
      cystectomy = if_else(
        rowSums(across(starts_with("p41272var_a"), ~ grepl("J18", .x)) &
          across(starts_with("p41282_a"), ~ .x < baseline_start_date)) > 0,
        "yes",
        "no"
      )
    )
  data <- data %>%
    left_join(cystect %>% select(id, cystectomy), by = "id")
  remove(cystect)
  return(data)
}
data <- cystectomy(data)

covariates <- function(data) {
  # renaming variables to appropriate names and mutating covariates:
  data <- data %>%
    mutate(
      sex = p31,
      wc = p48_i0,
      education = p6138_i0,
      bmi = p21001_i0,
      phys_acti = p22040_i0,
      tdi = p22189,
      spouse = if_else(p709_i0 == 1, "Yes", "No"),
      smoking = ifelse(p20116_i0 == "Prefer not to answer", "Never", p20116_i0),
      smoking = factor(smoking, levels = c("Never", "Previous", "Current")),
      education = case_when(
        grepl("College", education, ignore.case = TRUE) ~ "High",
        grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "Intermediate",
        grepl("O levels/GCSEs", education, ignore.case = TRUE) ~ "Intermediate",
        grepl("CSEs", education, ignore.case = TRUE) ~ "Low",
        grepl("NVQ or HND", education, ignore.case = TRUE) ~ "Low",
        grepl("Other professional", education, ignore.case = TRUE) ~ "Low",
        grepl("None of the above", education, ignore.case = TRUE) ~ "Low",
        grepl("Prefer not to answer", education, ignore.case = TRUE) ~ "Low",
        TRUE ~ as.character(education)
      ),
      education = factor(education, levels = c("High", "Intermediate", "Low")),
      ethnicity = case_when(
        p21000_i0 == "African" ~ "Other", # check the layers to the variable
        p21000_i0 == "Any other Black background" ~ "Other",
        p21000_i0 == "Asian or Asian British" ~ "Other",
        p21000_i0 == "Bangladeshi" ~ "Other",
        p21000_i0 == "Chinese" ~ "Other",
        p21000_i0 == "Indian" ~ "Other",
        p21000_i0 == "Pakistani" ~ "Other",
        p21000_i0 == "Any other Asian background" ~ "Other",
        p21000_i0 == "British" ~ "White",
        p21000_i0 == "Any other white background" ~ "White",
        p21000_i0 == "Irish" ~ "White",
        p21000_i0 == "White" ~ "White",
        p21000_i0 == "White and Asian" ~ "Other",
        p21000_i0 == "White and Black African" ~ "Other",
        p21000_i0 == "White and Black Caribbean" ~ "Other",
        p21000_i0 == "Any other mixed background" ~ "Other",
        p21000_i0 == "Caribbean" ~ "Other",
        p21000_i0 == "Do not know" ~ "Other",
        p21000_i0 == "Other ethnic group" ~ "Other",
        p21000_i0 == "Prefer not to answer" ~ "Other"
      ),
      ethnicity = factor(ethnicity, levels = c("White", "Other")),
      bmi_category = case_when(
        bmi < 25 ~ "Normal weight",
        bmi >= 25 & bmi < 30 ~ "Overweight",
        bmi >= 30 ~ "Obese"
      ),
      bmi_category = factor(bmi_category, levels = c("Normal weight", "Overweight", "Obese")),
      exercise = case_when(
        p22040_i0 < 600 ~ "Low",
        p22040_i0 >= 600 & p22040_i0 < 3000 ~ "Moderate",
        p22040_i0 > 3000 ~ "High"
      ),
      exercise = factor(exercise, levels = c("Low", "Moderate", "High")),
      diabetes = if_else(diabetes_ins_non == "No" & diabetes_ins == "No", "No", "Yes")
    )
  return(data)
}
data <- covariates(data)

other_variables <- function(data) {
  data <- data %>%
    mutate(
      birth_year = p34,
      month_of_birth = p52,
      l2fu_d = p191,
      age_recruit = p21022,
      dead_date = p40000_i0,
      dead_cause = p40001_i0,
      age_dead = p40007_i0
    )
  return(data)
}
data <- other_variables(data)

# Function with food group variables:
# Average dietary intake of food groups -----------------------------------
calculate_food_intake <- function(data) {
  # estimating average daily and weekly intakes of food groups in g
  data <- data %>%
    # creating food groups from UKB Aurora Perez
    mutate(
      # refined cereals
      cereal_refined_daily = (rowSums(select(., starts_with("p26113") | starts_with("p26079") |
        starts_with("p26071") | starts_with("p26072") |
        starts_with("p26073") | starts_with("p26075") |
        starts_with("p26068") | starts_with("p26083")), na.rm = TRUE)) / p20077,
      cereal_refined_daily_15 = (cereal_refined_daily) / 15,
      # whole-grain cereals
      whole_grain_daily = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
        starts_with("p26077") | starts_with("p26078") |
        starts_with("p26105") | starts_with("p26114")), na.rm = TRUE) / p20077,
      whole_grain_daily_15 = whole_grain_daily / 15,
      # mixed dishes
      mixed_dish_daily = rowSums(select(., starts_with("p26128") | starts_with("p26097") |
        starts_with("p26116") | starts_with("p26135") |
        starts_with("p26139")), na.rm = TRUE) / p20077,
      mixed_dish_daily_15 = mixed_dish_daily / 15,
      # dairy
      dairy_daily = rowSums(select(., starts_with("p26154") | starts_with("p26087") |
        starts_with("p26096") | starts_with("p26102") |
        starts_with("p26103") | starts_with("p26099") |
        starts_with("p26131") | starts_with("p26133") |
        starts_with("p26150")), na.rm = TRUE) / p20077,
      dairy_daily_15 = dairy_daily / 15,
      # fats and spread
      fats_daily = rowSums(select(., starts_with("p26112") | starts_with("p26062") |
        starts_with("p26063") | starts_with("p26155") |
        starts_with("p26110") | starts_with("p26111")), na.rm = TRUE),
      fats_daily_15 = fats_daily / 15,
      # fruit
      fruit_daily = rowSums(select(., starts_with("p26089") | starts_with("p26090") |
        starts_with("p26091") | starts_with("p26092") |
        starts_with("p26093") | starts_with("p26094")), na.rm = TRUE) / p20077,
      fruit_daily_15 = fruit_daily / 15,
      # nuts and seeds
      nut_daily = rowSums(select(., starts_with("p26107") | starts_with("p26108")), na.rm = TRUE) / p20077,
      nut_daily_15 = nut_daily / 15,
      # vegetables
      veggie_daily = (rowSums(select(., starts_with("p26065") | starts_with("p26098") |
        starts_with("p26115") | starts_with("p26123") |
        starts_with("p26125") | starts_with("p26143") |
        starts_with("p26146") | starts_with("p26147")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + # assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE)) / p20077, # assuming half peas half corn
      veggie_daily_15 = veggie_daily / 15,
      # potatoes
      potato_daily = rowSums(select(., starts_with("p26118") | starts_with("p26119") |
        starts_with("p26120")), na.rm = TRUE) / p20077,
      potato_daily_15 = potato_daily / 15,
      # eggs
      egg_daily = rowSums(select(., starts_with("p26088")), na.rm = TRUE) / p20077,
      egg_daily_15 = egg_daily / 15,
      # meat substitutes
      meat_sub_daily = rowSums(select(., starts_with("p26145")), na.rm = TRUE) / p20077,
      meat_sub_daily_15 = meat_sub_daily / 15,
      # non-alcoholic beverages
      non_alc_beverage_daily = rowSums(select(., starts_with("p26124") | starts_with("p26141") |
        starts_with("p26142") | starts_with("p26148") |
        starts_with("p26081") | starts_with("p26082") |
        starts_with("p26095") | starts_with("p26126") |
        starts_with("p26127")), na.rm = TRUE) / p20077,
      non_alc_beverage_daily_15 = non_alc_beverage_daily / 15,
      # alcoholic beverages
      alc_beverage_daily = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
        starts_with("p26153") | starts_with("p26067") |
        starts_with("p26138")), na.rm = TRUE) / p20077,
      alc_beverage_daily_15 = alc_beverage_daily / 15,
      # sugar, preserves, cakes & confectionery, snacks
      snack_daily = rowSums(select(., starts_with("p26106") | starts_with("p26140") |
        starts_with("p26134") | starts_with("p26084") |
        starts_with("p26085") | starts_with("p26064") |
        starts_with("p26080")), na.rm = TRUE) / p20077,
      snack_daily_15 = snack_daily / 15,
      # Sauces & condiments
      sauce_daily = rowSums(select(., starts_with("p26129") | starts_with("p26130")), na.rm = TRUE) / p20077,
      sauce_daily_15 = sauce_daily / 15,
      # poultry
      poultry_daily = rowSums(select(., starts_with("p26121") | starts_with("p26069")), na.rm = TRUE) / p20077,
      poultry_daily_15 = poultry_daily / 15,
      # fish
      fish_daily = rowSums(select(., starts_with("p26070") | starts_with("p26109") |
        starts_with("p26132") | starts_with("p26149")), na.rm = TRUE) / p20077,
      fish_daily_15 = fish_daily / 15,
      # alcohol
      alcohol_daily = rowSums(select(., starts_with("p26030")), na.rm = TRUE) / p20077,
      # legumes
      legume_daily = (rowSums(select(., starts_with("p26086") | starts_with("p26101") |
        starts_with("p26136") | starts_with("p26137")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + # assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE)) / p20077, # assuming half peas half corn
      legume_daily_15 = legume_daily / 15,
      # red meats
      red_meat_daily = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
        starts_with("p26104") | starts_with("p26117")), na.rm = TRUE) / p20077,
      red_meat_daily_15 = red_meat_daily / 15,
      # processed meat
      proc_meat_daily = rowSums(select(., starts_with("p26122")), na.rm = TRUE) / p20077,
      proc_meat_daily_15 = proc_meat_daily / 15
    )
  return(data)
}
data <- calculate_food_intake(data)

food_intake_extra <- function(data) {
  data <- data |>
    mutate(
      # baked beans and pulses separated from other legumes
      pulse_daily = rowSums(select(., starts_with("p26101")), na.rm = TRUE) / p20077,
      pulse_daily_15 = pulse_daily / 15,
      legume_other_daily = (rowSums(select(., starts_with("p26086") |
        starts_with("p26136") | starts_with("p26137")), na.rm = TRUE) +
        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + # assuming half hummus half guacamole
        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE)) / p20077, # assuming half peas half corn
      legume_other_daily_15 = legume_other_daily / 15,
      # total meat
      total_meat_daily = red_meat_daily + proc_meat_daily,
      total_meat_daily_15 = total_meat_daily / 15,
      # total weight of all foods
      total_weight_food_daily = legume_daily + red_meat_daily + proc_meat_daily +
        poultry_daily + fish_daily + dairy_daily + egg_daily + cereal_refined_daily +
        whole_grain_daily + veggie_daily + potato_daily + fruit_daily + nut_daily +
        meat_sub_daily + snack_daily + mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily,
      # total energy of all foods and beverages
      total_energy_food_daily = rowSums(select(., starts_with("p26002")), na.rm = TRUE) / p20077
    )
  return(data)
}
data <- food_intake_extra(data)

legumes_strat <- function(data) {
  data <- data %>%
    filter(legume_daily != 0) %>%
    mutate(
      legume_quintile = ntile(legume_daily, 4),
      legume_category = factor(legume_quintile, labels = c("Low", "Medium", "High", "Highest"))
    ) %>%
    bind_rows(data %>%
      filter(legume_daily == 0) %>%
      mutate(legume_category = "Lowest")) %>%
    mutate(legume_category = factor(legume_category, levels = c("Lowest", "Low", "Medium", "High", "Highest")))
  return(data)
}
data <- legumes_strat(data)

birth_date <- function(data) {
  # Merging birth year and month of birth into one column:
  month_names <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  data <- data %>%
    mutate(month_of_birth_num = sprintf("%02d", match(month_of_birth, month_names)))
  data <- data %>%
    unite(date_birth, birth_year, month_of_birth_num, sep = "-")
  # adding 15 as DD for all participants:
  data$date_birth <- as.Date(paste0(data$date_birth, "-15"))
  return(data)
}
data <- birth_date(data)

baseline_age <- function(data) {
  # Creating age at baseline:
  data <- data %>%
    mutate(age_at_baseline = year(baseline_start_date) - year(date_birth) -
      ifelse(month(baseline_start_date) < month(date_birth) |
        (month(baseline_start_date) == month(date_birth) &
          day(baseline_start_date) < day(date_birth)), 1, 0))
  return(data)
}
data <- baseline_age(data)

follow_up <- function(data) {
  # Creating age at loss to follow-up:
  data <- data %>%
    mutate(age_l2fu = as.numeric(difftime(l2fu_d, date_birth, units = "days")) / 365.25)
  # Removing participants who were lost to follow-up before baseline:
  data <- data %>%
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date)
  return(data)
}
data <- follow_up(data)

status <- function(data) {
  # Creating status, status date and status age:
  data <- data %>%
    mutate(
      earliest_date = pmin(dead_date, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date, l2fu_d, na.rm = TRUE) %>%
        coalesce(as.Date("2022-12-31")),
      status = case_when(
        earliest_date == cancer_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == cancer_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == icd10_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == icd10_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == dead_date ~ "Censored",
        TRUE ~ "Censored"
      ),
      status_date = earliest_date,
      status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
      status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25, # Calculating age in years
      study_time = status_age - age_at_baseline
    )
  return(data)
}
data <- status(data)

remove_liver_before <- function(data) {
  data_liver_before <- data %>%
    filter(cancer_hcc_date <= baseline_start_date |
      cancer_icc_date <= baseline_start_date |
      icd10_hcc_date <= baseline_start_date |
      icd10_icc_date <= baseline_start_date)
  data <- data %>%
    anti_join(data_liver_before %>% select(id, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date), by = "id")
  return(data)
}
data <- remove_liver_before(data)

stratify_prepare1 <- function(data) {
  data_wc_high <- data %>%
    filter(sex == "Male" & wc >= 102 | sex == "Female" & wc >= 88)

  data_wc_low <- data %>%
    filter(sex == "Male" & wc < 102 | sex == "Female" & wc < 88)

  data_diabetes_yes <- data %>%
    filter(diabetes == "Yes")

  data_diabetes_no <- data %>%
    filter(diabetes == "No")

  data_men <- data %>%
    filter(sex == "Male")

  data_women <- data %>%
    filter(sex == "Female")

  return(list(
    data_wc_high = data_wc_high,
    data_wc_low = data_wc_low,
    data_diabetes_yes = data_diabetes_yes,
    data_diabetes_no = data_diabetes_no,
    data_men = data_men,
    data_women = data_women
  ))
}
data_list <- stratify_prepare1(data)
data_wc_high <- data_list$data_wc_high
data_wc_low <- data_list$data_wc_low
data_diabetes_yes <- data_list$data_diabetes_yes
data_diabetes_no <- data_list$data_diabetes_no
data_men <- data_list$data_men
data_women <- data_list$data_women

stratify_prepare2 <- function(data) {
  data_hcc <- data %>%
    mutate(
      earliest_date = pmin(dead_date, cancer_hcc_date, icd10_hcc_date, l2fu_d, na.rm = TRUE) %>%
        coalesce(as.Date("2022-12-31")),
      status = case_when(
        earliest_date == cancer_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == icd10_hcc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == dead_date ~ "Censored",
        TRUE ~ "Censored"
      ),
      status_date = earliest_date,
      status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
      status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25, # Calculating age in years
      time = status_age - age_at_baseline
    ) %>%
    select(-earliest_date)
  return(data_hcc)
}
data_hcc <- stratify_prepare2(data)

stratify_prepare3 <- function(data) {
  data_icc <- data %>%
    mutate(
      earliest_date = pmin(dead_date, cancer_icc_date, icd10_icc_date, l2fu_d, na.rm = TRUE) %>%
        coalesce(as.Date("2022-12-31")),
      status = case_when(
        earliest_date == cancer_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == icd10_icc_date & earliest_date > baseline_start_date ~ "Liver cancer",
        earliest_date == l2fu_d & earliest_date > baseline_start_date ~ "Censored",
        earliest_date == dead_date ~ "Censored",
        TRUE ~ "Censored"
      ),
      status_date = earliest_date,
      status_date = if_else(is.na(status_date), as.Date("2022-12-31"), status_date),
      status_age = as.numeric(difftime(earliest_date, date_birth, units = "days")) / 365.25, # Calculating age in years
      time = status_age - age_at_baseline
    ) %>%
    select(-earliest_date)
  return(data_icc)
}
data_icc <- stratify_prepare3(data)

remove_liver_disease_before <- function(data) {
  data_liver_disease_before <- data %>%
    filter(
      inflam_liver == "Yes" | alc_liver == "Yes" | cirr_liver == "Yes" | viral_hepatitis == "Yes" |
        nafld == "Yes"
    )
  data <- data %>%
    anti_join(data_liver_disease_before %>% select(id, inflam_liver, alc_liver, cirr_liver, viral_hepatitis, nafld), by = "id")
  return(data)
}
data_sens <- remove_liver_disease_before(data)

data <- data %>%
  mutate(
    liver_disease = if_else(nafld == "Yes" | inflam_liver == "Yes" || alc_liver == "Yes" | cirr_liver == "Yes" | viral_hepatitis == "Yes", "Yes", "No")
  )

filter_ques_comp_n <- function(data) {
  data_ques_3 <- data %>%
    filter(p20077 >= 3)
  data_ques_4 <- data %>%
    filter(p20077 >= 4)
  data_ques_5 <- data %>%
    filter(p20077 >= 5)
  return(list(
    data_ques_3 = data_ques_3,
    data_ques_4 = data_ques_4,
    data_ques_5 = data_ques_5
  ))
}
ques_comp_list <- filter_ques_comp_n(data)
data_ques_3 <- ques_comp_list$data_ques_3
data_ques_4 <- ques_comp_list$data_ques_4
data_ques_5 <- ques_comp_list$data_ques_5

data_liver <- data %>%
  filter(status == "Liver cancer")
