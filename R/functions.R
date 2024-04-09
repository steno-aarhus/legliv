# Prepare data ------------------------------------------------------------

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
  data <- data %>%
    select(starts_with("p41270"), starts_with("p41280")) %>%
    select_if(~ !all(is.na(.))) %>%
    bind_cols(data %>% select(-starts_with("p41270"), -starts_with("p41280")))
  # Split the diagnosis-variable into separate columns based on delimiter "|" (ICD9 codes)
  data <- data %>%
    separate_wider_delim(p41271,
      delim = "|",
      names = paste0("p41271var_a", 0:46), too_few = "debug"
    )
  # Split the diagnosis-variable into separate columns based on delimiter "|" (OPCS4 codes)
  data <- data %>%
    separate_wider_delim(p41272,
      delim = "|",
      names = paste0("p41272var_a", 0:125), too_few = "debug"
    )
  # Split the diagnosis-variable into separate columns based on delimiter "|" (OPCS3 codes)
  data <- data %>%
    separate_wider_delim(p41273,
      delim = "|",
      names = paste0("p41273var_a", 0:15), too_few = "debug"
    )
  return(data)
}

# Find liver cancer cases -------------------------------------------------

icd10_longer_subset <- function(data) {
  data %>%
    select(matches("p41270|p41280|id")) %>%
    pivot_longer(
      cols = matches("_a[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    )
}

cancer_longer_subset <- function(data) {
  data %>%
    select(matches("p40006|p40005|id")) %>%
    pivot_longer(
      cols = matches("_i[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    )
}

icd10_hcc <- function(data) {
  data %>%
    # TODO: I don't this does what you think it's doing. Need to assess in RAP
    mutate(
      icd10_hcc_date = ifelse(str_detect(p41270var, "C22.0"),
        as.character(c_across(starts_with("p41280"))),
        NA
      ),
      icd10_hcc_date = as.Date(icd10_hcc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(icd10_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, icd10_hcc_date)
}

icd10_icc <- function(data) {
  data %>%
    mutate(
      icd10_icc_date = ifelse(str_detect(p41270var, "C22.1"),
        as.character(c_across(starts_with("p41280"))),
        NA
      ),
      icd10_icc_date = as.Date(icd10_icc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(icd10_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, icd10_icc_date)
}

cancer_hcc <- function(data) {
  data %>%
    mutate(
      cancer_hcc_date = ifelse(str_detect(p40006, "C22.0"),
        as.character(c_across(starts_with("p40005"))),
        NA
      ),
      cancer_hcc_date = as.Date(cancer_hcc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(cancer_hcc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, cancer_hcc_date)
}

cancer_icc <- function(data) {
  data %>%
    mutate(
      cancer_icc_date = ifelse(str_detect(p40006, "C22.1"),
        as.character(c_across(starts_with("p40005"))),
        NA
      ),
      cancer_icc_date = as.Date(cancer_icc_date, format = "%Y-%m-%d")
    ) |>
    filter(!is.na(cancer_icc_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() |>
    select(id, cancer_icc_date)
}

# Define baseline date ----------------------------------------------------

remove_timestamp <- function(data) {
  # Removing specific time stamp from date of completed questionnaires:
  data %>%
    mutate(across(
      c(
        p105010_i0,
        p105010_i1,
        p105010_i2,
        p105010_i3,
        p105010_i4
      ), ~ substr(.x, 1, 10)
    ))
}

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

covariates <- function(data) {
  data <- data %>%
    mutate(
      sex = p31,
      wc = p48_i0,
      education = p6138_i0,
      bmi = p21001_i0,
      tdi = p22189,
      spouse = if_else(p709_i0 == 1, "Yes", "No"),
      spouse = as.factor(spouse),
      smoking = if_else(p20116_i0 == "Prefer not to answer" | p20116_i0 == "Never", "Never", "Ever"),
      smoking = factor(smoking, levels = c("Never", "Ever")),
      smoking_pack = p20162_i0,
      smoking_pack = replace_na(smoking_pack, 0),
      # TODO: We need to have a bigger discussion about this in the UK Biobank group.
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
      education = as.factor(education),
      # TODO: We'll need to have a bigger discussion about this variable, plus move it over into ukbAid.
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
      # TODO: Move this over into ukbAid
      bmi_category = case_when(
        bmi < 25 ~ "Normal weight",
        bmi >= 25 & bmi < 30 ~ "Overweight",
        bmi >= 30 ~ "Obese"
      ),
      bmi_category = factor(bmi_category, levels = c("Normal weight", "Overweight", "Obese")),
      bmi_category = as.factor(bmi_category),
      exercise = as.factor(p22035_i0)
    )
  return(data)
}

metabolic_syndrome <- function(data) {
  data <- data %>%
    mutate(
      trigly = p30870_i0,
      hdl = p30760_i0,
      med_men = p6177_i0,
      med_women = p6153_i0,
      glucose = p30750_i0
    )
}

other_variables <- function(data) {
  data <- data %>%
    mutate(
      birth_year = p34,
      month_of_birth = p52,
      recruit_to_baseline = as.numeric(difftime(baseline_start_date, p53_i0, units = "days")) / 365.25,
      l2fu_d = p191,
      age_recruit = p21022,
      dead_date = p40000_i0,
      dead_cause = p40001_i0,
      age_dead = p40007_i0
    )
  data <- data %>%
    mutate(across(starts_with("p100020_i"), ~ coalesce(., "Yes"))) %>%
    mutate(
      typical_diet = if_else(p100020_i0 == "No" | p100020_i1 == "No" | p100020_i2 == "No" | p100020_i3 == "No" | p100020_i4 == "No", "No", "Yes")
    )
  return(data)
}

# TODO: Redo this to mimic what was done in Fie's repository.
calculate_food_intake <- function(data) {
  # estimating average daily and weekly intakes of food groups in g
  data <- data %>%
    # creating food groups from UKB Aurora Perez
    mutate(
      # legumes
      legume_daily = (rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                       starts_with("p26136") | starts_with("p26137")), na.rm = TRUE) +
                        rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + # assuming half hummus half guacamole
                        rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE)) / p20077, # assuming half peas half corn
      legume_daily_15 = legume_daily / 15,
      legume_daily_75 = legume_daily / 7.5,
      legume_daily_30 = legume_daily / 30,
      # red meats
      red_meat_daily = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
                                        starts_with("p26104") | starts_with("p26117")), na.rm = TRUE) / p20077,
      red_meat_daily_15 = red_meat_daily / 15,
      red_meat_daily_75 = red_meat_daily / 7.5,
      red_meat_daily_30 = red_meat_daily / 30,
      # processed meat
      proc_meat_daily = rowSums(select(., starts_with("p26122")), na.rm = TRUE) / p20077,
      proc_meat_daily_15 = proc_meat_daily / 15,
      proc_meat_daily_75 = proc_meat_daily / 7.5,
      proc_meat_daily_30 = proc_meat_daily / 30,
      # animal foods
      animal_foods = rowSums(select(., starts_with("p26069") | starts_with("p26121") | # Poultry
                                      starts_with("p26070") | starts_with("p26109") | # Fish
                                      starts_with("p26132") | starts_with("p26149") |
                                      starts_with("p26062") | starts_with("p26063") | # Dairy & fats
                                      starts_with("p26084") | starts_with("p26087") |
                                      starts_with("p26096") | starts_with("p26099") |
                                      starts_with("p26102") | starts_with("p26103") |
                                      starts_with("p26131") | starts_with("p26133") |
                                      starts_with("p26150") | starts_with("p26154") |
                                      starts_with("p26088") | # Eggs
                                      starts_with("p26129") | starts_with("p26130") | # Sauces
                                      starts_with("p26116") | starts_with("p26135") | # Mixed dishes
                                      starts_with("p26139") | starts_with("p26134")
      ), na.rm = TRUE) / p20077,

      hpdi = (rowSums(select(., starts_with("p26071") | starts_with("p26074") | # Whole grains
                               starts_with("p26075") | starts_with("p26076") |
                               starts_with("p26077") | starts_with("p26078") |
                               starts_with("p26105") | starts_with("p26114") |

                               starts_with("p26089") | starts_with("p26090") | # Fruits
                               starts_with("p26091") | starts_with("p26092") |
                               starts_with("p26093") | starts_with("p26094") |

                               starts_with("p26106") | starts_with("p26107") | # Nuts
                               starts_with("p26108") |

                               starts_with("p26110") | starts_with("p26111") | # Plant oils
                               starts_with("p26112") |

                               starts_with("p26081") | starts_with("p26082") |  # Tea, coffee & water
                               starts_with("p26141") | starts_with("p26142") |
                               starts_with("p26148") |

                               starts_with("p26065") | starts_with("p26098") | # Vegetables
                               starts_with("p26123") | starts_with("p26125") |
                               starts_with("p26143") | starts_with("p26146") |
                               starts_with("p26147")), na.rm = TRUE) +
                rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + # assuming half hummus half guacamole
                rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE)) / p20077,

      updi = rowSums(select(., starts_with("p26068") | starts_with("p26072") | # Refined grains
                              starts_with("p26073") | starts_with("p26079") |
                              starts_with("p26083") | starts_with("p26113") |
                              starts_with("p26118") | starts_with("p26119") | # Potatoes
                              starts_with("p26120") |
                              starts_with("p26095") | # Fruit juice
                              starts_with("p26097") | starts_with("p26128") | # Mixed dishes, vegetarian
                              starts_with("p26145") |
                              starts_with("p26064") | starts_with("p26080") | # Sweets and dessert
                              starts_with("p26085") | starts_with("p26140") |
                              starts_with("p26124") | starts_with("p26126") | # sugar sweetened beverages
                              starts_with("p26127")), na.rm = TRUE) / p20077,
      alc_beverage_daily = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
                                            starts_with("p26153") | starts_with("p26067") |
                                            starts_with("p26138")), na.rm = TRUE) / p20077,
      alcohol_daily = rowSums(select(., starts_with("p26030")), na.rm = TRUE) / p20077
    )
  return(data)
}

food_intake_extra <- function(data) {
  data <- data %>%
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
        animal_foods + hpdi + updi + alc_beverage_daily,
      # total energy of all foods and beverages
      total_energy_food_daily = rowSums(select(., starts_with("p26002")), na.rm = TRUE) / p20077,
      # all other foods except legumesand meat
      other_foods_daily = animal_foods + hpdi + updi + alc_beverage_daily,
      other_foods_legume_daily = other_foods_daily + legume_other_daily
    )
  return(data)
}

legume_strat <- function(data) {
  data <- data %>%
    filter(legume_daily != 0) %>%
    mutate(
      legume_quintile = ntile(legume_daily, 4),
      legume_category = factor(legume_quintile, labels = c("Q1", "Q2", "Q3", "Q4"))
    ) %>%
    bind_rows(data %>%
                filter(legume_daily == 0) %>%
                mutate(legume_category = "No intake")) %>%
    mutate(legume_category = factor(legume_category, levels = c("No intake", "Q1", "Q2", "Q3", "Q4")))
  return(data)
}

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

baseline_age <- function(data) {
  # Creating age at baseline:
  data <- data %>%
    mutate(age_at_baseline = year(baseline_start_date) - year(date_birth) -
             ifelse(month(baseline_start_date) < month(date_birth) |
                      (month(baseline_start_date) == month(date_birth) &
                         day(baseline_start_date) < day(date_birth)), 1, 0)) |>
    filter(!(is.na(age_at_baseline))) # 3 participants who lack dates for completed questionnaire.
  return(data)
}

follow_up <- function(data) {
  # Creating age at loss to follow-up:
  data <- data %>%
    mutate(age_l2fu = as.numeric(difftime(l2fu_d, date_birth, units = "days")) / 365.25)
  # Removing participants who were lost to follow-up before baseline:
  data <- data %>%
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date)
  return(data)
}

end_of_follow_up <- function(data) {
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

remove_liver_before <- function(data) {
  data <- data %>%
    filter(
      cancer_hcc_date <= baseline_start_date |
        cancer_icc_date <= baseline_start_date |
        icd10_hcc_date <= baseline_start_date |
        icd10_icc_date <= baseline_start_date
    )
  return(data)
}
