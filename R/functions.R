data_id <- function(data) {
  data <- data |>
    dplyr::mutate(id = dplyr::row_number())
  return(data)
}

# Covariates and diet ------------------------------------------------------

covariates <- function(data) {
  data <- data |>
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
      exercise = as.factor(p22035_i0),
      exercise = case_when(
        exercise == "Yes" ~ "Above",
        exercise == "No" ~ "Below",
        is.na(exercise) ~ "Missing"
      ),
      exercise = factor(exercise, levels = c("Above", "Below", "Missing"))
    )
  return(data)
}

metabolic_syndrome <- function(data) {
  data <- data |>
    mutate(
      trigly = p30870_i0,
      hdl = p30760_i0,
      med_men = p6177_i0,
      med_women = p6153_i0,
      glucose = p30750_i0
    )
}

other_variables <- function(data) {
  data <- data |>
    mutate(
      birth_year = p34,
      month_of_birth = p52,
      l2fu_d = p191,
      age_recruit = p21022,
      dead_date = p40000_i0,
      dead_cause = p40001_i0,
      age_dead = p40007_i0
    )
  data <- data |>
    mutate(across(starts_with("p100020_i"), ~ coalesce(., "Yes"))) |>
    mutate(
      typical_diet = if_else(p100020_i0 == "No" | p100020_i1 == "No" | p100020_i2 == "No" | p100020_i3 == "No" | p100020_i4 == "No", "No", "Yes")
    )
  return(data)
}

# TODO: Redo this to mimic what was done in Fie's repository.
calculate_food_intake <- function(data) {
  # estimating average daily and weekly intakes of food groups in g
  data <- data |>
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
  data <- data |>
    filter(legume_daily != 0) |>
    mutate(
      legume_quintile = ntile(legume_daily, 4),
      legume_category = factor(legume_quintile, labels = c("Q1", "Q2", "Q3", "Q4"))
    ) |>
    bind_rows(data |>
                filter(legume_daily == 0) |>
                mutate(legume_category = "No intake")) |>
    mutate(legume_category = factor(legume_category, levels = c("No intake", "Q1", "Q2", "Q3", "Q4")))
  return(data)
}

# Prepare data ------------------------------------------------------------

two_ques_only <- function(data) {
  # Removing participants who did not complete 2 or more diet questionnaires
  data <- data |>
    filter(p20077 >= 2)
  return(data)
}

remove_timestamp <- function(data) { # Removing specific time stamp from date of completed questionnaires:
  data <- data |>
    mutate(across(
      c(
        p105010_i0,
        p105010_i1,
        p105010_i2,
        p105010_i3,
        p105010_i4
      ), ~ substr(.x, 1, 10)
    ))
  return(data)
}

baseline_date <- function(data) {
baseline_start_date <- data |>
  select(p20077, starts_with("p105010_i"), id) |>
  pivot_longer(
    cols = starts_with("p105010_i"),
    names_to = "instance",
    values_to = "completion_date"
  ) |>
  filter(!is.na(completion_date)) |>
  filter(id != 287216) |> # error in data, p20077 > 2 but only 1 completion date
  group_by(id) |>
  arrange(completion_date, .by_group = TRUE) |>
  slice_tail() |>
  rename(baseline_start_date = completion_date) |>
  ungroup() |>
  select(id, baseline_start_date)
data <- data |>
    left_join(baseline_start_date, by = "id")
  return(data)
}

birth_date <- function(data) {
  # Merging birth year and month of birth into one column:
  month_names <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  data <- data |>
    mutate(month_of_birth_num = sprintf("%02d", match(month_of_birth, month_names)))
  data <- data |>
    unite(date_birth, birth_year, month_of_birth_num, sep = "-")
  # adding 15 as DD for all participants:
  data$date_birth <- as.Date(paste0(data$date_birth, "-15"))
  return(data)
}

baseline_age <- function(data) {
  # Creating age at baseline:
  data <- data |>
    mutate(age_at_baseline = year(baseline_start_date) - year(date_birth) -
             ifelse(month(baseline_start_date) < month(date_birth) |
                      (month(baseline_start_date) == month(date_birth) &
                         day(baseline_start_date) < day(date_birth)), 1, 0)) |>
    filter(!(is.na(age_at_baseline))) # 3 participants who lack dates for completed questionnaire.
  return(data)
}

# Split ICD and OPCS columns ---------------------------------------------------------------

split_column <- function(data) {
  # Split the diagnosis-variable into separate columns based on delimiter "|" (ICD10 codes)
  data <- data |>
    separate_wider_delim(p41270,
      delim = "|",
      names = paste0("p41270var_a", 0:258), too_few = "debug"
    )
  data <- data |>
    select(starts_with("p41270"), starts_with("p41280")) |>
    select_if(~ !all(is.na(.))) |>
    bind_cols(data |> select(-starts_with("p41270"), -starts_with("p41280")))
  # Split the diagnosis-variable into separate columns based on delimiter "|" (ICD9 codes)
  data <- data |>
    separate_wider_delim(p41271,
      delim = "|",
      names = paste0("p41271var_a", 0:46), too_few = "debug"
    )
  # Split the diagnosis-variable into separate columns based on delimiter "|" (OPCS4 codes)
  data <- data |>
    separate_wider_delim(p41272,
      delim = "|",
      names = paste0("p41272var_a", 0:125), too_few = "debug"
    )
  # Split the diagnosis-variable into separate columns based on delimiter "|" (OPCS3 codes)
  data <- data |>
    separate_wider_delim(p41273,
      delim = "|",
      names = paste0("p41273var_a", 0:15), too_few = "debug"
    )
  return(data)
}

# Find liver cancer cases -------------------------------------------------

icd10_longer_subset <- function(data) {
  data |>
    select(matches("p41270|p41280|id")) |>
    pivot_longer(
      cols = matches("_a[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    )
}

cancer_longer_subset <- function(data) {
  data |>
    select(matches("p41270|p41280|p40006|p40005|id|baseline_start_date|p40001|p40000|p191")) |>
    pivot_longer(
      cols = matches("_a[0-9]*$|_i[0-9]*$"),
      names_to = c(".value", "a"),
      names_sep = "_"
    ) |>
    select(id, p41270var, p40006, p41280, p40005, baseline_start_date, p40001, p40000, p191) |>
    pivot_longer(
      cols = matches("p41280|p40005"),
      names_to = "cancer",
      values_to = "date"
    )
}

liver_cancer_main <- function(data){
  data |>
    filter(str_detect(p41270var, "C22.0|C22.1")|str_detect(p40006, "C22.0|C22.1")) |>
    filter(!is.na(date)) |>
    group_by(id) |>
    arrange(date, .by_group = TRUE) |>
    slice_head() |>
    ungroup() |>
    rename(liver_cancer_date = date) |>
    select(id, liver_cancer_date)
}

liver_cancer_hcc <- function(data) {
  data |>
    filter(str_detect(p41270var, "C22.0")|str_detect(p40006, "C22.0")) |>
    filter(!is.na(date)) |>
    group_by(id) |>
    arrange(date, .by_group = TRUE) |>
    slice_head() |>
    ungroup() |>
    rename(hcc_date = date) |>
    select(id, hcc_date)
}

liver_cancer_icc <- function(data) {
  data |>
    filter(str_detect(p41270var, "C22.1")|str_detect(p40006, "C22.1")) |>
    filter(!is.na(date)) |>
    group_by(id) |>
    arrange(date, .by_group = TRUE) |>
    slice_head() |>
    ungroup() |>
    rename(icc_date = date) |>
    select(id, icc_date)
}

remove_before_baseline_main <- function(data) {
  data <- data |>
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date) |>
    filter(is.na(liver_cancer_date) | liver_cancer_date >= baseline_start_date) |>
    filter(is.na(dead_date) | dead_date >= baseline_start_date) |>
    mutate(cens_date = if_else(is.na(liver_cancer_date) & is.na(dead_date) & is.na(l2fu_d), as.Date("2022-12-31"), NA))
  return(data)
}

end_of_follow_up_main <- function(data) {
  # Creating status, status date and status age:
  data <- data |>
    select(id, liver_cancer_date, dead_date, l2fu_d, cens_date) |>
    pivot_longer(
      cols = matches("liver_cancer_date|dead_date|l2fu_d|cens_date"),
      names_to = "status",
      values_to = "status_date"
    ) |>
    filter(!is.na(status_date)) |>
    group_by(id) |>
    arrange(status_date, .by_group = TRUE) |>
    slice_head() |>
    ungroup() |>
    mutate(status = case_when(
      status == "liver_cancer_date" ~ "Liver cancer",
      status == "dead_date" ~ "Censored",
      status == "l2fu_d" ~ "Censored",
      status == "cens_date" ~ "Censored"
    )) |>
    select(id, status, status_date)
  return(data)
}

make_status_age <- function(data) {
  data <- data |>
    mutate(
      status_age = as.numeric(difftime(status_date, date_birth, units = "days")) / 365.25, # Calculating age in years
      study_time = status_age - age_at_baseline
    )
  return(data)
}

remove_before_baseline_hcc <- function(data) {
  data <- data |>
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date) |>
    filter(is.na(hcc_date) | hcc_date >= baseline_start_date) |>
    filter(is.na(dead_date) | dead_date >= baseline_start_date) |>
    mutate(cens_date = if_else(is.na(hcc_date) & is.na(dead_date) & is.na(l2fu_d), as.Date("2022-12-31"), NA))
  return(data)
}

end_of_follow_up_hcc <- function(data) {
  # Creating status, status date and status age:
  data <- data |>
    select(id, hcc_date, dead_date, l2fu_d, cens_date) |>
    pivot_longer(
      cols = matches("hcc_date|dead_date|l2fu_d|cens_date"),
      names_to = "status",
      values_to = "status_date"
    ) |>
    filter(!is.na(status_date)) |>
    group_by(id) |>
    arrange(status_date, .by_group = TRUE) |>
    slice_head() |>
    ungroup() |>
    mutate(status = case_when(
      status == "hcc_date" ~ "Liver cancer",
      status == "dead_date" ~ "Censored",
      status == "l2fu_d" ~ "Censored",
      status == "cens_date" ~ "Censored"
    )) |>
    select(id, status, status_date)
  return(data)
}

remove_before_baseline_icc <- function(data) {
  data <- data |>
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date) |>
    filter(is.na(icc_date) | icc_date >= baseline_start_date) |>
    filter(is.na(dead_date) | dead_date >= baseline_start_date) |>
    mutate(cens_date = if_else(is.na(icc_date) & is.na(dead_date) & is.na(l2fu_d), as.Date("2022-12-31"), NA))
  return(data)
}

end_of_follow_up_icc <- function(data) {
  # Creating status, status date and status age:
  data <- data |>
    select(id, icc_date, dead_date, l2fu_d, cens_date) |>
    pivot_longer(
      cols = matches("icc_date|dead_date|l2fu_d|cens_date"),
      names_to = "status",
      values_to = "status_date"
    ) |>
    filter(!is.na(status_date)) |>
    group_by(id) |>
    arrange(status_date, .by_group = TRUE) |>
    slice_head() |>
    ungroup() |>
    mutate(status = case_when(
      status == "icc_date" ~ "Liver cancer",
      status == "dead_date" ~ "Censored",
      status == "l2fu_d" ~ "Censored",
      status == "cens_date" ~ "Censored"
    )) |>
    select(id, status, status_date)
  return(data)
}

remove_high_alcohol <- function(data) {
  data <- data |>
    filter(
      sex == "Male" & alcohol_daily < 32 | sex == "Female" & alcohol_daily < 24
    )
  return(data)
}

remove_misreporter <- function(data) {
  data <- data |>
    filter(
      sex == "Male" & total_energy_food_daily > 3200 & total_energy_food_daily < 16800 |
        sex == "Female" & total_energy_food_daily > 2000 & total_energy_food_daily < 14000
    )
  return(data)
}

filter_ques_comp <- function(data) {
  data <- data |>
    filter(p20077 >= 3)
  return(data)
}

reduce_full_data <- function(data) {
  data <- data |>
    select(id, p20077, p191)
  return(data)
}

reduce_baseline_data <- function(data) {
  data <- data |>
    select(id, liver_cancer_date, baseline_start_date)
  return(data)
}

icd10_liver_disease <- function(data) {
  data |>
    filter(str_detect(p41270var, "K7|B18")) |>
    group_by(id) |>
    arrange(p41280, .by_group = TRUE) |>
    slice_head() |>
    ungroup() |>
    mutate(liver_disease_date = p41280) |>
    select(id, liver_disease_date)
}

remove_liver_disease_before <- function(data) {
  data |>
    mutate(liver_disease = if_else(liver_disease_date >= baseline_start_date | is.na(liver_disease_date), "No", "Yes")) |>
    filter(liver_disease == "No")
}
