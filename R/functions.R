
# Data with ID ------------------------------------------------------------

data_id <- function(data) {
  data <- data %>%
    dplyr::mutate(id = dplyr::row_number())
  return(data)
}

# Prepare data ------------------------------------------------------------

other_variables <- function(data) {
  data <- data %>%
    mutate(
      birth_year = p34,
      birth_month = p52,
      l2fu_d = p191,
      age_recruit = p21022,
      p40000_d0 = p40000_i0,
      p40001_d0 = p40001_i0,
      age_dead = p40007_i0
    )
  data <- data %>%
    mutate(across(starts_with("p100020_i"), ~ coalesce(., "Yes"))) %>%
    mutate(
      typical_diet = if_else(p100020_i0 == "No" | p100020_i1 == "No" | p100020_i2 == "No" | p100020_i3 == "No" | p100020_i4 == "No", "No", "Yes")
    )
  return(data)
}

two_ques_only <- function(data) {
  # Removing participants who did not complete 2 or more diet questionnaires
  data <- data %>%
    filter(p20077 >= 2)
  return(data)
}

remove_timestamp <- function(data) { # Removing specific time stamp from date of completed questionnaires:
  data <- data %>%
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
  baseline_start_date <- data %>%
    select(p20077, starts_with("p105010_i"), id) %>%
    pivot_longer(
      cols = starts_with("p105010_i"),
      names_to = "instance",
      values_to = "completion_date"
    ) %>%
    filter(!is.na(completion_date)) %>%
    group_by(id) %>%
    arrange(completion_date, .by_group = TRUE) %>%
    slice_tail() %>%
    rename(baseline_start_date = completion_date) %>%
    ungroup() %>%
    select(id, baseline_start_date)
  data <- data %>%
    left_join(baseline_start_date, by = "id")
  return(data)
}

birth_date <- function(data) {
  # Merging birth year and month of birth into one column:
  month_names <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  data <- data %>%
    mutate(birth_month_num = sprintf("%02d", match(birth_month, month_names)))
  data <- data %>%
    unite(birth_date, birth_year, birth_month_num, sep = "-")
  # adding 15 as DD for all participants:
  data$birth_date <- as.Date(paste0(data$birth_date, "-15"))
  return(data)
}

baseline_age <- function(data) {
  # Creating age at baseline:
  data <- data %>%
    mutate(age_at_baseline = year(baseline_start_date) - year(birth_date) -
             ifelse(month(baseline_start_date) < month(birth_date) |
                      (month(baseline_start_date) == month(birth_date) &
                         day(baseline_start_date) < day(birth_date)), 1, 0))
  return(data)
}

remove_ques_error <- function(data) { # 3 participants who lack dates for completed questionnaire.
  data <- data %>%
    filter(
      !rowSums(!is.na(pick(matches("p105010")))) <= 1
    )
  return(data)
}

# Covariates and diet ------------------------------------------------------

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
      exercise = as.factor(p22035_i0),
      exercise = case_when(
        exercise == "Yes" ~ "Above",
        exercise == "No" ~ "Below",
        is.na(exercise) ~ "Missing"
      ),
      exercise = factor(exercise, levels = c("Above", "Below", "Missing")),
      age_strat = case_when(
        p21022 < 45 ~ 1,
        p21022 >= 45 & p21022 < 50 ~ 2,
        p21022 >= 50 & p21022 < 55 ~ 3,
        p21022 >= 55 & p21022 < 60 ~ 4,
        p21022 >= 60 & p21022 < 65 ~ 5,
        p21022 >= 65 ~ 6
      ),
      alat = p30620_i0,
      asat = p30650_i0
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
      glucose = p30750_i0,
      high_trigly = if_else(trigly >= 1.7, "Yes", "No"),
      low_hdl = if_else(hdl <= 1.036 & sex == "Male" | hdl <= 1.295 & sex == "Female", "Yes", "No"),
      chol_med_men = ifelse(grepl("Cholesterol lowering medication", med_men), "Yes", "No"),
      chol_med_women = ifelse(grepl("Cholesterol lowering medication", med_women), "Yes", "No"),
      chol_med = if_else(chol_med_men == "Yes" | chol_med_women == "Yes", "Yes", "No"),
      low_hdl_chol_med = if_else(low_hdl == "No" & chol_med == "No", "No", "Yes"),
      bp_med_men = ifelse(grepl("Blood pressure medication", med_men), "Yes", "No"),
      bp_med_women = ifelse(grepl("Blood pressure medication", med_women), "Yes", "No"),
      bp_med = if_else(bp_med_men == "Yes" | bp_med_women == "Yes", "Yes", "No"),
      high_wc = if_else(sex == "Male" & wc >= 94 | sex == "Female" & wc >= 80, "Yes", "No"),
      high_bmi = if_else(bmi >= 30, "Yes", "No"),
      high_bmi_wc = if_else(high_wc == "No" & high_bmi == "No", "No", "Yes"),
      bs_high = if_else(glucose >= 39, "Yes", "No"),
      ins_med_men = ifelse(grepl("Insulin", med_men), "Yes", "No"),
      ins_med_women = ifelse(grepl("Insulin", med_women), "Yes", "No"),
      ins_med = if_else(ins_med_men == "Yes" | ins_med_women == "Yes", "Yes", "No"),
      high_bs = if_else(bs_high == "No" & ins_med == "No", "No", "Yes"),
      p93_i0 = rowMeans(pick(matches("p93_i0_")), na.rm = TRUE),
      p94_i0 = rowMeans(pick(matches("p94_i0_")), na.rm = TRUE),
      p4079_i0 = rowMeans(pick(matches("p4079_i0_")), na.rm = TRUE),
      p4080_i0 = rowMeans(pick(matches("p4080_i0_")), na.rm = TRUE),
      high_dia = if_else(p94_i0 >= 85 | p4079_i0 >= 85, "Yes", "No"),
      high_sys = if_else(p93_i0 >= 130 | p4080_i0 >= 130, "Yes", "No"),
      high_bp = if_else(high_dia == "Yes" & high_sys == "Yes", "Yes", "No")
      # met_synd = case_when(
      #   rowSums(is.na(select(., c("high_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs", "high_bp")))) > 0 ~ NA_character_,
      #   rowSums(select(., c("high_bmi_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs", "high_bp")) == "Yes", na.rm = TRUE) >= 3 ~ "Yes",
      #   TRUE ~ "No"
      # )
    )
}

# Diet --------------------------------------------------------------------

# TODO: Redo this to mimic what was done in Fie's repository.
calculate_food_intake <- function(data) {
  data <- data |>
    mutate(
      hummus_guac = rowSums(pick(matches("p26144")), na.rm = TRUE) / 2,
      peas_corn = rowSums(pick(matches("p26115")), na.rm = TRUE) / 2,
      legume_daily = rowSums(pick(matches("p26086|p26101|p26136|p26137|hummus_guac|peas_corn")), na.rm = TRUE) / p20077,
      legume_daily_15 = legume_daily / 15,
      red_meat_daily = rowSums(pick(matches("p26066|p26100|p26104|p26117")), na.rm = TRUE) / p20077,
      red_meat_daily_15 = red_meat_daily / 15,
      proc_meat_daily = rowSums(pick(matches("p26122")), na.rm = TRUE) / p20077,
      proc_meat_daily_15 = proc_meat_daily / 15,
      animal_foods = rowSums(pick(matches(
        "p26069|p26121|p26070|p26109|p26132|p26149|p26062|p26063|p26084|p26087|p26096|p26099|p26102|p26103|p26131|p26133|p26150|p26154|p26088|p26129|p26130|p26116|p26135|p26139|p26134"
      )), na.rm = TRUE) / p20077,
      hpdi = rowSums(pick(matches(
        "p26071|p26074|p26075|p26076|p26077|p26078|p26105|p26114|p26089|p26090|p26091|p26092|p26093|p26094|p26106|p26107|p26108|p26110|p26111|p26112|p26081|p26082|p26141|p26142|p26148|p26065|p26098|p26123|p26125|p26143|p26146|p26147|hummus_guac|peas_corn"
      )), na.rm = TRUE) / p20077,
      updi = rowSums(pick(matches(
        "p26068|p26072|p26073|p26079|p26083|p26113|p26118|p26119|p26120|p26095|p26097|p26128|p26145|p26064|p26080|p26085|p26140|p26124|p26126|p26127"
      )), na.rm = TRUE) / p20077,
      alc_beverage_daily = rowSums(pick(matches(
        "p26067|p26138|p26151|p26152|p26153"
      )), na.rm = TRUE) / p20077,
      alcohol_daily = rowSums(pick(matches("p26030")), na.rm = TRUE) / p20077,
      total_food = rowSums(pick(matches("p26000")), na.rm = TRUE) / p20077,
      total_weight_food_daily = legume_daily + red_meat_daily + proc_meat_daily +
        animal_foods + hpdi + updi + alc_beverage_daily
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

# Split ICD and OPCS columns ---------------------------------------------------------------

split_column <- function(data) {
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
      names = paste0("p41271var_b", 0:46), too_few = "debug"
    ) %>%
    rename_with(~paste0("p41281_b", gsub("p41281_a", "", .x)), starts_with("p41281_a"))
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

# ICD and Cancer long format -------------------------------------------------

icd_longer_subset <- function(data) {
  data %>%
    select(matches("p41270var|p41280|p41271var|p41281|id")) %>%
    pivot_longer(
      cols = matches("_a|_b"),
      names_to = c(".value", "a"),
      names_sep = "_"
    ) %>%
    filter(!is.na(p41270var) | !is.na(p41271var)) %>%
    pivot_longer(
      cols = matches("p41280|p41281"),
      names_to = "date_name",
      values_to = "date"
    ) %>%
    filter(!is.na(date))
}

cancer_longer_subset <- function(data) {
  data %>%
    select(matches("id|p41270var|p41280|p41271var|p41281|p40006|p40005|p40013")) %>%
    pivot_longer(
      cols = matches("_a|_b|_i"),
      names_to = c(".value", "number"),
      names_sep = "_"
    ) %>%
    filter(!is.na(p41270var) | !is.na(p41271var) | !is.na(p40006) | !is.na(p40013)) %>%
    pivot_longer(
      cols = matches("p41280|p41281|p40005"),
      names_to = "date_name",
      values_to = "date"
    ) %>%
    filter(!is.na(date))
}

cancer_longer_subset_death <- function(data) {
  data %>%
    select(matches("id|p41270var|p41280|p41271var|p41281|p40006|p40005|p40013|p40001_d0|p40000_d0|p40002")) %>%
    unite(p40002_d0, starts_with("p40002_i0_a"), sep = "|", na.rm = TRUE) %>%
    mutate(p40002_d0 = if_else(p40002_d0 == "", NA, p40002_d0)) %>%
    pivot_longer(
      cols = matches("_a|_b|_i|_d"),
      names_to = c(".value", "number"),
      names_sep = "_"
    ) %>%
    filter(!is.na(p41270var) | !is.na(p41271var) | !is.na(p40006) | !is.na(p40013) | !is.na(p40001) | !is.na(p40002)) %>%
    pivot_longer(
      cols = matches("p41280|p41281|p40005|p40000"),
      names_to = "date_name",
      values_to = "date"
    ) %>%
    filter(!is.na(date))
}

liver_cancer_main <- function(data){
  data %>%
    filter(str_detect(p41270var, "C22.0|C22.1")|str_detect(p41271var, "^155[0-9]")|str_detect(p40006, "C22.0|C22.1")|str_detect(p40013, "^155[0-9]")) %>%
    group_by(id) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    rename(liver_cancer_date = date) %>%
    select(id, liver_cancer_date)
}

liver_cancer_hcc <- function(data) {
  data %>%
    filter(str_detect(p41270var, "C22.0")|str_detect(p41271var, "^155[0-9]")|str_detect(p40006, "C22.0")|str_detect(p40013, "^155[0-9]")) %>%
    group_by(id) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    rename(hcc_date = date) %>%
    select(id, hcc_date)
}

liver_cancer_icc <- function(data) {
  data %>%
    filter(str_detect(p41270var, "C22.1")|str_detect(p41271var, "^155[0-9]")|str_detect(p40006, "C22.1")|str_detect(p40013, "^155[0-9]")) %>%
    filter(!is.na(date)) %>%
    group_by(id) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    rename(icc_date = date) %>%
    select(id, icc_date)
}

liver_cancer_main_death <- function(data){
  data %>%
    filter(str_detect(p41270var, "C22.0|C22.1")
           |str_detect(p41271var, "^155[0-9]")
           |str_detect(p40006, "C22.0|C22.1")
           |str_detect(p40013, "^155[0-9]")
           |str_detect(p40001, "C22.0|C22.1")
           |str_detect(p40002, "C22.0|C22.1")
           ) %>%
    group_by(id) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    rename(liver_cancer_date_death = date) %>%
    select(id, liver_cancer_date_death)
}

# Remove before baseline --------------------------------------------------

remove_before_baseline_main <- function(data) {
  data <- data %>%
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date) %>%
    filter(is.na(liver_cancer_date) | liver_cancer_date >= baseline_start_date) %>%
    filter(is.na(p40000_i0) | p40000_i0 >= baseline_start_date) %>%
    mutate(
      cens_date = if_else(is.na(liver_cancer_date) & is.na(p40000_i0) & is.na(l2fu_d), as.Date("2022-10-31"), NA),
      p40000_i0 = if_else(p40000_i0 >= as.Date("2022-10-31"), as.Date("2022-10-31"), p40000_i0)
      )
  return(data)
}

remove_before_baseline_hcc <- function(data) {
  data <- data %>%
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date) %>%
    filter(is.na(hcc_date) | hcc_date >= baseline_start_date) %>%
    filter(is.na(p40000_i0) | p40000_i0 >= baseline_start_date) %>%
    mutate(cens_date = if_else(is.na(hcc_date) & is.na(p40000_i0) & is.na(l2fu_d), as.Date("2022-10-31"), NA))
  return(data)
}

remove_before_baseline_icc <- function(data) {
  data <- data %>%
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date) %>%
    filter(is.na(icc_date) | icc_date >= baseline_start_date) %>%
    filter(is.na(p40000_i0) | p40000_i0 >= baseline_start_date) %>%
    mutate(cens_date = if_else(is.na(icc_date) & is.na(p40000_i0) & is.na(l2fu_d), as.Date("2022-10-31"), NA))
  return(data)
}

remove_before_baseline_death <- function(data) {
  data <- data %>%
    filter(is.na(l2fu_d) | l2fu_d >= baseline_start_date) %>%
    filter(is.na(liver_cancer_date_death) | liver_cancer_date_death >= baseline_start_date) %>%
    filter(is.na(p40000_i0) | p40000_i0 >= baseline_start_date) %>%
    mutate(cens_date = if_else(is.na(liver_cancer_date_death) & is.na(p40000_i0) & is.na(l2fu_d), as.Date("2022-10-31"), NA))
  return(data)
}

# Define end of follow-up -------------------------------------------------

end_of_follow_up_main <- function(data) {
  # Creating status, status date and status age:
  data <- data %>%
    select(id, liver_cancer_date, p40000_i0, l2fu_d, cens_date) %>%
    pivot_longer(
      cols = matches("liver_cancer_date|p40000_i0|l2fu_d|cens_date"),
      names_to = "status",
      values_to = "status_date"
    ) %>%
    filter(!is.na(status_date)) %>%
    group_by(id) %>%
    arrange(status_date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(status = case_when(
      status == "liver_cancer_date" ~ "Liver cancer",
      status == "p40000_i0" ~ "Censored",
      status == "l2fu_d" ~ "Censored",
      status == "cens_date" ~ "Censored"
    ),
    status_date = if_else(status_date >= as.Date("2022-10-31"), as.Date("2022-10-31"), status_date)
    ) %>%
    select(id, status, status_date)
  return(data)
}

end_of_follow_up_hcc <- function(data) {
  # Creating status, status date and status age:
  data <- data %>%
    select(id, hcc_date, p40000_i0, l2fu_d, cens_date) %>%
    pivot_longer(
      cols = matches("hcc_date|p40000_i0|l2fu_d|cens_date"),
      names_to = "status",
      values_to = "status_date"
    ) %>%
    filter(!is.na(status_date)) %>%
    group_by(id) %>%
    arrange(status_date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(status = case_when(
      status == "hcc_date" ~ "Liver cancer",
      status == "p40000_i0" ~ "Censored",
      status == "l2fu_d" ~ "Censored",
      status == "cens_date" ~ "Censored"
    )) %>%
    select(id, status, status_date)
  return(data)
}

end_of_follow_up_icc <- function(data) {
  # Creating status, status date and status age:
  data <- data %>%
    select(id, icc_date, p40000_i0, l2fu_d, cens_date) %>%
    pivot_longer(
      cols = matches("icc_date|p40000_i0|l2fu_d|cens_date"),
      names_to = "status",
      values_to = "status_date"
    ) %>%
    filter(!is.na(status_date)) %>%
    group_by(id) %>%
    arrange(status_date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(status = case_when(
      status == "icc_date" ~ "Liver cancer",
      status == "p40000_i0" ~ "Censored",
      status == "l2fu_d" ~ "Censored",
      status == "cens_date" ~ "Censored"
    )) %>%
    select(id, status, status_date)
  return(data)
}

end_of_follow_up_death <- function(data) {
  # Creating status, status date and status age:
  data <- data %>%
    select(id, liver_cancer_date_death, p40000_i0, l2fu_d, cens_date) %>%
    pivot_longer(
      cols = matches("liver_cancer_date_death|p40000_i0|l2fu_d|cens_date"),
      names_to = "status",
      values_to = "status_date"
    ) %>%
    filter(!is.na(status_date)) %>%
    group_by(id) %>%
    arrange(status_date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(status = case_when(
      status == "liver_cancer_date_death" ~ "Liver cancer",
      status == "p40000_i0" ~ "Censored",
      status == "l2fu_d" ~ "Censored",
      status == "cens_date" ~ "Censored"
    )) %>%
    select(id, status, status_date)
  return(data)
}

make_status_age <- function(data) {
  data <- data %>%
    mutate(
      status_age = as.numeric(difftime(status_date, birth_date, units = "days")) / 365.25, # Calculating age in years
      study_time = status_age - age_at_baseline
    )
  return(data)
}

# Prepare sensitivity analyses --------------------------------------------

remove_high_alcohol <- function(data) {
  data <- data %>%
    filter(
      sex == "Male" & alcohol_daily < 32 | sex == "Female" & alcohol_daily < 24
    )
  return(data)
}

remove_misreporter <- function(data) {
  data <- data %>%
    filter(
      sex == "Male" & total_energy_food_daily > 3200 & total_energy_food_daily < 16800 |
        sex == "Female" & total_energy_food_daily > 2000 & total_energy_food_daily < 14000
    )
  return(data)
}

filter_ques_comp <- function(data) {
  data <- data %>%
    filter(p20077 >= 3)
  return(data)
}

reduce_full_data <- function(data) {
  data <- data %>%
    select(id, p20077, p191)
  return(data)
}

reduce_baseline_data <- function(data) {
  data <- data %>%
    select(id, liver_cancer_date, baseline_start_date)
  return(data)
}

icd_liver_disease <- function(data) {
  data %>%
    filter(str_detect(p41270var, "^K7[0-9]|^B1[6-9]|^Z94.4|^I82.0|^I85|^I86.4|^E83.[0-1]|^E88.0") | str_detect(p41271var, "^57[1-4]|^070|^V427|^275[0-1]")) %>%
    group_by(id) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(liver_disease_date = date) %>%
    select(id, liver_disease_date)
}

remove_liver_disease_before <- function(data) {
  data %>%
    mutate(liver_disease = if_else(liver_disease_date >= baseline_start_date | is.na(liver_disease_date), "No", "Yes")) %>%
    filter(liver_disease == "No")
}

icd_any_cancer <- function(data) {
  data %>%
    filter(str_detect(p41270var, "^C[0-9]{2}|^D([0-3][0-9]|4[0-8])") |
             str_detect(p41271var, "^1[4-9]|^2[0-3][0-9]") |
             str_detect(p40006, "^C[0-9]{2}|^D([0-3][0-9]|4[0-8])") |
             str_detect(p40013, "^1[4-9]|^2[0-3][0-9]")
    ) %>%
    group_by(id) %>%
    arrange(date, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup() %>%
    mutate(cancer_before_date = date) %>%
    select(id, cancer_before_date)
}

remove_any_cancer_before <- function(data) {
  data %>%
    mutate(cancer_before = if_else(cancer_before_date >= baseline_start_date | is.na(cancer_before_date), "No", "Yes")) %>%
    filter(cancer_before == "No")
}

reduce_dataset <- function(data) {
  data %>%
    select(-matches("^p[0-9]"), all_of("p20077"))
}

remove_high_alat <- function(data) { # Have to redefine cutoff
  data <- data %>%
    filter(sex == "Male" & alat < 30 | sex == "Female" & alat < 20 | is.na(alat))
}

energy_outlier <- function(data) {
  data <- data %>%
    group_by(sex) %>%
    filter(total_energy_food_daily < quantile(total_energy_food_daily, 0.9) & total_energy_food_daily > quantile(total_energy_food_daily, 0.1)) %>%
    ungroup()
}

high_alcohol <- function(data) {
  data <- data %>%
    group_by(sex) %>%
    filter(alcohol_daily < quantile(alcohol_daily, 0.9)) %>%
    ungroup()
}
