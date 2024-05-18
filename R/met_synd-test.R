data <- data %>%
  mutate(
    p93_i0 = rowMeans(pick(matches("p93_i0_")), na.rm = TRUE),
    p94_i0 = rowMeans(pick(matches("p94_i0_")), na.rm = TRUE),
    p4079_i0 = rowMeans(pick(matches("p4079_i0_")), na.rm = TRUE),
    p4080_i0 = rowMeans(pick(matches("p4080_i0_")), na.rm = TRUE),
    high_dia = if_else(p94_i0 >= 85 | p4079_i0 >= 85, "Yes", "No"),
    high_sys = if_else(p93_i0 >= 130 | p4080_i0 >= 130, "Yes", "No"),
    high_bp = if_else(high_dia == "Yes" & high_sys == "Yes", "Yes", "No")
  )

data %>% select(matches("p93|p94|p4079|p4080|high_")) %>% summary()
data %>% group_by(high_dia) %>% summarise(n())
data <- targets::tar_read(data_main)
data <- data %>%
  met_synd = case_when(
    rowSums(is.na(select(., c("high_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs", "high_bp")))) > 0 ~ NA_character_,
    rowSums(select(., c("high_bmi_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs", "high_bp")) == "Yes", na.rm = TRUE) >= 3 ~ "Yes",
    TRUE ~ "No"
  )


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

    met_synd = case_when(
      rowSums(is.na(select(., c("high_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs")))) > 0 ~ NA_character_,
      rowSums(select(., c("high_bmi_wc", "high_trigly", "bp_med", "low_hdl_chol_med", "high_bs")) == "Yes", na.rm = TRUE) >= 3 ~ "Yes",
      TRUE ~ "No"
    )
  )
