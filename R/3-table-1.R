table_1 <- data %>%
  select(age_at_baseline, sex, education, tdi, spouse, exercise, smoking, bmi_category, wc, ethnicity, diabetes, nafld, cholelith, cystectomy, status)

table_1 %>%
  tbl_summary(
    by = status,
    label = list(
      age_at_baseline ~ "Age",
      sex ~ "Sex",
      education ~ "Educational level",
      tdi ~ "Townsend Deprivation Index",
      exercise ~ "Physical activity",
      smoking ~ "Smoking",
      bmi_category ~ "Body mass index",
      wc ~ "Waist circumference",
      sex ~ "Sex",
      ethnicity ~ "Ethnicity",
      diabetes ~ "Type 2 diabetes",
      nafld ~ "NAFLD",
      cholelith ~ "Cholelithiasis",
      cystectomy ~ "Cholecystectomy"
    )
  ) %>%
  bold_labels() %>%
  add_p() %>%
  modify_caption("**Table 1. Baseline characteristics of the UK Biobank participants who completed ≥ 2 Oxford WebQ 24-hour diet recall.**") %>%
  modify_header(label ~ "**Variable**")
