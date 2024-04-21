# library(gt)
# library(gtsummary)
# library(tidyverse)
# library(survival)

table_1_all <- data %>%
  select(typical_diet, age_at_baseline, sex, education, tdi, spouse, exercise, smoking, alcohol_daily, wc)

table_1_cancer <- data %>%
  filter(status == "Liver cancer") %>%
  select(typical_diet, age_at_baseline, sex, education, tdi, spouse, exercise, smoking, alcohol_daily, wc)

table_all <- table_1_all %>%
  tbl_summary(
    missing_text = "Missing",
    label = list(
      typical_diet ~ "Typical diet yesterday",
      age_at_baseline ~ "Age, years",
      sex ~ "Sex",
      education ~ "Educational level",
      tdi ~ "Townsend Deprivation Index",
      spouse ~ "Living alone",
      exercise ~ "Physical activity",
      smoking ~ "Smoking",
      alcohol_daily ~ "Alcohol intake, g/day",
      wc ~ "Waist circumference, cm"
    ),
    digits = tdi ~ 1
  )

table_cancer <- table_1_cancer %>%
  tbl_summary(
    missing_text = "Missing",
    label = list(
      typical_diet ~ "Typical diet yesterday",
      age_at_baseline ~ "Age, years",
      sex ~ "Sex",
      education ~ "Educational level",
      tdi ~ "Townsend Deprivation Index",
      spouse ~ "Living alone",
      exercise ~ "Physical activity",
      smoking ~ "Smoking",
      alcohol_daily ~ "Alcohol intake, g/day",
      wc ~ "Waist circumference, cm"
    ),
    digits = tdi ~ 1
  )

table_one <- tbl_merge(
  tbls = list(table_all, table_cancer),
  tab_spanner = c("**Cohort**", "**Liver cancer**")
) %>%
  bold_labels() %>%
  modify_caption("**Table 1. Baseline characteristics of UK Biobank participants who completed ≥ 2 Oxford WebQ 24-hour diet recall.**") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) for continous variables; n (%) for categorical variables"
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Typical diet yesterday",
    footnote = "Participants who reported eating a typical diet yesterday for all completed diet questionnaires."
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Educational level",
    footnote = "High: College or University degree;
    Intermediate: A levels/AS levels, O levels/GCSEs, or equivalent;
    Low: none of the previous mentioned."
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Physical activity",
    footnote = "Above or below the 2017 UK Physical activity guidelines of 150 minutes of moderate activity per week or 75 minutes of vigorous activity."
  )
table_one %>% as_gt()
