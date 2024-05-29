library(gt)
library(gtsummary)
library(tidyverse)
source(here::here("R","gtsummary-theme.R"))
gtsummary::set_gtsummary_theme(my_theme)

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
  tbls = list(table_all, table_cancer)
) %>%
  bold_labels() %>%
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
  ) %>%
  modify_spanning_header(everything() ~ NA_character_) %>%
  as_gt() %>%
  tab_spanner(
    label = md("**Cohort**"),
    columns = c(stat_0_1),
    id = "cohort"
  ) %>%
  tab_spanner(
    label = md("**Liver cancer**"),
    columns = c(stat_0_2),
    id = "livercancer"
  ) %>%
  tab_header(
    title = md("**Table 1. Baseline characteristics of UK Biobank participants who completed ≥ 2 Oxford WebQ 24-hour diet recall.**")
  )

# table_one %>% gtsave("doc/latex-tables/table-baseline.tex")
