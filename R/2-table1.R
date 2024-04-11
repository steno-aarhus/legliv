library(gtsummary)

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
      age_at_baseline ~ "Age",
      sex ~ "Sex",
      education ~ "Educational level",
      tdi ~ "Townsend Deprivation Index",
      spouse ~ "Living alone",
      exercise ~ "Physically active",
      smoking ~ "Smoking",
      alcohol_daily ~ "Alcohol intake (g/day)",
      wc ~ "Waist circumference"
    )
  )

table_cancer <- table_1_cancer %>%
  tbl_summary(
    missing_text = "Missing",
    label = list(
      typical_diet ~ "Typical diet yesterday",
      age_at_baseline ~ "Age",
      sex ~ "Sex",
      education ~ "Educational level",
      tdi ~ "Townsend Deprivation Index",
      spouse ~ "Living alone",
      exercise ~ "Physically active",
      smoking ~ "Smoking",
      alcohol_daily ~ "Alcohol intake (g/day)",
      wc ~ "Waist circumference"
    )
  )

table_combined <- tbl_merge(
  tbls = list(table_all, table_cancer),
  tab_spanner = c("**Cohort**", "**Liver cancer**")
) %>%
  bold_labels() %>%
  modify_caption("**Table 1. Baseline characteristics of the UK Biobank participants who completed ≥ 2 Oxford WebQ 24-hour diet recall.**") %>%
  modify_header(label ~ "**Variable**")
table_combined %>% as_gt()
# table_combined %>%
#   as_gt() %>% # convert to gt table
#   gt::gtsave( # save table as image
#     filename = "table-1.png", path = "~/legliv/doc/Images"
#   )

