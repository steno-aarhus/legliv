create_table_one <- function(data, gt_theme) {
  gtsummary::set_gtsummary_theme(gt_theme)

  table_1_all <- data %>%
    select(age_at_baseline, sex, education, tdi, spouse, exercise, smoking, alcohol_daily, wc)

  table_1_cancer <- data %>%
    filter(status == "Liver cancer") %>%
    select(age_at_baseline, sex, education, tdi, spouse, exercise, smoking, alcohol_daily, wc)

  table_all <- table_1_all %>%
    tbl_summary(
      missing_text = "Missing",
      label = list(
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
    modify_footnote(
      all_stat_cols() ~ NA
    ) %>%
    modify_footnote(label ~ "Median (Q1, Q3) for continuous variables; n (%) for categorical variables. Variables are in bold.") %>%
    modify_table_styling(
      columns = label,
      rows = label == "Educational level",
      footnote = "High: College or University degree;
    Intermediate: A levels/AS levels, O levels/GCSEs, or equivalent;
    Low: none of the previously mentioned."
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
    tab_caption(
      md("**Baseline characteristics of UK Biobank participants who completed $\\geq$ 2 Oxford WebQ dietary recalls.**")
    )

  return(table_one)
}
