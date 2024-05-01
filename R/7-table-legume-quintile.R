library(gt)
library(gtsummary)
library(tidyverse)
library(survival)

set_gtsummary_theme(theme_gtsummary_journal("jama"),
                    quiet = TRUE)

model1t_leg <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    animal_foods + hpdi + updi + total_weight_food_daily,
  data = data
)

m1t_leg <- model1t_leg %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Legume category",
  ) %>% bold_p(t = 0.05)

model2t_leg <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    animal_foods + hpdi + updi + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data
)

m2t_leg <- model2t_leg %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Legume category",
  ) %>% bold_p(t = 0.05)

table_legume <- tbl_merge(
  tbls = list(m1t_leg, m2t_leg)
) %>%
  modify_spanning_header(everything() ~ NA_character_) %>%
  modify_caption("**Supplementary table 3. No intake of legumes vs. quartiles of daily legume intake and hazard ratios and 95% confidence intervals for primary liver cancer.** (N = {N})") %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Legume category",
    footnote = "mean daily intake of legumes in grams for each quartile: Q1: 6.3, Q2: 15.7, Q3: 34.3, Q4 109."
  ) %>%
  modify_table_styling(
    column = c(p.value_1, p.value_2),
    hide = TRUE
  ) %>%
  as_gt() %>%
  tab_spanner(
    label = "Model 1",
    columns = c(estimate_1, ci_1, p.value_1)
  ) %>%
  tab_spanner(
    label = "Model 2",
    columns = c(estimate_2, ci_2, p.value_2)
  ) %>%
  tab_footnote(
    footnote = "Adjusted for age (as underlying timescale), other food groups, and total food intake.",
    locations = cells_column_spanners(spanners = "Model 1")
  ) %>%
  tab_footnote(
    footnote = "Further adjusted for sex, educational level, Townsend deprivation index, living alone, physical activity, smoking, alcohol intake, and waist circumference.",
    locations = cells_column_spanners(spanners = "Model 2")
  )
