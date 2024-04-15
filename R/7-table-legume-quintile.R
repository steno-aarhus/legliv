model1t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    animal_foods + hpdi + updi + total_weight_food_daily,
  data = data
)

m1t <- model1t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Legume category",
  )

model2t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    animal_foods + hpdi + updi + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data
)

m2t <- model2t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Legume category",
  ) %>% bold_p(t = 0.05)

table_legume <- tbl_merge(
  tbls = list(m1t, m2t)
  ) %>%
  modify_spanning_header(everything() ~ NA_character_) %>%
  modify_caption("**Supplementary table 2. No intake of legumes vs. quartiles of daily legume intake** (N = {N})") %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  as_gt() %>%
  tab_spanner(
    label = "Crude",
    columns = c(estimate_1, ci_1, p.value_1)
  ) %>%
  tab_spanner(
    label = "Adjusted",
    columns = c(estimate_2, ci_2, p.value_2)
  ) %>%
  tab_footnote(
    footnote = "Minimally adjusted to fit the substitution model: adjusted for age (as underlying timescale), other food groups and total food intake.",
    locations = cells_column_spanners(spanners = "Crude")
  ) %>%
  tab_footnote(
    footnote = "Further adjusted for sex, educational level, Townsend Deprivation Index, living alone, physical activity, smoking, alcohol intake and waist circumference.",
    locations = cells_column_spanners(spanners = "Adjusted")
  )
table_legume

