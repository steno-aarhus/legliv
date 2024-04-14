library(survival)
library(gt)
library(gtsummary)
model1t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data
)

model2t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data
)

model1r <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data
)

model2r <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data
)

model1p <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data
)

model2p <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data
)


m1t <- model1t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m1r <- model1r %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m1p <- model1p %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

m2t <- model2t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r <- model2r %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p <- model2p %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

row1 <- tbl_merge(list(m1t, m2t)) %>%
  modify_spanning_header(everything() ~ NA_character_)
row2 <- tbl_merge(list(m1r, m2r))
row3 <- tbl_merge(list(m1p, m2p))

table_main <-
  tbl_stack(list(row1, row2, row3)) %>%
  modify_caption("**Table 3. Hazard ratios for substitution of legumes for total meat, red meat and processed meat.** (N = {N})") %>%
  modify_header(label = "**15 g/day substitution**") %>%
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
table_main
