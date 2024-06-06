library(gt)
library(gtsummary)
library(tidyverse)
library(survival)
source(here::here("R","gtsummary-theme.R"))
gtsummary::set_gtsummary_theme(my_theme)

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
    label = legume_daily_15 ~ "Total red meat",
  )

m1r <- model1r %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m1p <- model1p %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
  )

m2t <- model2t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Total red meat",
  )

m2r <- model2r %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p <- model2p %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
  )

row1 <- tbl_merge(list(m1t, m2t)) %>%
  modify_spanning_header(everything() ~ NA_character_)
row2 <- tbl_merge(list(m1r, m2r))
row3 <- tbl_merge(list(m1p, m2p))

table_main <-
  tbl_stack(list(row1, row2, row3)) %>%
  modify_header(label = "**15 g/day of legumes replacing:**") %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  as_gt() %>%
  tab_spanner(
    label = md("**Model 1**"),
    columns = c(estimate_1),
    id = "model1"
  ) %>%
  tab_spanner(
    label = md("**Model 2**"),
    columns = c(estimate_2),
    id = "model2"
  ) %>%
  tab_caption(
    md("**Replacing 15/day of total red meat, unprocessed red meat, and processed meat with legumes and hazard ratios and 95% confidence intervals for primary liver cancer.**")
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "dimgrey", align = "left", size = "medium"),
      cell_borders(sides = c("top","left","right"), style = "hidden")
    ),
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Multivariate Cox proportional hazards regression model adjusted for age (as underlying timescale), other food groups, and total food intake.",
    locations = cells_column_spanners(spanners = "model1")
  ) %>%
  tab_footnote(
    footnote = "Further adjusted for sex, educational level, Townsend deprivation index, living alone, physical activity, smoking, alcohol intake, and waist circumference.",
    locations = cells_column_spanners(spanners = "model2")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()
  )
