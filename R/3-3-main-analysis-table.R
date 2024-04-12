library(survival)
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
    label = legume_daily_15 ~ " ",
  ) %>%
  modify_caption("**Table 3. Hazard ratios for substitution of legumes for total meat, red meat and processed meat.** (N = {N})")

m1r <- model1r %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m1p <- model1p %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2t <- model2t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2r <- model2r %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2p <- model2p %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

row1 <- tbl_merge(list(m1t, m2t), tab_spanner = c("Model 1", "Model 2"))
row2 <- tbl_merge(list(m1r, m2r))
row3 <- tbl_merge(list(m1p, m2p))

table_main <-
  tbl_stack(list(row1, row2, row3),
            group_header = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"),
            quiet = TRUE) %>%
  modify_header(label = "**15 g/day substitution**")

table_main %>% as_gt()
