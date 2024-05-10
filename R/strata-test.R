data <- targets::tar_read(data_main)
library(gt)
library(gtsummary)
library(tidyverse)
library(survival)

set_gtsummary_theme(theme_gtsummary_journal("jama"),
                    quiet = TRUE)
model1t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data
)

model1t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    age_at_baseline,
  data = data
)

m1t <- model1t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

model1t_test <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(age_strat, p54_i0),
  data = data
)

m1t_test <- model1t_test %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )


tbl_stack(list(m1t,m1t_test))


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

m2t <- model2t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

model2t_test <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(age_strat) + strata(p54_i0) +
    strata(sex) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data
)

m2t_test <- model2t_test %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

tbl_stack(list(m2t, m2t_test))
