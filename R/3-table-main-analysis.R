library(survival)
library(gtsummary)

model1t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily +
    fruit_daily + nut_daily + snack_daily +
    mixed_dish_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data
)

model2t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily +
    fruit_daily + nut_daily + snack_daily +
    mixed_dish_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data
)

model1r <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily +
    fruit_daily + nut_daily + snack_daily +
    mixed_dish_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data
)

model2r <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily +
    fruit_daily + nut_daily + snack_daily +
    mixed_dish_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data
)

model1p <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily +
    fruit_daily + nut_daily + snack_daily +
    mixed_dish_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data
)

model2p <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily +
    fruit_daily + nut_daily + snack_daily +
    mixed_dish_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data
)


m1t <- model1t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  ) %>%
  modify_caption("**Participants who completed two or more  24-hour recall diet questionnaires** (N = {N})")

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

row1 <- tbl_merge(list(m1t, m1r, m1p), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"))
row2 <- tbl_merge(list(m2t, m2r, m2p))

tbl_stack <-
  tbl_stack(list(row1, row2), group_header = c("Model 1", "Model 2")) %>%
  modify_header(label = "**15 g/day substitution**")

tbl_stack %>%
  as_gt() %>% # convert to gt table
  gt::gtsave( # save table as image
    filename = "table-main-analysis.png", path = "~/legliv/doc/Images"
  )

