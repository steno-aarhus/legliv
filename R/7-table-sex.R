library(survival)
library(gtsummary)

model0t_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_men
)

m0t_men <- model0t_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  ) %>%
  modify_caption("**Stratified on sex** (N = {N})")

model1t_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_men
)

m1t_men <- model1t_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )



model2t_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    +
      education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_men
)

m2t_men <- model2t_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )


###############################################################################################################


model0r_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_men
)

m0r_men <- model0r_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )

model1r_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_men
)

m1r_men <- model1r_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )



model2r_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    +
      education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_men
)

m2r_men <- model2r_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )



###############################################################################################################


model0p_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_men
)

m0p_men <- model0p_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )

model1p_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_men
)

m1p_men <- model1p_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )



model2p_men <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    +
      education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_men
)

m2p_men <- model2p_men %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Men",
  )

model0t_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_women
)

m0t_women <- model0t_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )

model1t_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_women
)

m1t_women <- model1t_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )



model2t_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    +
      education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_women
)

m2t_women <- model2t_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )


###############################################################################################################


model0r_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_women
)

m0r_women <- model0r_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )

model1r_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_women
)

m1r_women <- model1r_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )



model2r_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    +
      education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_women
)

m2r_women <- model2r_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )



###############################################################################################################


model0p_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_women
)

m0p_women <- model0p_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )

model1p_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_women
)

m1p_women <- model1p_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )



model2p_women <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    +
      education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + wc + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_women
)

m2p_women <- model2p_women %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Women",
  )

row1 <- tbl_merge(list(m0t_men, m0r_men, m0p_men), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"))
row2 <- tbl_merge(list(m1t_men, m1r_men, m1p_men))
row3 <- tbl_merge(list(m2t_men, m2r_men, m2p_men))
row4 <- tbl_merge(list(m0t_women, m0r_women, m0p_women))
row5 <- tbl_merge(list(m1t_women, m1r_women, m1p_women))
row6 <- tbl_merge(list(m2t_women, m2r_women, m2p_women))

table_wc <-
  tbl_stack(list(row1, row4, row2, row5, row3, row6), group_header = c("Model 0", "Model 0", "Model 1", "Model 1", "Model 2", "Model 2")) %>%
  modify_header(label = "**15 g/day substitution**")

table_wc
