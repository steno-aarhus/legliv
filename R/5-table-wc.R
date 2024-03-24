library(survival)
library(gtsummary)

model0t_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_wc_high
)

m0t_wc_high <- model0t_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  ) %>%
  modify_caption("**Stratified on waist circumference (102 cm for men, 88 cm for women)** (N = {N})")

model1t_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data_wc_high
)

m1t_wc_high <- model1t_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )



model2t_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_wc_high
)

m2t_wc_high <- model2t_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )


###############################################################################################################


model0r_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_wc_high
)

m0r_wc_high <- model0r_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )

model1r_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data_wc_high
)

m1r_wc_high <- model1r_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )



model2r_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_wc_high
)

m2r_wc_high <- model2r_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )



###############################################################################################################


model0p_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_wc_high
)

m0p_wc_high <- model0p_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )

model1p_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data_wc_high
)

m1p_wc_high <- model1p_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )



model2p_wc_high <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_wc_high
)

m2p_wc_high <- model2p_wc_high %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "High WC",
  )

model0t_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_wc_low
)

m0t_wc_low <- model0t_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )

model1t_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data_wc_low
)

m1t_wc_low <- model1t_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )



model2t_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_wc_low
)

m2t_wc_low <- model2t_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )


###############################################################################################################


model0r_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_wc_low
)

m0r_wc_low <- model0r_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )

model1r_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data_wc_low
)

m1r_wc_low <- model1r_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )



model2r_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_wc_low
)

m2r_wc_low <- model2r_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )



###############################################################################################################


model0p_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
  data = data_wc_low
)

m0p_wc_low <- model0p_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )

model1p_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex,
  data = data_wc_low
)

m1p_wc_low <- model1p_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )



model2p_wc_low <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily +
    whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
    egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
    fruit_daily + nut_daily + meat_sub_daily + snack_daily +
    mixed_dish_daily + sauce_daily + fats_daily +
    non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + ethnicity + spouse +
    bmi_category + exercise + smoking + alcohol_daily +
    diabetes + cholelith + nafld + cystectomy,
  data = data_wc_low
)

m2p_wc_low <- model2p_wc_low %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Low WC",
  )

row1 <- tbl_merge(list(m0t_wc_high, m0r_wc_high, m0p_wc_high), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"))
row2 <- tbl_merge(list(m1t_wc_high, m1r_wc_high, m1p_wc_high))
row3 <- tbl_merge(list(m2t_wc_high, m2r_wc_high, m2p_wc_high))
row4 <- tbl_merge(list(m0t_wc_low, m0r_wc_low, m0p_wc_low))
row5 <- tbl_merge(list(m1t_wc_low, m1r_wc_low, m1p_wc_low))
row6 <- tbl_merge(list(m2t_wc_low, m2r_wc_low, m2p_wc_low))

table_wc <-
  tbl_stack(list(row1, row4, row2, row5, row3, row6), group_header = c("Model 0", "Model 0", "Model 1", "Model 1", "Model 2", "Model 2")) %>%
  modify_header(label = "**15 g/day substitution**")

table_wc
