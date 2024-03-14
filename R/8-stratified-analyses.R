data_hcc <- data %>%
    filter_at(vars(starts_with("cancer_diag")), all_vars(. != "C22.1 Intrahepatic bile duct carcinoma" | is.na(.)))

data_icc <- data %>%
    filter_at(vars(starts_with("cancer_diag")), all_vars(. != "C22.0 Liver cell carcinoma" | is.na(.)))


# Model 1: leave one out
# minimally adjusted for age at recruitment, sex, total energy intake, and all other dietary components:
model1 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    fish_daily +
                    poultry_daily +
                    dairy_daily +
                    egg_daily +
                    cereal_refined_daily +
                    whole_grain_daily +
                    potato_daily +
                    fruit_daily +
                    nut_daily +
                    veggie_daily +
                    meat_sub_daily +
                    snack_daily +
                    mixed_dish_daily +
                    sauce_daily +
                    fats_daily +
                    non_alc_beverage_daily +
                    alc_beverage_daily +
                    total_weight_food_daily +
                    sex +
                    age_at_baseline,
                data = data_icc)
model1 %>%
    parameters(exponentiate = T)

model1 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    proc_meat_daily +
                    fish_daily +
                    poultry_daily +
                    dairy_daily +
                    egg_daily +
                    cereal_refined_daily +
                    whole_grain_daily +
                    potato_daily +
                    fruit_daily +
                    nut_daily +
                    veggie_daily +
                    meat_sub_daily +
                    snack_daily +
                    mixed_dish_daily +
                    sauce_daily +
                    fats_daily +
                    non_alc_beverage_daily +
                    alc_beverage_daily +
                    total_weight_food_daily +
                    sex +
                    age_at_baseline,
                data = data_icc)
model1 %>%
    parameters(exponentiate = T)

model1 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    red_meat_daily +
                    fish_daily +
                    poultry_daily +
                    dairy_daily +
                    egg_daily +
                    cereal_refined_daily +
                    whole_grain_daily +
                    potato_daily +
                    fruit_daily +
                    nut_daily +
                    veggie_daily +
                    meat_sub_daily +
                    snack_daily +
                    mixed_dish_daily +
                    sauce_daily +
                    fats_daily +
                    non_alc_beverage_daily +
                    alc_beverage_daily +
                    total_weight_food_daily +
                    sex +
                    age_at_baseline,
                data = data_icc)
model1 %>%
    parameters(exponentiate = T)



# Model 2: leave one out
model2 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    fish_daily +
                    poultry_daily +
                    dairy_daily +
                    egg_daily +
                    cereal_refined_daily +
                    whole_grain_daily +
                    potato_daily +
                    fruit_daily +
                    nut_daily +
                    veggie_daily +
                    meat_sub_daily +
                    snack_daily +
                    mixed_dish_daily +
                    sauce_daily +
                    fats_daily +
                    non_alc_beverage_daily +
                    alc_beverage_daily +
                    total_weight_food_daily +
                    sex +
                    age_at_baseline +
                    education +
                    tdi +
                    ethnicity +
                    bmi_category +
                    exercise +
                    smoking +
                    wc +
                    diabetes +
                    cholelith +
                    alc_liver +
                    nafld +
                    cystectomy,
                data = data_hcc)
model2 %>%
    parameters(exponentiate = T)

model2 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    proc_meat_daily +
                    fish_daily +
                    poultry_daily +
                    dairy_daily +
                    egg_daily +
                    cereal_refined_daily +
                    whole_grain_daily +
                    potato_daily +
                    fruit_daily +
                    nut_daily +
                    veggie_daily +
                    meat_sub_daily +
                    snack_daily +
                    mixed_dish_daily +
                    sauce_daily +
                    fats_daily +
                    non_alc_beverage_daily +
                    alc_beverage_daily +
                    total_weight_food_daily +
                    sex +
                    age_at_baseline +
                    education +
                    tdi +
                    ethnicity +
                    bmi_category +
                    exercise +
                    smoking +
                    wc +
                    diabetes +
                    cholelith +
                    alc_liver +
                    nafld +
                    cystectomy,
                data = data_hcc)
model2 %>%
    parameters(exponentiate = T)

model2 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    red_meat_daily +
                    fish_daily +
                    poultry_daily +
                    dairy_daily +
                    egg_daily +
                    cereal_refined_daily +
                    whole_grain_daily +
                    potato_daily +
                    fruit_daily +
                    nut_daily +
                    veggie_daily +
                    meat_sub_daily +
                    snack_daily +
                    mixed_dish_daily +
                    sauce_daily +
                    fats_daily +
                    non_alc_beverage_daily +
                    alc_beverage_daily +
                    total_weight_food_daily +
                    sex +
                    age_at_baseline +
                    education +
                    tdi +
                    ethnicity +
                    bmi_category +
                    exercise +
                    smoking +
                    wc +
                    diabetes +
                    cholelith +
                    alc_liver +
                    nafld +
                    cystectomy,
                data = data_hcc)
model2 %>%
    parameters(exponentiate = T)
