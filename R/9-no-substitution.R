# Model 1:
# minimally adjusted for age at recruitment, sex, total energy intake, and all other dietary components:
model1 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    red_meat_daily_25 +
                    proc_meat_daily_25 +
                    fish_daily_25 +
                    poultry_daily_25 +
                    dairy_daily_25 +
                    egg_daily_25 +
                    cereal_refined_daily_25 +
                    whole_grain_daily_25 +
                    potato_daily_25 +
                    fruit_daily_25 +
                    nut_daily_25 +
                    veggie_daily_25 +
                    meat_sub_daily_25 +
                    snack_daily_25 +
                    mixed_dish_daily_25 +
                    sauce_daily_25 +
                    fats_daily_25 +
                    non_alc_beverage_daily_25 +
                    alc_beverage_daily_25 +
                    total_weight_food_daily +
                    sex +
                    age_at_baseline,
                data = data)

model1 %>%
    parameters(exponentiate = T)



# Model 1: leave one out (red_proc_meat_daily)
# minimally adjusted for age at recruitment, sex, total energy intake, and all other dietary components:
model2 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    red_meat_daily_25 +
                    proc_meat_daily_25 +
                    fish_daily_25 +
                    poultry_daily_25 +
                    dairy_daily_25 +
                    egg_daily_25 +
                    cereal_refined_daily_25 +
                    whole_grain_daily_25 +
                    potato_daily_25 +
                    fruit_daily_25 +
                    nut_daily_25 +
                    veggie_daily_25 +
                    meat_sub_daily_25 +
                    snack_daily_25 +
                    mixed_dish_daily_25 +
                    sauce_daily_25 +
                    fats_daily_25 +
                    non_alc_beverage_daily_25 +
                    alc_beverage_daily_25 +
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
                data = data)

model2 %>%
    parameters(exponentiate = T)


