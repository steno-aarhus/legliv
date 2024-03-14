model0t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_category + red_meat_daily + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data
)

m0t <- model0t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_category,
        label = legume_category ~ "Legume category",
    ) %>%
    modify_caption("**Full cohort** (N = {N})")

model1t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_category + red_meat_daily + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline,
    data = data
)

m1t <- model1t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_category,
        label = legume_category ~ "Legume category",
    ) %>%
    modify_caption("**Full cohort** (N = {N})")


model2t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_category + red_meat_daily + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data
)

m2t <- model2t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_category,
        label = legume_category ~ "Legume category",
    ) %>%
    modify_caption("**Full cohort** (N = {N})")

tbl_merge <- tbl_merge(
    tbls = list(m0t,m1t,m2t),
    tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**")
)
tbl_merge
