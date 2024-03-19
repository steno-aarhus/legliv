model1t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_category + red_meat_daily + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex,
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
        sex +
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

data_legume_quintile <- data %>%
    select(legume_category, legume_daily)

mean_legume <- data_legume_quintile %>%
    group_by(legume_category) %>%
    summarise(mean_legume_daily = mean(legume_daily, na.rm = TRUE))

tbl_mean_legume <- mean_legume %>%
    tbl_summary()
tbl_category <- data_legume_quintile %>%
    select(-legume_daily) %>%
    tbl_summary()
tbl_mean_legume
tbl_category
tbl_merge_sum <- tbl_merge(
    list(tbl_category, tbl_mean_legume)
)
tbl_merge_sum
tbl_merge <- tbl_merge(
    tbls = list(tbl_category, tbl_mean_legume, m1t, m2t)
)
tbl_merge
