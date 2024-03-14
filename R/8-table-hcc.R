library(survival)
library(gtsummary)

model0t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_hcc
)

m0t <- model0t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    ) %>%
    modify_caption("**Liver cancer is defined as hepatocellular carcinoma (HCC)** (N = {N})")

model1t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline,
    data = data_hcc
)

m1t <- model1t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )



model2t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_hcc
)

m2t <- model2t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )


###############################################################################################################


model0r <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_hcc
)

m0r <- model0r %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )

model1r <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline,
    data = data_hcc
)

m1r <- model1r %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )



model2r <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_hcc
)

m2r <- model2r %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )



###############################################################################################################


model0p <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_hcc
)

m0p <- model0p %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )

model1p <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline,
    data = data_hcc
)

m1p <- model1p %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )



model2p <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily +
        sex + age_at_baseline +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_hcc
)

m2p <- model2p %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ " ",
    )


row1 <- tbl_merge(list(m0t, m0r, m0p), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"))
row2 <- tbl_merge(list(m1t, m1r, m1p))
row3 <- tbl_merge(list(m2t, m2r, m2p))

tbl_stack <-
    tbl_stack(list(row1, row2, row3), group_header = c("Model 0", "Model 1", "Model 2")) %>%
    modify_header(label = "**15 g/day substitution**")

tbl_stack
