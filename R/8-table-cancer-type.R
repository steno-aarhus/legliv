library(survival)
library(gtsummary)

model0t_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_hcc
)

m0t_hcc <- model0t_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    ) %>%
    modify_caption("**Stratified on liver cancer type** (N = {N})")

model1t_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_hcc
)

m1t_hcc <- model1t_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )



model2t_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +
        +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_hcc
)

m2t_hcc <- model2t_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )


###############################################################################################################


model0r_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_hcc
)

m0r_hcc <- model0r_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )

model1r_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_hcc
)

m1r_hcc <- model1r_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )



model2r_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +
        +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_hcc
)

m2r_hcc <- model2r_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )



###############################################################################################################


model0p_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_hcc
)

m0p_hcc <- model0p_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )

model1p_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_hcc
)

m1p_hcc <- model1p_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )



model2p_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +
        +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_hcc
)

m2p_hcc <- model2p_hcc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "HCC",
    )

model0t_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_icc
)

m0t_icc <- model0t_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    )

model1t_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_icc
)

m1t_icc <- model1t_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    )



model2t_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +
        +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_icc
)

m2t_icc <- model2t_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    )


###############################################################################################################


model0r_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_icc
)

m0r_icc <- model0r_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    ) %>%
    bold_p(t=0.05)

model1r_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_icc
)

m1r_icc <- model1r_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    ) %>%
    bold_p(t=0.05)



model2r_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +
        +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_icc
)

m2r_icc <- model2r_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    )



###############################################################################################################


model0p_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_icc
)

m0p_icc <- model0p_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    )

model1p_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_icc
)

m1p_icc <- model1p_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    )



model2p_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +
        +
        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_icc
)

m2p_icc <- model2p_icc %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ "ICC",
    )

row1 <- tbl_merge(list(m0t_hcc, m0r_hcc, m0p_hcc), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"))
row2 <- tbl_merge(list(m1t_hcc, m1r_hcc, m1p_hcc))
row3 <- tbl_merge(list(m2t_hcc, m2r_hcc, m2p_hcc))
row4 <- tbl_merge(list(m0t_icc, m0r_icc, m0p_icc))
row5 <- tbl_merge(list(m1t_icc, m1r_icc, m1p_icc))
row6 <- tbl_merge(list(m2t_icc, m2r_icc, m2p_icc))

table_wc <-
    tbl_stack(list(row1, row4, row2, row5, row3, row6), group_header = c("Model 0", "Model 0", "Model 1", "Model 1", "Model 2", "Model 2")) %>%
    modify_header(label = "**15 g/day substitution**")

table_wc
