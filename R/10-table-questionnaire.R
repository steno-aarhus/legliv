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
    data = data
)

m0t <- model0t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
    ) %>%
    modify_caption("**Different levels of diet questionnaires completed** (N = {N})")

model1t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data
)

m1t <- model1t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
    )



model2t <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data
)

m2t <- model2t %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
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
    data = data
)

m0r <- model0r %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
    )

model1r <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data
)

m1r <- model1r %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
    )



model2r <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data
)

m2r <- model2r %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
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
    data = data
)

m0p <- model0p %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
    )

model1p <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data
)

m1p <- model1p %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
    )



model2p <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data
)

m2p <- model2p %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=2",
    )

model0t_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_3
)

m0t_ques_3 <- model0t_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )

model1t_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_3
)

m1t_ques_3 <- model1t_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )



model2t_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_3
)

m2t_ques_3 <- model2t_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )


###############################################################################################################


model0r_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_3
)

m0r_ques_3 <- model0r_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )

model1r_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_3
)

m1r_ques_3 <- model1r_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )



model2r_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_3
)

m2r_ques_3 <- model2r_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )



###############################################################################################################


model0p_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_3
)

m0p_ques_3 <- model0p_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )

model1p_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_3
)

m1p_ques_3 <- model1p_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )



model2p_ques_3 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_3
)

m2p_ques_3 <- model2p_ques_3 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=3",
    )

model0t_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_4
)

m0t_ques_4 <- model0t_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )

model1t_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_4
)

m1t_ques_4 <- model1t_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )



model2t_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_4
)

m2t_ques_4 <- model2t_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )


###############################################################################################################


model0r_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_4
)

m0r_ques_4 <- model0r_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )

model1r_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_4
)

m1r_ques_4 <- model1r_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )



model2r_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_4
)

m2r_ques_4 <- model2r_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )



###############################################################################################################


model0p_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_4
)

m0p_ques_4 <- model0p_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )

model1p_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_4
)

m1p_ques_4 <- model1p_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )



model2p_ques_4 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_4
)

m2p_ques_4 <- model2p_ques_4 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=4",
    )

model0t_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_5
)

m0t_ques_5 <- model0t_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )

model1t_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_5
)

m1t_ques_5 <- model1t_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )



model2t_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_5
)

m2t_ques_5 <- model2t_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )


###############################################################################################################


model0r_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_5
)

m0r_ques_5 <- model0r_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )

model1r_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_5
)

m1r_ques_5 <- model1r_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )



model2r_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + proc_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_5
)

m2r_ques_5 <- model2r_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    ) %>%
    bold_p(t=0.05)



###############################################################################################################


model0p_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily,
    data = data_ques_5
)

m0p_ques_5 <- model0p_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )

model1p_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex,
    data = data_ques_5
)

m1p_ques_5 <- model1p_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )



model2p_ques_5 <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
        legume_daily_15 + red_meat_daily +
        whole_grain_daily + poultry_daily + fish_daily + dairy_daily +
        egg_daily + cereal_refined_daily + veggie_daily + potato_daily +
        fruit_daily + nut_daily + meat_sub_daily + snack_daily +
        mixed_dish_daily + sauce_daily + fats_daily +
        non_alc_beverage_daily + alc_beverage_daily + total_weight_food_daily + sex +

        education + tdi + ethnicity + spouse +
        bmi_category + exercise + smoking + wc + alcohol_daily +
        diabetes + cholelith + nafld + cystectomy,
    data = data_ques_5
)

m2p_ques_5 <- model2p_ques_5 %>%
    tbl_regression(
        exponentiate = T,
        include = legume_daily_15,
        label = legume_daily_15 ~ ">=5",
    )

row1 <- tbl_merge(list(m0t, m0r, m0p), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat")) %>%
row2 <- tbl_merge(list(m1t, m1r, m1p))
row3 <- tbl_merge(list(m2t, m2r, m2p))
row4 <- tbl_merge(list(m0t_ques_3, m0r_ques_3, m0p_ques_3))
row5 <- tbl_merge(list(m1t_ques_3, m1r_ques_3, m1p_ques_3))
row6 <- tbl_merge(list(m2t_ques_3, m2r_ques_3, m2p_ques_3))
row7 <- tbl_merge(list(m0t_ques_4, m0r_ques_4, m0p_ques_4))
row8 <- tbl_merge(list(m1t_ques_4, m1r_ques_4, m1p_ques_4))
row9 <- tbl_merge(list(m2t_ques_4, m2r_ques_4, m2p_ques_4))
row10 <- tbl_merge(list(m0t_ques_5, m0r_ques_5, m0p_ques_5))
row11 <- tbl_merge(list(m1t_ques_5, m1r_ques_5, m1p_ques_5))
row12 <- tbl_merge(list(m2t_ques_5, m2r_ques_5, m2p_ques_5))

table_ques <-
    tbl_stack(list(row1, row4, row7, row10, row2, row5, row8, row11, row3, row6, row9, row12),
              group_header = c("Model 0", "Model 0", "Model 0", "Model 0",
                               "Model 1", "Model 1", "Model 1", "Model 1",
                               "Model 2", "Model 2", "Model 2", "Model 2")
    ) %>%
    modify_header(label = "**15 g/day substitution**")

table_ques
