library(survival)
install.packages('parameters')
library(parameters)

# Model 1:
# minimally adjusted for age at recruitment, sex, total energy intake, and all other dietary components:
model1 <- coxph(Surv(time = time, event = status=="liver cancer") ~
                     legume_daily +
                     red_proc_meat_daily +
                     age_at_baseline +
                     sex +
                     total_energy_food_daily +
                     poultry_daily +
                     fish_daily +
                     dairy_daily +
                     egg_daily +
                     cereal_refined_daily +
                     whole_grain_daily +
                     veggie_daily +
                     potato_daily +
                     fruit_daily +
                     nut_daily +
                     meat_sub_daily +
                     snack_daily +
                     mixed_dish_daily +
                     sauce_daily +
                     fats_daily +
                     non_alc_beverage_daily +
                     alc_beverage_daily,
                 data = data)

model1 %>%
    parameters(exponentiate = T)

coefficients <- coef(model1)

# Summarize the coefficients
hazard_ratio <- exp(sum(coefficients[-1]))  # Exclude the intercept term

# Print the hazard ratio
print(hazard_ratio)

# Model 2:
# Further adjusted for education, TDI, ethnicity, BMI, physical activity, smoking, alcohol,
# waist circumference, T2D, cholelithiasis, and cholecystectomy

model2 <- coxph(Surv(time = time, event = status=="liver cancer") ~
                    legume_daily +
                    red_proc_meat_daily +
                    age_at_baseline +
                    sex +
                    total_energy_food_daily +
                    poultry_daily +
                    fish_daily +
                    dairy_daily +
                    egg_daily +
                    cereal_refined_daily +
                    whole_grain_daily +
                    veggie_daily +
                    potato_daily +
                    fruit_daily +
                    nut_daily +
                    meat_sub_daily +
                    snack_daily +
                    mixed_dish_daily +
                    sauce_daily +
                    fats_daily +
                    non_alc_beverage_daily +
                    alc_beverage_daily +
                    education +
                    tdi +
                    ethnicity +
                    bmi_category +
                    phys_acti +
                    smoking +
                    wc,
                data = data)

model2 %>%
    parameters(exponentiate = T)



model0 <- coxph(Surv(time = time, event = status=='liver cancer') ~ red_proc_meat_daily,
                data = data)

model0 %>%
    summary()

data %>%
    select(time) %>%
    summary()

data %>%
    select(total_energy_food_daily) %>%
    summary()

data_liver %>%
    ggplot(aes(x = total_energy_food_daily)) +
    geom_histogram(bins = 15)

data_liver %>%
    ggplot(aes(sample = total_energy_food_daily)) +
    stat_qq() +
    stat_qq_line()
