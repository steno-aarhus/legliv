library(survival)
install.packages('parameters')
library(parameters)
install.packages('rms')
library(rms)

# Model 1: leave one out (red_proc_meat_daily)
# minimally adjusted for age at recruitment, sex, total energy intake, and all other dietary components:
model1 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    sex +
                    age_at_baseline +
                    total_weight_food_daily +
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

# subtraction model
# difhr <- exp(model1$coefficients[1]-model1$coefficients[2])
# difhr


# Model 1 but only red meat:
model1_red <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    proc_meat_daily +
                    sex +
                    age_at_baseline +
                    total_weight_food_daily +
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

model1_red %>%
    parameters(exponentiate = T)

# Model 1 but only processed meat:
model1_proc <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                        legume_daily_25 +
                        red_meat_daily +
                        sex +
                        age_at_baseline +
                        total_weight_food_daily +
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

model1_proc %>%
    parameters(exponentiate = T)

# Model 2:
# Further adjusted for education, TDI, ethnicity, BMI, physical activity, smoking, alcohol,
# waist circumference, T2D, cholelithiasis, and cholecystectomy

model2 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    sex +
                    age_at_baseline +
                    total_weight_food_daily +
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

# model 2 but only red meat
model2_red <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_daily_25 +
                    proc_meat_daily +
                    sex +
                    age_at_baseline +
                    total_weight_food_daily +
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
                    wc +
                    diabetes +
                    cholelith +
                    alc_liver +
                    nafld +
                    cystectomy,
                data = data)

model2_red %>%
    parameters(exponentiate = T)


# model 2 but only processed meat
model2_proc <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                        legume_daily_25 +
                        red_meat_daily +
                        sex +
                        age_at_baseline +
                        total_weight_food_daily +
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
                        wc +
                        cholelith +
                        alc_liver +
                        nafld +
                        cystectomy,
                    data = data)

model2_proc %>%
    parameters(exponentiate = T)


model0 <- coxph(Surv(time = time, event = status=='liver cancer') ~ proc_meat_daily_25,
                data = data)

model0 %>%
    parameters(exponentiate = T)

difhr <- exp(model0$coefficients[2]-model0$coefficients[1])
difhr


data %>%
    select(ends_with("daily")) %>%
    summary()

model_pulse <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    pulse_daily_25 +
                    sex +
                    age_at_baseline +
                    total_weight_food_daily +
                    legume_other_daily +
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
                        wc +
                        cholelith +
                        alc_liver +
                        nafld +
                        cystectomy,
                data = data)

model_pulse %>%
    parameters(exponentiate = T)


fit <- coxph(Surv(time = time, event = status=="liver cancer") ~ rcs(legume_daily, 4),
             data = data, ties='breslow')
fit %>%
    parameters(exponentiate = T)

# Generate values of red_proc_meat_daily to cover the range of your data
legume_daily_values <- seq(min(data$legume_daily), 50, length.out = 100)

# Predict the hazard ratios and their standard errors using the fitted model
predictions <- predict(fit, newdata = data.frame(legume_daily = legume_daily_values), type = "terms", se.fit = TRUE)

# Extract predicted hazard ratios and standard errors
predicted_hr <- exp(predictions$fit)
se_hr <- predictions$se.fit

# Calculate upper and lower bounds of the confidence interval
upper_bound <- exp(log(predicted_hr) + 1.96 * se_hr)
lower_bound <- exp(log(predicted_hr) - 1.96 * se_hr)

# Plot the predicted hazard ratios against legume_daily
plot(legume_daily_values, predicted_hr, type = "l", xlab = "legume_daily", ylab = "Hazard Ratio", main = "Spline Function of legume_daily", ylim = c(0.5, 1.5))

# Add confidence interval
lines(legume_daily_values, upper_bound, col = "blue", lty = 2)
lines(legume_daily_values, lower_bound, col = "blue", lty = 2)
