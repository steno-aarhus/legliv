library(survival)
install.packages('parameters')
library(parameters)
install.packages('rms')
library(rms)

# Model 1:
# minimally adjusted for age at recruitment, sex, total energy intake, and all other dietary components:
model1 <- coxph(Surv(time = time, event = status=="liver cancer") ~
                     legume_daily_25 +
                     red_proc_meat_daily_25 +
                     age_at_baseline +
                     sex +
                     total_energy_food_daily +
                     poultry_daily_25 +
                     fish_daily_25 +
                     dairy_daily_25 +
                     egg_daily_25 +
                     cereal_refined_daily_25 +
                     whole_grain_daily_25 +
                     veggie_daily_25 +
                     potato_daily_25 +
                     fruit_daily_25 +
                     nut_daily_25 +
                     meat_sub_daily_25 +
                     snack_daily25 +
                     mixed_dish_daily_25 +
                     sauce_daily_25 +
                     fats_daily_25 +
                     non_alc_beverage_daily_25 +
                     alc_beverage_daily_25,
                 data = data)

model1 %>%
    parameters(exponentiate = T)

# Model 2:
# Further adjusted for education, TDI, ethnicity, BMI, physical activity, smoking, alcohol,
# waist circumference, T2D, cholelithiasis, and cholecystectomy

model2 <- coxph(Surv(time = time, event = status=="liver cancer") ~
                    legume_daily_25 +
                    red_proc_meat_daily_25 +
                    age_at_baseline +
                    sex +
                    total_energy_food_daily +
                    poultry_daily_25 +
                    fish_daily_25 +
                    dairy_daily_25 +
                    egg_daily_25 +
                    cereal_refined_daily_25 +
                    whole_grain_daily_25 +
                    veggie_daily_25 +
                    potato_daily_25 +
                    fruit_daily_25 +
                    nut_daily_25 +
                    meat_sub_daily_25 +
                    snack_daily25 +
                    mixed_dish_daily_25 +
                    sauce_daily_25 +
                    fats_daily_25 +
                    non_alc_beverage_daily_25 +
                    alc_beverage_daily_25 +
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



model0 <- coxph(Surv(time = time, event = status=='liver cancer') ~ legume_daily_25 + red_proc_meat_daily_25 + total_energy_food_daily,
                data = data)

model0 %>%
    parameters(exponentiate = T)


fit <- coxph(Surv(time = time, event = status=="liver cancer") ~ rcs(legume_daily, 4),
             data = data, ties='breslow')

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
