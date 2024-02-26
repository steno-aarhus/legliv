library(survival)
library(parameters)

fit_cox <- coxph(Surv(time = time, event = status=="liver cancer") ~
                     total_energy_food_daily +
                     total_weight_food_daily +
                     legume_daily +
                     red_proc_meat_daily +
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
                     sex +
                     age_recruit,
                 data = data)

fit_cox <- coxph(Surv(time = time, event = status=="liver cancer") ~
                     bmi * sex,
                 data = data)

fit_cox %>%
    parameters(exponentiate = T)

fit_cox %>%
    summary()

data_liver %>%
    select(total_energy_food_daily) %>%
    summary()

data_liver %>%
    ggplot(aes(x = total_energy_food_daily)) +
    geom_histogram(bins = 15)

data_liver %>%
    ggplot(aes(sample = total_energy_food_daily)) +
    stat_qq() +
    stat_qq_line()
