data <- data %>%
    mutate(
        legume_strata = case_when(
            legume_daily == 0 ~ "0",
            legume_daily > 0 & legume_daily <= 10 ~ "10",
            legume_daily > 10 & legume_daily <= 20 ~ "20",
            legume_daily > 20 & legume_daily <= 30 ~ "30",
            legume_daily > 30 & legume_daily <= 40 ~ "40",
            legume_daily > 40 & legume_daily <= 50 ~ "50",
            legume_daily > 50 & legume_daily <= 60 ~ "60",
            legume_daily > 60 & legume_daily <= 70 ~ "70",
            legume_daily > 70 & legume_daily <= 80 ~ "80",
            legume_daily > 80 & legume_daily <= 90 ~ "90",
            legume_daily > 90 ~ "above_90"
        ),
        legume_strata = factor(legume_strata)
    )


table(data$legume_strata)

loghr <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ legume_strata,
               data = data) %>%
    coef()

coxph(Surv(time = status_age, event = status=="liver cancer") ~ legume_strata,
      data = data) %>%
    parameters(exponentiate = T)

data.frame(left=c(0,0.5,10,20,30,40, 50, 60, 70, 80, 90, 100),
           loghr=c(0,loghr,last(loghr))) |>
    ggplot(aes(left,loghr)) +
    geom_step() +
    labs(y="log(HR)", x="Legume intake (g/day)") +
    theme_classic()

library(rms)
spline <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ rcs(legume_daily_25,3),
      data = data, ties = 'breslow')

termplot(spline, term=1, se=TRUE, col.term=1, col.se=1)

spline %>%
    parameters(exponentiate = T)

data <- data %>%
    mutate(
        proc_meat_strata = case_when(
            proc_meat_daily == 0 ~ "0",
            proc_meat_daily > 0 & proc_meat_daily <= 10 ~ "10",
            proc_meat_daily > 10 & proc_meat_daily <= 20 ~ "20",
            proc_meat_daily > 20 & proc_meat_daily <= 30 ~ "30",
            proc_meat_daily > 30 & proc_meat_daily <= 40 ~ "40",
            proc_meat_daily > 40 & proc_meat_daily <= 50 ~ "50",
            proc_meat_daily > 50 ~ "above_50"
        ),
        proc_meat_strata = factor(proc_meat_strata)
    )

table(data$proc_meat_strata)

loghr <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ proc_meat_strata,
               data = data) %>%
    coef()

data.frame(left=c(0,0.5,10,20,30,40, 50, 60),
           loghr=c(0,loghr,last(loghr))) |>
    ggplot(aes(left,loghr)) +
    geom_step() +
    labs(y="log(HR)", x="Processed meat intake (g/day)") +
    theme_classic()

data <- data %>%
    mutate(
        red_meat_strata = case_when(
            red_meat_daily >= 0 & red_meat_daily <= 10 ~ "0",
            red_meat_daily > 10 & red_meat_daily <= 20 ~ "20",
            red_meat_daily > 20 & red_meat_daily <= 30 ~ "30",
            red_meat_daily > 30 & red_meat_daily <= 40 ~ "40",
            red_meat_daily > 40 & red_meat_daily <= 50 ~ "50",
            red_meat_daily > 50 ~ "above_50"
        ),
        red_meat_strata = factor(red_meat_strata)
    )

table(data$proc_meat_strata)

loghr <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ red_meat_strata,
               data = data) %>%
    coef()

data.frame(left=c(0,10,20,30,40, 50, 60),
           loghr=c(0,loghr,last(loghr))) |>
    ggplot(aes(left,loghr)) +
    geom_step() +
    labs(y="log(HR)", x="Red meat intake (g/day)") +
    theme_classic()

data <- data %>%
    mutate(
        red_proc_meat_strata = case_when(
            red_proc_meat_daily == 0 ~ "0",
            red_proc_meat_daily > 0 & red_proc_meat_daily <= 10 ~ "10",
            red_proc_meat_daily > 10 & red_proc_meat_daily <= 20 ~ "20",
            red_proc_meat_daily > 20 & red_proc_meat_daily <= 30 ~ "30",
            red_proc_meat_daily > 30 & red_proc_meat_daily <= 40 ~ "40",
            red_proc_meat_daily > 40 & red_proc_meat_daily <= 50 ~ "50",
            red_proc_meat_daily > 50 & red_proc_meat_daily <= 60 ~ "60",
            red_proc_meat_daily > 60 & red_proc_meat_daily <= 70 ~ "70",
            red_proc_meat_daily > 70 & red_proc_meat_daily <= 80 ~ "80",
            red_proc_meat_daily > 80 & red_proc_meat_daily <= 90 ~ "90",
            red_proc_meat_daily > 90 ~ "above_90"
        ),
        red_proc_meat_strata = factor(red_proc_meat_strata)
    )

table(data$red_proc_meat_strata)

loghr <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ red_proc_meat_strata,
               data = data) %>%
    coef()

coxph(Surv(time = status_age, event = status=="liver cancer") ~ red_proc_meat_strata,
      data = data) %>%
    parameters(exponentiate = T)

data.frame(left=c(0,0.5,10,20,30,40, 50, 60, 70, 80, 90, 100),
           loghr=c(0,loghr,last(loghr))) |>
    ggplot(aes(left,loghr)) +
    geom_step() +
    labs(y="log(HR)", x="Total red and processed meat intake (g/day)") +
    theme_classic()




data <- data %>%
    mutate(
        veggie_strata = case_when(
            veggie_daily <= 50 ~ "050",
            veggie_daily > 50 & veggie_daily <= 100 ~ "100",
            veggie_daily > 100 & veggie_daily <= 150 ~ "150",
            veggie_daily > 150 & veggie_daily <= 200 ~ "200",
            veggie_daily > 200 & veggie_daily <= 250 ~ "250",
            veggie_daily > 250 & veggie_daily <= 300 ~ "300",
            veggie_daily > 300 ~ "above_300"
        ),
        veggie_strata = factor(veggie_strata)
    )

table(data$veggie_strata)

loghr <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ veggie_strata,
               data = data) %>%
    coef()


coxph(Surv(time = status_age, event = status=="liver cancer") ~ veggie_strata,
      data = data) %>%
    parameters(exponentiate = T)

data.frame(left=c(0,50,100,150,200,250,300,350),
           loghr=c(0,loghr,last(loghr))) |>
    ggplot(aes(left,loghr)) +
    geom_step() +
    labs(y="log(HR)", x="Vegetables intake (g/day)") +
    theme_classic()

data <- data %>%
    mutate(
        fish_strata = case_when(
            fish_daily == 0 ~ "0",
            fish_daily > 0 & fish_daily <= 10 ~ "10",
            fish_daily > 10 & fish_daily <= 20 ~ "20",
            fish_daily > 20 & fish_daily <= 30 ~ "30",
            fish_daily > 30 & fish_daily <= 40 ~ "40",
            fish_daily > 40 & fish_daily <= 50 ~ "50",
            fish_daily > 50 ~ "above_50"
        ),
        fish_strata = factor(fish_strata)
    )

table(data$fish_strata)

loghr <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ fish_strata,
               data = data) %>%
    coef()


coxph(Surv(time = status_age, event = status=="liver cancer") ~ fish_strata,
      data = data) %>%
    parameters(exponentiate = T)

data.frame(left=c(0, 0.1, 10, 20, 30, 40, 50, 60),
           loghr=c(0,loghr,last(loghr))) |>
    ggplot(aes(left,loghr)) +
    geom_step() +
    labs(y="log(HR)", x="Fish intake (g/day)") +
    theme_classic()


model1 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ red_meat_strata,
               data = data)

model1 %>%  parameters(exponentiate = T)

loghr <- coxph(Surv(time = status_age, event = status=="liver cancer") ~ proc_meat_strata,
                data = data) %>%
    coef()
print(loghr)


data.frame(left=c(0,10,20,30,40, 50, 60, 70),
           loghr=c(0,loghr,last(loghr))) |>
    ggplot(aes(left,loghr)) +
    geom_step() +
    labs(y="log(HR)", x="intake (g/day)") +
    theme_classic()

data %>%
    select(red_meat_strata) %>%
    group_by() %>%
    summary()
# Model 1: leave one out (red_proc_meat_daily)
# minimally adjusted for age at recruitment, sex, total energy intake, and all other dietary components:
model1 <- coxph(Surv(time = status_age, event = status=="liver cancer") ~
                    legume_strata +
                    sex +
                    age_at_baseline +
                    total_weight_food_daily +
                    red_meat_daily +
                    proc_meat_daily+
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
