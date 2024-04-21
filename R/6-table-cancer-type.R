library(survival)
library(gt)
library(gtsummary)

set_gtsummary_theme(theme_gtsummary_journal("jama"),
                    quiet = TRUE)

model1t_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data_hcc
)

model2t_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_hcc
)

model1r_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data_hcc
)

model2r_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_hcc
)

model1p_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data_hcc
)

model2p_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_hcc
)

m1t_hcc  <- model1t_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m1r_hcc  <- model1r_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m1p_hcc  <- model1p_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

m2t_hcc  <- model2t_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_hcc  <- model2r_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_hcc  <- model2p_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

model1t_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data_icc
)

model2t_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_icc
)

model1r_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data_icc
)

model2r_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_icc
)

model1p_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily,
  data = data_icc
)

model2p_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_icc
)

m1t_icc  <- model1t_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m1r_icc  <- model1r_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m1p_icc  <- model1p_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

m2t_icc  <- model2t_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_icc  <- model2r_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_icc  <- model2p_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

row1 <- tbl_merge(list(m1t_hcc, m2t_hcc)) %>%
  modify_spanning_header(everything() ~ NA_character_)
row2 <- tbl_merge(list(m1r_hcc, m2r_hcc))
row3 <- tbl_merge(list(m1p_hcc, m2p_hcc))
row4 <- tbl_merge(list(m1t_icc, m2t_icc))
row5 <- tbl_merge(list(m1r_icc, m2r_icc))
row6 <- tbl_merge(list(m1p_icc, m2p_icc))

table_cancer_type <-
  tbl_stack(list(row1, row2, row3, row4, row5, row6),
            group_header = c("Hepatocellular carcinoma", "Hepatocellular carcinoma", "Hepatocellular carcinoma", "Intrahepatic cholangiocarcinoma", "Intrahepatic cholangiocarcinoma", "Intrahepatic cholangiocarcinoma")) %>%
  modify_caption("**Supplementary table 2. Substitution of total meat, red meat and processed meat with legumes and hazard ratios and 95% confidence intervals for hepatocellular carcinoma and intrahepatic cholangiocarcinoma.** (N = {N})") %>%
  modify_header(label = "**15 g/day substitution**") %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  modify_table_styling(
    column = c(p.value_1, p.value_2),
    hide = TRUE
  ) %>%
  as_gt() %>%
  tab_spanner(
    label = "Model 1",
    columns = c(estimate_1, ci_1, p.value_1)
  ) %>%
  tab_spanner(
    label = "Model 2",
    columns = c(estimate_2, ci_2, p.value_2)
  ) %>%
  tab_footnote(
    footnote = "Adjusted for age (as underlying timescale), other food groups, and total food intake.",
    locations = cells_column_spanners(spanners = "Model 1")
  ) %>%
  tab_footnote(
    footnote = "Further adjusted for sex, educational level, Townsend deprivation index, living alone, physical activity, smoking, alcohol intake, and waist circumference.",
    locations = cells_column_spanners(spanners = "Model 2")
  )
table_cancer_type
