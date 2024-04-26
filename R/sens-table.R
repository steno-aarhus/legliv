set_gtsummary_theme(theme_gtsummary_journal("jama"),
                    quiet = TRUE)

model2t_alc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_alc
)

model2r_alc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_alc
)

model2p_alc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_alc
)

m2t_alc <- model2t_alc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_alc <- model2r_alc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_alc <- model2p_alc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

model2t_misreporter <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_misreporter
)

model2r_misreporter <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_misreporter
)

model2p_misreporter <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_misreporter
)

m2t_misreporter <- model2t_misreporter %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_misreporter <- model2r_misreporter %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_misreporter <- model2p_misreporter %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )


model2t_3_ques_comp <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_3_ques_comp
)

model2r_3_ques_comp <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_3_ques_comp
)

model2p_3_ques_comp <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_3_ques_comp
)

m2t_3_ques_comp <- model2t_3_ques_comp %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_3_ques_comp <- model2r_3_ques_comp %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_3_ques_comp <- model2p_3_ques_comp %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

row1 <- tbl_merge(list(m2t_alc, m2t_misreporter, m2t_3_ques_comp)) %>%
  modify_spanning_header(everything() ~ NA_character_)
row2 <- tbl_merge(list(m2r_alc, m2r_misreporter, m2r_3_ques_comp))
row3 <- tbl_merge(list(m2p_alc, m2p_misreporter, m2p_3_ques_comp))

table_sens <-
  tbl_stack(list(row1, row2, row3)) %>%
  modify_caption("**Supplementary table 3. Exclusion of participants** (N = {N})") %>%
  modify_header(label = "**15 g/day substitution**") %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  modify_table_styling(
    column = c(p.value_1, p.value_2, p.value_3),
    hide = TRUE
  ) %>%
  as_gt() %>%
  tab_spanner(
    label = "High alchol consumption",
    columns = c(estimate_1, ci_1, p.value_1)
  ) %>%
  tab_spanner(
    label = "Food intake outliers",
    columns = c(estimate_2, ci_2, p.value_2)
  ) %>%
  tab_spanner(
    label = "< 3 Oxfords WebQs",
    columns = c(estimate_3, ci_3, p.value_3)
  )
table_sens
