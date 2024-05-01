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
    label = legume_daily_15 ~ " ",
  )

m2r_alc <- model2r_alc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2p_alc <- model2p_alc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
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
    label = legume_daily_15 ~ " ",
  )

m2r_misreporter <- model2r_misreporter %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2p_misreporter <- model2p_misreporter %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
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
    label = legume_daily_15 ~ " ",
  )

m2r_3_ques_comp <- model2r_3_ques_comp %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2p_3_ques_comp <- model2p_3_ques_comp %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

model2t_liver_disease <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_liver_disease
)

model2r_liver_disease <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_liver_disease
)

model2p_liver_disease <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_liver_disease
)

m2t_liver_disease <- model2t_liver_disease %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2r_liver_disease <- model2r_liver_disease %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2p_liver_disease <- model2p_liver_disease %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

model2t_death <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_death
)

model2r_death <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_death
)

model2p_death <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_death
)

m2t_death <- model2t_death %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2r_death <- model2r_death %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

m2p_death <- model2p_death %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ " ",
  )

row1 <- tbl_merge(list(m2t_alc, m2r_alc, m2p_alc))
row2 <- tbl_merge(list(m2t_misreporter, m2r_misreporter, m2p_misreporter))
row3 <- tbl_merge(list(m2t_3_ques_comp, m2r_3_ques_comp, m2p_3_ques_comp))
row4 <- tbl_merge(list(m2t_liver_disease, m2r_liver_disease, m2p_liver_disease))
row5 <- tbl_merge(list(m2t_death, m2r_death, m2p_death))

table_sens <-
  tbl_stack(list(row1, row2, row3, row4, row5),
            group_header = c("Exclusion of partipants with a high alcohol intake:",
                             "Exclusion of participants with a implausible food intake:",
                             "Inclusion criteria set to >= 3 Oxfords WebQs:",
                             "Exclusion of participants with any liver disease before baseline:",
                             "Liver cancer as primary cause of death counts as an event:")) %>%
  modify_caption("**Supplementary table 3. Sensitivity analyses**") %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: grey'> Supplementary table 3. Sensitivity analyses</div>") %>%
  modify_spanning_header(
    update = estimate_1 ~ "**Legumes for total meat**", estimate_2 ~ "**Legumes for red meat**", estimate_3 ~ "**Legumes for processed meat**"
  ) %>%
  modify_header(label = "**15 g/day substitution**") %>%
  modify_footnote(update = everything() ~ NA, abbreviation = TRUE) %>%
  modify_table_styling(
    columns = c(p.value_1, p.value_2, p.value_3),
    hide = TRUE
  )
