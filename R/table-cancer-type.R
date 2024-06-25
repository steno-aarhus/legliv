prepare_table_hcc <- function(data, gt_theme) {

  gtsummary::set_gtsummary_theme(gt_theme)

  model1t_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center),
    data = data
  )

  model2t_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center) +
      education + tdi + spouse +
      exercise + smoking_pack + alcohol_daily +
      wc,
    data = data
  )

  model1r_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + proc_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center),
    data = data
  )

  model2r_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + proc_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center) +
      education + tdi + spouse +
      exercise + smoking_pack + alcohol_daily +
      wc,
    data = data
  )

  model1p_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + red_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center),
    data = data
  )

  model2p_hcc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + red_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center) +
      education + tdi + spouse +
      exercise + smoking_pack + alcohol_daily +
      wc,
    data = data
  )

  m1t_hcc  <- model1t_hcc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Total red meat",
    ) %>% tbl_butcher()

  m1r_hcc  <- model1r_hcc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Unprocessed red meat",
    ) %>% tbl_butcher()

  m1p_hcc  <- model1p_hcc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Processed red meat",
    ) %>% tbl_butcher()

  m2t_hcc  <- model2t_hcc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Total red meat",
    ) %>% tbl_butcher(include = "inputs")

  m2r_hcc  <- model2r_hcc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Unprocessed red meat",
    ) %>% tbl_butcher(include = "inputs")

  m2p_hcc  <- model2p_hcc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Processed red meat",
    ) %>% tbl_butcher(include = "inputs")

  row1 <- tbl_merge(list(m1t_hcc, m2t_hcc)) %>%
    modify_spanning_header(everything() ~ NA_character_) %>% tbl_butcher()
  row2 <- tbl_merge(list(m1r_hcc, m2r_hcc)) %>% tbl_butcher()
  row3 <- tbl_merge(list(m1p_hcc, m2p_hcc)) %>% tbl_butcher()

  return(list(m2t_hcc=m2t_hcc,m2r_hcc=m2r_hcc,m2p_hcc=m2p_hcc,row1=row1,row2=row2,row3=row3))
}

prepare_table_icc <- function(data, gt_theme) {

  gtsummary::set_gtsummary_theme(gt_theme)

  model1t_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center),
    data = data
  )

  model2t_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center) +
      education + tdi + spouse +
      exercise + smoking_pack + alcohol_daily +
      wc,
    data = data
  )

  model1r_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + proc_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center),
    data = data
  )

  model2r_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + proc_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center) +
      education + tdi + spouse +
      exercise + smoking_pack + alcohol_daily +
      wc,
    data = data
  )

  model1p_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + red_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center),
    data = data
  )

  model2p_icc <- coxph(
    Surv(time = status_age, event = status == "Liver cancer") ~
      legume_daily_15 + red_meat_daily_15 +
      updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
      strata(sex, age_strat, ass_center) +
      education + tdi + spouse +
      exercise + smoking_pack + alcohol_daily +
      wc,
    data = data
  )

  m1t_icc  <- model1t_icc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Total red meat",
    ) %>% tbl_butcher()

  m1r_icc  <- model1r_icc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Unprocessed red meat",
    ) %>% tbl_butcher()

  m1p_icc  <- model1p_icc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Processed red meat",
    ) %>% tbl_butcher()

  m2t_icc  <- model2t_icc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Total red meat",
    ) %>% tbl_butcher(include = "inputs")

  m2r_icc  <- model2r_icc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Unprocessed red meat",
    ) %>% tbl_butcher(include = "inputs")

  m2p_icc  <- model2p_icc %>%
    tbl_regression(
      exponentiate = T,
      include = legume_daily_15,
      label = legume_daily_15 ~ "Processed red meat",
    ) %>% tbl_butcher(include = "inputs")

  row4 <- tbl_merge(list(m1t_icc, m2t_icc)) %>% tbl_butcher()
  row5 <- tbl_merge(list(m1r_icc, m2r_icc)) %>% tbl_butcher()
  row6 <- tbl_merge(list(m1p_icc, m2p_icc)) %>% tbl_butcher()

  return(list(m2t_icc=m2t_icc,m2r_icc=m2r_icc,m2p_icc=m2p_icc,row4=row4,row5=row5,row6=row6))
}

create_table_cancer <- function(row1, row2, row3, row4, row5, row6, gt_theme) {
  gtsummary::set_gtsummary_theme(gt_theme)
table_cancer_type <-
  tbl_stack(list(row1, row2, row3, row4, row5, row6),
            group_header = c("Hepatocellular carcinoma (n = 87)", "Hepatocellular carcinoma (n = 87)", "Hepatocellular carcinoma (n = 87)", "Intrahepatic cholangiocarcinoma (n = 100)", "Intrahepatic cholangiocarcinoma (n = 100)", "Intrahepatic cholangiocarcinoma (n = 100)")) %>%
  modify_header(label = "**15 g/day of legumes replacing:**") %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  tbl_butcher() %>%
  as_gt() %>%
  tab_spanner(
    label = md("**Model 1**"),
    columns = c(estimate_1),
    id = "model1"
  ) %>%
  tab_spanner(
    label = md("**Model 2**"),
    columns = c(estimate_2),
    id = "model2"
  ) %>%
  tab_caption(
    md("**Replacing 15 g/day of total meat, red meat and processed meat with legumes and hazard ratios and 95% confidence intervals for hepatocellular carcinoma and intrahepatic cholangiocarcinoma.**")
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "dimgrey", align = "left"),
      cell_borders(sides = c("top","left","right"), style = "hidden")
    ),
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Multivariate Cox proportional hazards regression model adjusted for age (as underlying timescale), other food groups, and total food intake, and additionally stratified on sex, age, and attended assessment centre.",
    locations = cells_column_spanners(spanners = "model1")
  ) %>%
  tab_footnote(
    footnote = "Further adjusted for educational level, Townsend deprivation index, living alone, physical activity, smoking, alcohol intake, and waist circumference.",
    locations = cells_column_spanners(spanners = "model2")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups = everything())
  )
return(table_cancer_type)
}
