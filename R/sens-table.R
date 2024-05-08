library(gt)
library(gtsummary)
library(tidyverse)
library(survival)

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
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_liver_disease <- model2r_liver_disease %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_liver_disease <- model2p_liver_disease %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
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
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_death <- model2r_death %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_death <- model2p_death %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

model2t_nowc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily,
  data = data
)

model2r_nowc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily,
  data = data
)

model2p_nowc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily,
  data = data
)

m2t_nowc <- model2t_nowc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for total meat",
  )

m2r_nowc <- model2r_nowc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for red meat",
  )

m2p_nowc <- model2p_nowc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Legumes for processed meat",
  )

row1 <- tbl_merge(list(m2t_alc,m2t_misreporter,m2t_liver_disease,m2t_3_ques_comp,m2t_death,m2t_nowc))
row2 <- tbl_merge(list(m2r_alc,m2r_misreporter,m2r_liver_disease,m2r_3_ques_comp,m2r_death,m2r_nowc))
row3 <- tbl_merge(list(m2p_alc,m2p_misreporter,m2p_liver_disease,m2p_3_ques_comp,m2p_death,m2p_nowc))

table_sens <-
  tbl_stack(list(row1, row2, row3)) %>%
  modify_header(label = "**15 g/day substitution**") %>%
  modify_spanning_header(everything() ~ NA_character_) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  modify_table_styling(
    column = c(p.value_1,p.value_2,p.value_3,p.value_4,p.value_5,p.value_6),
    hide = TRUE
  ) %>%
  as_gt() %>%
  tab_spanner(
    label = md("**High alcohol intake**"),
    columns = c(estimate_1),
    id = "sens1"
  ) %>%
  tab_spanner(
    label = md("**Implausible food intake**"),
    columns = c(estimate_2),
    id = "sens2"
  ) %>%
  tab_spanner(
    label = md("**Fewer than 3 Oxford WebQs**"),
    columns = c(estimate_4),
    id = "sens3"
  ) %>%
  tab_spanner(
    label = md("**Liver disease before baseline**"),
    columns = c(estimate_3),
    id = "sens4"
  ) %>%
  tab_spanner(
    label = md("**Death register as source for liver cancer events**"),
    columns = c(estimate_5),
    id = "sens5"
  ) %>%
  tab_spanner(
    label = md("**Exclusion of waist circumference from analysis**"),
    columns = c(estimate_6),
    id = "sens6"
  ) %>%
  tab_spanner(
    label = md("**Supplementary table 3. Sensitivity analyses**"),
    columns = everything(),
    level = 3,
    id = "title"
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "dimgrey", align = "left"),
      cell_borders(sides = c("top","left","right"), style = "hidden")
    ),
    locations = cells_column_spanners(spanners = "title")
  ) %>%
  tab_spanner(
    label = md("**Exclusion of participants with:**"),
    columns = c(estimate_1,estimate_2, estimate_3, estimate_4),
    level = 2,
    id = "123"
  )
