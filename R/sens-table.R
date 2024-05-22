library(gt)
library(gtsummary)
library(tidyverse)
library(survival)
source(here::here("R","gtsummary-theme.R"))
gtsummary::set_gtsummary_theme(my_theme)

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
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_alc <- model2r_alc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_alc <- model2p_alc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
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
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_misreporter <- model2r_misreporter %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_misreporter <- model2p_misreporter %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
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
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_3_ques_comp <- model2r_3_ques_comp %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_3_ques_comp <- model2p_3_ques_comp %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
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
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_liver_disease <- model2r_liver_disease %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_liver_disease <- model2p_liver_disease %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
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
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_death <- model2r_death %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_death <- model2p_death %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
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
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_nowc <- model2r_nowc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_nowc <- model2p_nowc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
  )

model2t_any_cancer <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_any_cancer
)

model2r_any_cancer <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_any_cancer
)

model2p_any_cancer <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_any_cancer
)

m2t_any_cancer <- model2t_any_cancer %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_any_cancer <- model2r_any_cancer %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_any_cancer <- model2p_any_cancer %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
  )

row1 <- tbl_merge(list(m2t_alc,m2t_misreporter,m2t_liver_disease,m2t_any_cancer,m2t_3_ques_comp,m2t_death,m2t_nowc))
row2 <- tbl_merge(list(m2r_alc,m2r_misreporter,m2r_liver_disease,m2r_any_cancer,m2r_3_ques_comp,m2r_death,m2r_nowc))
row3 <- tbl_merge(list(m2p_alc,m2p_misreporter,m2p_liver_disease,m2p_any_cancer,m2p_3_ques_comp,m2p_death,m2p_nowc))

table_sens <-
  tbl_stack(list(row1, row2, row3)) %>%
  modify_header(label = "**15 g/day of legumes replacing:**") %>%
  modify_spanning_header(everything() ~ NA_character_) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  as_gt() %>%
  tab_spanner(
    label = md("**High alcohol intake**"),
    columns = c(estimate_1),
    id = "sens1"
  ) %>%
  tab_footnote(
    footnote = "Exclusion of the upper 10th percentile of daily alcohol intake in grams for each sex.",
    locations = cells_column_spanners(spanners = "sens1")
  ) %>%
  tab_spanner(
    label = md("**Implausible food intake**"),
    columns = c(estimate_2),
    id = "sens2"
  ) %>%
  tab_footnote(
    footnote = "Exclusion of the upper and lower decile of daily energy intake for each sex.",
    locations = cells_column_spanners(spanners = "sens2")
  ) %>%
  tab_spanner(
    label = md("**Liver disease before baseline**"),
    columns = c(estimate_3),
    id = "sens3"
  ) %>%
  tab_footnote(
    footnote = "ICD10 codes: K70-79, B16-19, Z94.4, I82.0, I85, I86.4, E83.0-1 and E88. ICD9 codes: 571-574, 070, V427 and 2750-2751.",
    locations = cells_column_spanners(spanners = "sens3")
  ) %>%
  tab_spanner(
    label = md("**Any cancer before baseline**"),
    columns = c(estimate_4),
    id = "sens4"
  ) %>%
  tab_footnote(
    footnote = "ICD10 codes: C00-C97 and D00-D48. ICD9 codes: 140-239.",
    locations = cells_column_spanners(spanners = "sens4")
  ) %>%
  tab_spanner(
    label = md("**Fewer than 3 Oxford WebQs**"),
    columns = c(estimate_5),
    id = "sens5"
  ) %>%
  tab_spanner(
    label = md("**Death register as source of liver cancer events**"),
    columns = c(estimate_6),
    id = "sens6"
  ) %>%
  tab_spanner(
    label = md("**Exclusion of waist circumference from analysis**"),
    columns = c(estimate_7),
    id = "sens7"
  ) %>%
  tab_header(
    title = md("**Supplementary table 4. Sensitivity analyses**")
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "dimgrey", align = "left"),
      cell_borders(sides = c("top","left","right"), style = "hidden")
    ),
    locations = cells_title()
  ) %>%
  tab_spanner(
    label = md("**Exclusion of participants with:**"),
    columns = c(estimate_1,estimate_2,estimate_3,estimate_4,estimate_5),
    level = 2,
    id = "123"
  ) %>%
  tab_options(table.width = pct(100),
              table.font.size = px(10)) %>%
  cols_width(matches("estimate|label") ~ pct(10))
