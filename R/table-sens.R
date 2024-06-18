library(gt)
library(gtsummary)
library(tidyverse)
library(survival)
source(here::here("R","gtsummary-theme.R"))
gtsummary::set_gtsummary_theme(my_theme())

model2t_alc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_alc
)

model2r_alc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_alc
)

model2p_alc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
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
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_misreporter
)

model2r_misreporter <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_misreporter
)

model2p_misreporter <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
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
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_3_ques_comp
)

model2r_3_ques_comp <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_3_ques_comp
)

model2p_3_ques_comp <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
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
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_liver_disease
)

model2r_liver_disease <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_liver_disease
)

model2p_liver_disease <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
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
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_death
)

model2r_death <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_death
)

model2p_death <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
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
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily,
  data = data
)

model2r_nowc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily,
  data = data
)

model2p_nowc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
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
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_any_cancer
)

model2r_any_cancer <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_any_cancer
)

model2p_any_cancer <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
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

model2t_nosoy <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_nosoy
)

model2r_nosoy <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_nosoy
)

model2p_nosoy <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    updi + hpdi + animal_foods + alc_beverage_daily + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data_nosoy
)

m2t_nosoy <- model2t_nosoy %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Total red meat",
  )

m2r_nosoy <- model2r_nosoy %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Unprocessed red meat",
  )

m2p_nosoy <- model2p_nosoy %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Processed red meat",
  )

row1 <- tbl_merge(list(m2t_alc,m2t_misreporter,m2t_liver_disease,m2t_any_cancer,m2t_3_ques_comp,m2t_death,m2t_nowc,m2t_nosoy))
row2 <- tbl_merge(list(m2r_alc,m2r_misreporter,m2r_liver_disease,m2r_any_cancer,m2r_3_ques_comp,m2r_death,m2r_nowc,m2r_nosoy))
row3 <- tbl_merge(list(m2p_alc,m2p_misreporter,m2p_liver_disease,m2p_any_cancer,m2p_3_ques_comp,m2p_death,m2p_nowc,m2p_nosoy))

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
    footnote = "Exclusion of the upper decile of alcohol intake (g/day) by sex. n = 150.",
    locations = cells_column_spanners(spanners = "sens1")
  ) %>%
  tab_spanner(
    label = md("**Implausible food intake**"),
    columns = c(estimate_2),
    id = "sens2"
  ) %>%
  tab_footnote(
    footnote = "Exclusion of participants below the 2.5th percentile and above the 97.5th percentile of energy intake (kJ/day) by sex. n = 164.",
    locations = cells_column_spanners(spanners = "sens2")
  ) %>%
  tab_spanner(
    label = md("**Liver disease before baseline**"),
    columns = c(estimate_3),
    id = "sens3"
  ) %>%
  tab_footnote(
    footnote = "ICD10 codes: K70-79, B16-19, Z94.4, I85, I86.4, and E83.0-1. ICD9 codes: 5710-5745, 0700-0709, V427 and 2750-2751. n = 151.",
    locations = cells_column_spanners(spanners = "sens3")
  ) %>%
  tab_spanner(
    label = md("**Any cancer before baseline**"),
    columns = c(estimate_4),
    id = "sens4"
  ) %>%
  tab_footnote(
    footnote = "ICD10 codes: C00-C97 and D00-D48. ICD9 codes: 1400-2399. n = 129.",
    locations = cells_column_spanners(spanners = "sens4")
  ) %>%
  tab_spanner(
    label = md("**Fewer than 3 Oxford WebQs**"),
    columns = c(estimate_5),
    id = "sens5"
  ) %>%
  tab_footnote(
    footnote = "n = 109.",
    locations = cells_column_spanners(spanners = "sens5")
  ) %>%
  tab_spanner(
    label = md("**Death register as source of liver cancer events**"),
    columns = c(estimate_6),
    id = "sens6"
  ) %>%
  tab_footnote(
    footnote = "n = 183.",
    locations = cells_column_spanners(spanners = "sens6")
  ) %>%
  tab_spanner(
    label = md("**Waist circumference from analysis**"),
    columns = c(estimate_7),
    id = "sens7"
  ) %>%
  tab_footnote(
    footnote = "n = 173.",
    locations = cells_column_spanners(spanners = "sens7")
  ) %>%
  tab_spanner(
    label = md("**Soy milk from food substitutions**"),
    columns = c(estimate_8),
    id = "sens8"
  ) %>%
  tab_footnote(
    footnote = "Soy milk was removed from the legumes food group and moved to the food group healthy plant-based foods. n = 173.",
    locations = cells_column_spanners(spanners = "sens8")
  ) %>%
  tab_caption(
    md("**Sensitivity analyses**")
  ) %>%
  tab_footnote(
    footnote = "All sensitivity analyses were modeled as the fully adjusted models in the main analyses",
    locations = cells_title()
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
  tab_spanner(
    label = md("**Exclusion of:**"),
    columns = c(estimate_7,estimate_8),
    level = 2,
    id = "456"
  ) %>%
  tab_options(table.width = pct(100),
              table.font.size = px(10.666)) %>%
  cols_width(matches("estimate|label") ~ pct(100/9))

# table_sens %>% gtsave("doc/latex-tables/table-sens.tex")
