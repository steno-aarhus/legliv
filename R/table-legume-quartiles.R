library(gt)
library(gtsummary)
library(tidyverse)
library(survival)
source(here::here("R","gtsummary-theme.R"))
gtsummary::set_gtsummary_theme(my_theme)

fn_add_mean <- function(data, variable, ...) {
  data %>%
    dplyr::group_by(.data[[variable]]) %>%
    dplyr::arrange(.data[[variable]]) %>%
    dplyr::summarise(legume_daily = mean(legume_daily, na.rm = TRUE)) %>%
    select(legume_daily) %>%
    mutate(legume_daily = style_sigfig(legume_daily))
}
fn_add_mean(data, "legume_category")

tbl_legume_mean <-
  data %>%
  select(legume_daily, legume_category) %>%
  tbl_summary(
    include  = -legume_daily,
    label = legume_category ~ "Categories:",
    type = everything() ~ "categorical"
  ) %>%
  add_stat(
    all_categorical() ~ fn_add_mean,
    location = all_categorical() ~  "level"
  ) %>%
  modify_header(legume_daily ~ "**Mean daily legume intake**") %>%
  modify_table_styling(columns = stat_0,
                       hide = TRUE)

model1t_leg <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    animal_foods + hpdi + updi + total_weight_food_daily +
    strata(sex, age_strat, ass_center),
  data = data
)

m1t_leg <- model1t_leg %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Categories:",
  ) %>% bold_p(t = 0.05)

model2t_leg <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    animal_foods + hpdi + updi + total_weight_food_daily +
    strata(sex, age_strat, ass_center) +
    education + tdi + spouse +
    exercise + smoking_pack + alcohol_daily +
    wc,
  data = data
)

m2t_leg <- model2t_leg %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Categories:",
  ) %>% bold_p(t = 0.05)

table_legume <- tbl_merge(
  tbls = list(tbl_legume_mean,m1t_leg, m2t_leg)
) %>%
  modify_spanning_header(everything() ~ NA_character_) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = T) %>%
  as_gt() %>%
  tab_spanner(
    label = md("**Model 1**"),
    columns = c(estimate_2),
    id = "model1"
  ) %>%
  tab_spanner(
    label = md("**Model 2**"),
    columns = c(estimate_3),
    id = "model2"
  ) %>%
  tab_caption(
    md("**No intake of legumes vs. quartiles of daily legume intake and hazard ratios and 95% confidence intervals for primary liver cancer.**")
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "dimgrey", align = "left"),
      cell_borders(sides = c("top","left","right"), style = "hidden")
    ),
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "Multivariate Cox proportional hazards regression model adjusted for age (as underlying timescale), other food groups, and total food intake.",
    locations = cells_column_spanners(spanners = "model1")
  ) %>%
  tab_footnote(
    footnote = "Further adjusted for sex, educational level, Townsend deprivation index, living alone, physical activity, smoking, alcohol intake, and waist circumference.",
    locations = cells_column_spanners(spanners = "model2")
  )
