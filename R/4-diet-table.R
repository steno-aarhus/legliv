library(gt)
library(gtsummary)
library(tidyverse)

diet_all_mean <- data %>%
  select(
    total_energy_food_daily, total_weight_food_daily
  )

diet_all_median <- data %>%
  select(
    legume_daily, total_meat_daily,
    red_meat_daily, proc_meat_daily, animal_foods, hpdi, updi, alc_beverage_daily
  )

diet_liver_mean <- data %>%
  filter(status == "Liver cancer") %>%
  select(
    total_energy_food_daily, total_weight_food_daily
  )

diet_liver_median <- data %>%
  filter(status == "Liver cancer") %>%
  select(
    legume_daily, total_meat_daily,
    red_meat_daily, proc_meat_daily, animal_foods, hpdi, updi, alc_beverage_daily
  )

diet_all_table_mean <- diet_all_mean %>%
  tbl_summary(
    label = list(
      total_energy_food_daily ~ "Energy, kJ",
      total_weight_food_daily ~ "Weight, g"
    )
  )

diet_all_table_median <- diet_all_median %>%
  tbl_summary(
    label = list(
      legume_daily ~ "Legumes",
      total_meat_daily ~ "Red and processed meat",
      red_meat_daily ~ "Red meat",
      proc_meat_daily ~ "Processed meat",
      animal_foods ~ "Other animal-based foods",
      hpdi ~ "Healthy plant-based foods",
      updi ~ "Unhealthy plant-based foods",
      alc_beverage_daily ~ "Alcoholic beverages"
    )
  )

diet_liver_table_mean <- diet_liver_mean %>%
  tbl_summary(
    label = list(
      total_energy_food_daily ~ "Energy, kJ",
      total_weight_food_daily ~ "Weight, g"
    )
  )

diet_liver_table_median <- diet_liver_median %>%
  tbl_summary(
    label = list(
      legume_daily ~ "Legumes",
      total_meat_daily ~ "Red and processed meat",
      red_meat_daily ~ "Red meat",
      proc_meat_daily ~ "Processed meat",
      animal_foods ~ "Other animal-based foods",
      hpdi ~ "Healthy plant-based foods",
      updi ~ "Unhealthy plant-based foods",
      alc_beverage_daily ~ "Alcoholic beverages"
    )
  )

row1 <- tbl_merge(list(diet_all_table_mean, diet_liver_table_mean), tab_spanner = c("**Cohort**", "**Liver cancer**")) %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR)")
row2 <- tbl_merge(list(diet_all_table_median, diet_liver_table_median)) %>%
  bold_labels()

diet_table <-
  tbl_stack(list(row1, row2),
            quiet = TRUE,
            group_header = c("Total food intake", "Food groups, g/day")) %>%
  modify_header(label ~ "**Daily food intake**") %>%
  modify_caption("**Table 2. Daily dietary intake of food groups, total food and total energy intake in UK Biobank participants who completed ≥ 2 Oxford WebQ 24-hour diet recall.**") %>%
  modify_table_styling(
    columns = label,
    rows = label == "Other animal-based foods",
    footnote = "Other animal-based foods include: poultry, fish, dairy, eggs, and mixed dishes with animal products."
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Healthy plant-based foods",
    footnote = "Healthy plant-based foods include: whole grains, vegetables, fruits, nuts, plant oils, and beverages (coffee, tea, water)."
  ) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Unhealthy plant-based foods",
    footnote = "Unhealthy plant-based foods includes: refined grains, potatoes, mixed vegetarian dishes, sweets and snacks, fruit juice, and sugar sweetened beverages."
  ) %>%
  as_gt()

diet_table
