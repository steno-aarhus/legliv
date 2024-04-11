library(gtsummary)
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
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(
      total_energy_food_daily ~ "Total energy intake",
      total_weight_food_daily ~ "Total food intake"
    )
  )

diet_all_table_median <- diet_all_median %>%
  tbl_summary(
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
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
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(
      total_energy_food_daily ~ "Total energy intake",
      total_weight_food_daily ~ "Total food intake"
    )
  )

diet_liver_table_median <- diet_liver_median %>%
  tbl_summary(
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
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
  modify_caption("**Table 2. Daily dietary intake.**") %>%
  modify_footnote(all_stat_cols() ~ "mean (±SD) for total energy and food intake; median (IQR) for food groups")
row2 <- tbl_merge(list(diet_all_table_median, diet_liver_table_median)) %>%
  bold_labels()
diet_table <-
  tbl_stack(list(row1, row2))
diet_table %>% as_gt()

# diet_table %>%
#   as_gt() %>% # convert to gt table
#   gt::gtsave( # save table as image
#     filename = "table-diet.png", path = "~/legliv/doc/Images"
#   )
