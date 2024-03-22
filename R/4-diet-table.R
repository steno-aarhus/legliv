library(gtsummary)
diet_all <- data %>%
  select(
    total_energy_food_daily, total_weight_food_daily, legume_daily, total_meat_daily,
    red_meat_daily, proc_meat_daily, poultry_daily, fish_daily, dairy_daily, egg_daily,
    cereal_refined_daily, whole_grain_daily, veggie_daily, fruit_daily,
    nut_daily, snack_daily, mixed_dish_daily, fats_daily,
    non_alc_beverage_daily, alc_beverage_daily
  )

diet_liver <- data %>%
  filter(status == "Liver cancer") %>%
  select(
    total_energy_food_daily, total_weight_food_daily, legume_daily, total_meat_daily,
    red_meat_daily, proc_meat_daily, poultry_daily, fish_daily, dairy_daily, egg_daily,
    cereal_refined_daily, whole_grain_daily, veggie_daily, fruit_daily,
    nut_daily, snack_daily, mixed_dish_daily, fats_daily,
    non_alc_beverage_daily, alc_beverage_daily
  )

diet_all_table <- diet_all %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(
      total_energy_food_daily ~ "Total energy intake",
      total_weight_food_daily ~ "Total food weight",
      legume_daily ~ "Legumes",
      total_meat_daily ~ "Red and processed meat",
      red_meat_daily ~ "Red meat",
      proc_meat_daily ~ "Processed meat",
      poultry_daily ~ "Poultry",
      fish_daily ~ "Fish",
      dairy_daily ~ "Dairy",
      egg_daily ~ "Eggs",
      cereal_refined_daily ~ "Cereals, refined",
      whole_grain_daily ~ "Cereals, whole grains",
      veggie_daily ~ "Vegetables",
      fruit_daily ~ "Fruit",
      nut_daily ~ "Nuts & seeds",
      snack_daily ~ "Snacks",
      mixed_dish_daily ~ "Mixed dishes",
      fats_daily ~ "Fat & spreads",
      non_alc_beverage_daily ~ "Beverages, non-alcoholic",
      alc_beverage_daily ~ "Beverages, alcoholic"
    )
  )

diet_liver_table <- diet_liver %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(
      total_energy_food_daily ~ "Total energy intake",
      total_weight_food_daily ~ "Total food weight",
      legume_daily ~ "Legumes",
      total_meat_daily ~ "Red and processed meat",
      red_meat_daily ~ "Red meat",
      proc_meat_daily ~ "Processed meat",
      poultry_daily ~ "Poultry",
      fish_daily ~ "Fish",
      dairy_daily ~ "Dairy",
      egg_daily ~ "Eggs",
      cereal_refined_daily ~ "Cereals, refined",
      whole_grain_daily ~ "Cereals, whole grains",
      veggie_daily ~ "Vegetables",
      fruit_daily ~ "Fruit",
      nut_daily ~ "Nuts & seeds",
      snack_daily ~ "Snacks",
      mixed_dish_daily ~ "Mixed dishes",
      fats_daily ~ "Fat & spreads",
      non_alc_beverage_daily ~ "Beverages, non-alcoholic",
      alc_beverage_daily ~ "Beverages, alcoholic"
    )
  )

diet_combined_table <- tbl_merge(
  tbls = list(diet_all_table, diet_liver_table),
  tab_spanner = c("**Cohort**", "**Liver cancer**")) %>%
  bold_labels() %>%
  modify_caption("**Table 2. Daily dietary intake.**")

diet_combined_table %>%
  as_gt() %>% # convert to gt table
  gt::gtsave( # save table as image
    filename = "table-diet.png", path = "~/legliv/doc/Images"
  )
