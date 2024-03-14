diet_table <- data %>%
  select(
    total_energy_food_daily, total_weight_food_daily, legume_daily, red_proc_meat_daily,
    red_meat_daily, proc_meat_daily, poultry_daily, fish_daily, dairy_daily, egg_daily,
    cereal_refined_daily, whole_grain_daily, veggie_daily, potato_daily, fruit_daily,
    nut_daily, meat_sub_daily, snack_daily, mixed_dish_daily, sauce_daily, fats_daily,
    non_alc_beverage_daily, alc_beverage_daily, status
  )

diet_table %>%
  tbl_summary(
    by = status,
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(
      total_energy_food_daily ~ "Total energy intake",
      total_weight_food_daily ~ "Total food weight",
      legume_daily ~ "Legumes",
      red_proc_meat_daily ~ "Red and processed meat",
      red_meat_daily ~ "Red meat",
      proc_meat_daily ~ "Processed meat",
      poultry_daily ~ "Poultry",
      fish_daily ~ "Fish",
      dairy_daily ~ "Dairy",
      egg_daily ~ "Eggs",
      cereal_refined_daily ~ "Cereals, refined",
      whole_grain_daily ~ "Cereals, whole grains",
      veggie_daily ~ "Vegetables",
      potato_daily ~ "Potatoes",
      fruit_daily ~ "Fruit",
      nut_daily ~ "Nuts & seeds",
      meat_sub_daily ~ "Meat substitutes, non-soy",
      snack_daily ~ "Snacks",
      mixed_dish_daily ~ "Mixed dishes",
      sauce_daily ~ "Sauces and condiments",
      fats_daily ~ "Fat & spreads",
      non_alc_beverage_daily ~ "Beverages, non-alcoholic",
      alc_beverage_daily ~ "Beverages, alcoholic"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 2. Daily dietary intake.**")
