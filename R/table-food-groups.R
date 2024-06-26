
create_table_food_groups_df <- function(gt_theme) {

  gtsummary::set_gtsummary_theme(gt_theme)

  id <- data.frame(id = 1:25)

  food_group <- c(
    "Legumes",
    "Red meat",
    "Processed meat",
    "Animal-based foods",
    "Healthy plant-based foods",
    "Unhealthy plant-based foods",
    "Alcoholic beverages"
  )
  food_group_df <- data.frame(food_group)

  includes <- c(
    "Soy-based desserts, baked beans, pulses, soy drinks (including calcium fortified),
      tofu-based products, hummus, peas",

    "Beef, lamb, other meat including offal, pork",

    "Sausages, bacon (with and without fat), ham, liver pate",

    "Poultry, fish, dairy, eggs, mixed dishes, and sauces and condiments",

    "Whole grains, fruits, nuts, plant oils, beverages (water, tea and coffee), vegetables",

    "Refined cereals, potatoes, fruit juice, mixed dishes (vegetarian), sweets & snacks, and sugar sweetened beverages",

    "Beer and cider, spirits and other alcoholic drinks, fortified wine, red and rose wine, white wine"
  )

  includes_df <- data.frame(includes)

  df <- data.frame(food_group = food_group_df,
                   includes = includes)
  return(df = df)
}

create_table_food_groups <- function(df, gt_theme) {

  gtsummary::set_gtsummary_theme(gt_theme)

  table_food_groups <- df %>%
    gt() %>%
    tab_caption(
      md("**Summary of included foods for each food group.**")
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "dimgrey", align = "left"),
        cell_borders(sides = c("top","left","right"), style = "hidden")
      ),
      locations = cells_title()
    ) %>%
    cols_label(
      food_group = md("**Food group**"),
      includes = md("**Includes**")
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = food_group
      )
    ) %>%
    tab_options(table.width = pct(100)) %>%
    cols_width(food_group ~ pct(20),
               includes ~ pct(80)) %>%
    tab_options(
      table.font.size = gt::px(12),
      data_row.padding = gt::px(1),
      summary_row.padding = gt::px(1),
      grand_summary_row.padding = gt::px(1),
      footnotes.padding = gt::px(1),
      source_notes.padding = gt::px(1),
      row_group.padding = gt::px(1),
      table.width = pct(100))

  return(table_food_groups = table_food_groups)
}
