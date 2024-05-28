library(tidyverse)
library(gt)
source(here::here("R","gtsummary-theme.R"))
gtsummary::set_gtsummary_theme(my_theme)
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
  "Soya-based desserts, Baked beans, pulses, Soya drinks (including calcium fortified),
  Tofu-based products, Hummus, Peas",

  "Beef, Lamb, Other meat including offal, Pork",

  "Sausages, bacon (with and without fat), ham, liver pate",

  "Poultry, fish, dairy, eggs, mixed dishes, and sauces and condiments",

  "Whole grains, fruits, nuts, plant oils, beverages (water, tea and coffee), vegetables",

  "Refined cereals, potatoes, fruit juice, mixed dishes (vegetarian), sweets & snacks, and sugar sweetened beverages",

  "Beer and cider, spirits and other alcoholic drinks, fortified wine, red and rose wine, white wine"
)

includes_df <- data.frame(includes)

df <- data.frame(food_group = food_group_df,
                 includes = includes)

food_group_table <- df %>%
  gt() %>%
  tab_header(
    title = md("**Supplementary table 1. Summary of included foods for each food group.**")
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
  tab_options(table.width = pct(100),
              latex.use.longtable = FALSE) %>%
  cols_width(food_group ~ pct(20),
             includes ~ pct(80)) %>%
  tab_options(
    table.font.size = gt::px(13.33),
    data_row.padding = gt::px(1),
    summary_row.padding = gt::px(1),
    grand_summary_row.padding = gt::px(1),
    footnotes.padding = gt::px(1),
    source_notes.padding = gt::px(1),
    row_group.padding = gt::px(1),
    table.width = pct(100)
  )

# food_group_table %>% gtsave("doc/latex-tables/table-food-group.tex")
