library(tidyverse)
library(gt)

id <- data.frame(id = 1:25)

food_group <- c(
  "Legumes", "Red meat", "Processed meat",
  "Animal-based foods", "Poultry", "Fish", "Dairy", "Eggs", "Sauces", "Mixed dishes", "Healthy plant-based foods",
  "Whole grains", "Fruits", "Nuts", "Plant oils", "Beverages", "Vegetables",
  "Unhealthy plant-based foods", "Refined cereals",
  "Potatoes", "Fruit juice", "Mixed dishes, vegetarian", "Sweets & snacks",
  "Sugar sweetened beverages", "Alcoholic beverages"
)
food_group_df <- data.frame(food_group)

includes <- c(
  "Soya-based desserts, Baked beans, pulses, Soya drinks (including calcium fortified),
  Tofu-based products, Hummus, Peas",

  "Beef, Lamb, Other meat including offal, Pork",

  "Sausages, bacon (with and without fat), ham, liver pate",

  " ",

  "Fried poultry with batter/breadcrumbs, Poultry (with/without skin)",

  "Fried fish with batter/breadcrumbs, Oily fish, including salmon, Prawns, lobster, crab, shellfish,
  Tinned tuna, white fish, other fish",

  "Spreadable/lower fat butter, dairy-based very low fat spread, Spreadable normal fat butter, dairy-based normal fat spread (including cholesterol lowering spread),
  Ice cream, milk puddings, milk-based desserts, cheesecake, Dairy-based smoothies, milk-based drinks, hot chocolate,
  Whole milk yogurt (plain), Cheese >17.5 g fat per 100 g, including hard cheese, soft cheese, spreadable, Blue, Feta, Mozzarella, Goats, other),
  Fat free and lower fat yogurt, plain or flavoured, Cheese <=17.5g fat per 100 g, including hard and spreadable lower fat cheese, Cottage,
  Semi-skimmed milk >1 g fat per 100 g (cow, other), Skimmed milk <1 g fat per 100 g (cow, cholesterol lowering, powdered),
  Whole milk >3.6 g fat per 100 g (cow, goat, sheep), Cream (cow's milk)",

  "Whole eggs and processed (omelette, scotch eggs, other)",

  "Mayonnaise, salad dressing, pesto, cheese sauce, white sauce, gravy, Yeast, chutney, olives, ketchup, brown sauce, tomato sauce",

  "Pizza (including gluten free crust), Crisps, savoury biscuits, cheese snacks, other savoury biscuits,
  Soups, homemade, powdered and canned, Sushi",

  " ",

  "Mixed, brown or seeded bread, sliced, baguette, bap, roll, Wholemeal bread, sliced, baguette, bap, roll,
  Wholewheat biscuit cereal, Bran cereal, Porridge oats (including milk/dried fruit added),
  Oatcrunch breakfast cereal, Muesli (with or without dried fruit), Brown and wholemeal pasta and rice",

  "Apples and pears, Blackberries, strawberries, blueberries, raspberries, cherries, Grapefruit, orange, satsuma,
  Dried fruit, prunes, Bananas, mixed fruit, grapes, mango, melon, peach, pineapple, kiwi, other, Stewed fruit, plums",

  "Peanut-butter and chocolate-based spread, Unsalted peanuts and nuts, Salted peanuts and nuts",

  "Olive oil, Olive oil based lower fat spread, plant-based lower fat margarine and soya-based lower fat spread (including cholesterol lowering spread),
  Olive oil based spread, plant-based soft or hard margarine and soya-based spread (including cholesterol lowering spread)",

  "Normal instant, filter, cappuccino, espresso coffee, Decaffeinated instant, filter, cappuccino, espresso coffee,
  Black, green and other tea, Decaffeinated black, herbal tea, rooibos, Plain water, sparkling water",

  "Garlic, leek, onion, Broccoli, cabbage, kale, cauliflower, spinach, sprouts, Mixed side salad, lettuce, watercress,
  Beetroot, carrots, celery, parsnip, turnip, Fresh and tinned tomatoes,
  Mushrooms, mixed vegetables, avocado, broad beans, green beans, butternut squash, courgettes, peppers, other,
  Coleslaw, salad with added fat/mayonnaise, guacamole, sweetcorn",

  " ",

  "Chocolate biscuits, plain biscuits, sweet biscuits and cookies, Naan, garlic bread, other bread (including gluten free),
  White bread, sliced, baguette, bap, roll, Oatcrunch breakfast cereal, Crisps, savoury biscuits, cheese snacks, other savoury biscuits,
  White pasta, rice, couscous, gluten free pasta",

  "Potatoes, sweet potatoes, boiled or baked, Potatoes and chips, fried or roasted with fat, Potatoes, mashed",

  "Orange, grapefruit drink and 100% fruit juice",

  "Double and single crust pies, crumble pies, Yorkshire pudding, snackpot noodles,
  Indian samosa, pakora snacks, Quorn-based and vegetarian burgers and products",

  "Table sugar, honey, jam and preserves,
  Chocolate bar (including white, milk and dark chocolate), chocolate-covered raisins, chocolate-covered sweets,
  Pancakes, croissant, Danish pastries, scones, fruitcakes, cakes, doughnuts, sponge puddings, other desserts, cereal bars, sweet snacks,
  Hard and soft sweets (including sugar free)",

  "Rice and oat vegetable drinks, Low calorie fizzy drinks and squash, Fizzy sugary drinks, squash, fruit smoothies",

  "Beer and cider, Spirits and other alcoholic drinks, Fortified wine, Red and rose wine, White wine"
)

includes_df <- data.frame(includes)

df <- data.frame(food_group = food_group_df,
                 includes = includes)

bold_text <- c(1, 2, 3, 4, 11, 18, 25)
indent_text <- c(5, 6)
food_group_table <- df %>%
  gt() %>%
  tab_spanner(
    label = md("**Supplementary table 1. Summary of included foods for each food group.**"),
    columns = everything(),
    level = 2,
    id = "title"
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "dimgrey", align = "left"),
      cell_borders(sides = c("top","left","right"), style = "hidden")
    ),
    locations = cells_column_spanners(spanners = "title")
  ) %>%
  cols_label(
    food_group = md("**Food group**"),
    includes = md("**Includes**")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = food_group,
      rows = bold_text
    )
  )
