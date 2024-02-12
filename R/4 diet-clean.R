# Average dietary intake of food groups -----------------------------------
calculate_food_intake <- function(diet_data) {
    data <- data %>%
        select(-id) # Or else function wont work
    # estimating average daily and weekly intakes of food groups in g
    diet_data <- data %>%
        #include foods from 24h recalls, number of recalls, and id's
        select(starts_with("p26"), "ques_comp_n") %>%
        # creating food groups from UKB Aurora Perez
        mutate(
            # refined cereals
            cereal_refined_total = rowSums(select(., starts_with("p26113") | starts_with("p26079") |
                                                      starts_with("p26071") | starts_with("p26072") |
                                                      starts_with("p26073") | starts_with("p26075") |
                                                      starts_with("p26068") | starts_with("p26083")), na.rm = TRUE),
            cereal_refined_daily = cereal_refined_total/ques_comp_n,
            cereal_refined_weekly = cereal_refined_daily * 7,
            # whole-grain cereals
            whole_grain_total = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
                                                   starts_with("p26077") | starts_with("p26078") |
                                                   starts_with("p26105") | starts_with("p26114")), na.rm = TRUE),
            whole_grain_daily = whole_grain_total/ques_comp_n,
            whole_grain_weekly = whole_grain_daily * 7,
            # mixed dishes
            mixed_dish_total = rowSums(select(., starts_with("p26128") | starts_with("p26097") |
                                                  starts_with("p26116") | starts_with("p26135") |
                                                  starts_with("p26139")), na.rm = TRUE),
            mixed_dish_daily = mixed_dish_total/ques_comp_n,
            mixed_dish_weekly = mixed_dish_daily * 7,
            # dairy
            dairy_total = rowSums(select(., starts_with("p26154") | starts_with("p26087") |
                                             starts_with("p26096") | starts_with("p26102") |
                                             starts_with("p26103") | starts_with("p26099") |
                                             starts_with("p26131") | starts_with("p26133") |
                                             starts_with("p26150")), na.rm = TRUE),
            dairy_daily = dairy_total/ques_comp_n,
            dairy_weekly = dairy_daily * 7,
            # fats and spread
            fats_total = rowSums(select(., starts_with("p26112") | starts_with("p26062") |
                                            starts_with("p26063") | starts_with("p26155") |
                                            starts_with("p26110") | starts_with("p26111")), na.rm = TRUE),
            fats_daily = fats_total/ques_comp_n,
            fats_weekly = fats_daily * 7,
            # fruit
            fruit_total = rowSums(select(., starts_with("p26089") | starts_with("p26090") |
                                             starts_with("p26091") | starts_with("p26092") |
                                             starts_with("p26093") | starts_with("p26094")), na.rm = TRUE),
            fruit_daily = fruit_total/ques_comp_n,
            fruit_weekly = fruit_daily * 7,
            # nuts and seeds
            nut_total = rowSums(select(., starts_with("p26107") | starts_with("p26108")), na.rm = TRUE),
            nut_daily = nut_total/ques_comp_n,
            nut_weekly = nut_daily*7,
            # vegetables
            veggie_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                              starts_with("p26115") | starts_with("p26123") |
                                              starts_with("p26125") | starts_with("p26143") |
                                              starts_with("p26146") | starts_with("p26147")), na.rm = TRUE) +
                rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + #assuming half hummus half guacamole
                rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE), #assuming half peas half corn
            veggie_daily = veggie_total/ques_comp_n,
            veggie_weekly = veggie_daily * 7,
            # potatoes
            potato_total = rowSums(select(., starts_with("p26118") | starts_with("p26119") |
                                              starts_with("p26120")), na.rm = TRUE),
            potato_daily = potato_total/ques_comp_n,
            potato_weekly = potato_daily * 7,
            # eggs
            egg_total = rowSums(select(., starts_with("p26088")), na.rm = TRUE),
            egg_daily = egg_total/ques_comp_n,
            egg_weekly = egg_daily * 7,
            # meat substitutes
            meat_sub_total = rowSums(select(., starts_with("p26145")), na.rm = TRUE),
            meat_sub_daily = meat_sub_total/ques_comp_n,
            meat_sub_weekly = meat_sub_daily * 7,
            # non-alcoholic beverages
            non_alc_beverage_total = rowSums(select(., starts_with("p26124") | starts_with("p26141") |
                                                        starts_with("p26142") | starts_with("p26148") |
                                                        starts_with("p26081") | starts_with("p26082") |
                                                        starts_with("p26095") | starts_with("p26126") |
                                                        starts_with("p26127")), na.rm = TRUE),
            non_alc_beverage_daily = non_alc_beverage_total/ques_comp_n,
            non_alc_beverages_weekly = non_alc_beverage_daily * 7,
            # alcoholic beverages
            alc_beverage_total = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
                                                    starts_with("p26153") | starts_with("p26067") |
                                                    starts_with("p26138")), na.rm = TRUE),
            alc_beverage_daily = alc_beverage_total/ques_comp_n,
            alc_beverage_weekly = alc_beverage_daily * 7,
            # sugar, preserves, cakes & confectionery, snacks
            snack_total = rowSums(select(., starts_with("p26106") | starts_with("p26140") |
                                             starts_with("p26134") | starts_with("p26084") |
                                             starts_with("p26085") | starts_with("p26064") |
                                             starts_with("p26080")), na.rm = TRUE),
            snack_daily = snack_total/ques_comp_n,
            Snack_weekly = snack_daily * 7,
            # Sauces & condiments
            sauce_total = rowSums(select(., starts_with("p26129") | starts_with("p26130")), na.rm = TRUE),
            sauce_daily = sauce_total/ques_comp_n,
            sauce_weekly = sauce_daily * 7,
            # legumes
            legume_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                              starts_with("p26136") | starts_with("p26137")), na.rm = TRUE) +
            rowSums(select(., starts_with("p26144")) * 0.5, na.rm = TRUE) + #assuming half hummus half guacamole
            rowSums(select(., starts_with("p26115")) * 0.5, na.rm = TRUE), #assuming half peas half corn
            legume_daily = legume_total/ques_comp_n,
            legume_weekly = legume_daily * 7,
            # red meats
            red_meat_total = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
                                                starts_with("p26104") | starts_with("p26117")), na.rm = TRUE),
            red_meat_daily = red_meat_total/ques_comp_n,
            red_meat_weekly = red_meat_daily * 7,
            # processed meat
            proc_meat_total = rowSums(select(., starts_with("p26122")), na.rm = TRUE),
            proc_meat_daily = proc_meat_total/ques_comp_n,
            proc_meat_weekly = proc_meat_daily * 7,
            # poultry
            poultry_total = rowSums(select(., starts_with("p26121") | starts_with("p26069")), na.rm = TRUE),
            poultry_daily = poultry_total/ques_comp_n,
            poultry_weekly = poultry_daily * 7,
            # fish
            fish_total = rowSums(select(., starts_with("p26070") | starts_with("p26109") |
                                            starts_with("p26132") | starts_with("p26149")), na.rm = TRUE),
            fish_daily = fish_total/ques_comp_n,
            fish_weekly = fish_daily * 7,
            # total weight of all foods
            total_weight_food = rowSums(select(., starts_with("p26000")), na.rm = TRUE),
            total_weight_food_daily = total_weight_food/ques_comp_n
        )
    diet_data <- diet_data %>%
        select(-starts_with("p26"), -ends_with("weekly"), -ends_with("total"), -ends_with("_n"))
    return(diet_data)
}

diet_data <- calculate_food_intake(diet_data)

data <- data %>%
    select(-id)

diet_data %>%
    select(proc_meat_daily, legume_daily, red_meat_daily, alc_beverage_daily) %>%
    summary()
