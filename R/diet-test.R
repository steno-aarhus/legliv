
calculate_total <- function(data) {
  sum(dplyr::c_across(data), na.rm = TRUE)
}

calculate_daily <- function(data) {
  data |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_total"), list(daily = ~ .x / p20077))) |>
    dplyr::rename_with(~ stringr::str_replace("_total_daily", "_daily"))
}

calculate_food_intake <- function(data) {
  # estimating average daily and weekly intakes of food groups in g
  data <- data %>%
    # creating food groups from UKB Aurora Perez
    rowwise() |>
    mutate(
      legume_total = calculate_total(matches("p26101|p26115|p26124|p26136|p26137|p26144|p26145")),
      meat_total = calculate_total(matches("p26066|p26100|p26104|p26117|p26122|p26069|p26121")),
      fish_total = calculate_total(matches("p26070|p26109|p26132|p26149")),
      dairy_total = calculate_total(matches("p26084|p26087|p26096|p26099|p26102|p26103|p26131|p26133|p26150|p26154")),
      egg_total = calculate_total(matches("p26088")),
      grains_whole_total = calculate_total(matches("p26071|p26074|p26075|p26076|p26077|p26078|p26105|p26114")),
      grains_refined_total = calculate_total(matches("p26068|p26072|p26073|p26079|p26083|p26085|p26097|p26113")),
      veggie_total = calculate_total(matches("p26065|p26098|p26123|p26125|p26143|p26146|p26147|p26115|p26144")),
      potato_total = calculate_total(matches("p26118|p26119|p26120")),
      fruit_total = calculate_total(matches("p26089|p26090|p26091|p26092|p26093|p26094")),
      nut_total = calculate_total(matches("p26107|p26108")),
      fat_plant_total = calculate_total(matches("p26110|p26111|p26112")),
      misc_total = calculate_total(matches("p26116|p26128|p26135|p26139")),
      sweet_total = calculate_total(matches("p26064|p2680|p2686|p26106|p26140")),
      juice_total = calculate_total(matches("p26095")),
      fat_animal_total = calculate_total(matches("p26062|p26063")),
      tea_cof_total = calculate_total(matches("p26081|p26082|p26141|p26142")),
      sugar_drink_total = calculate_total(matches("p26126|p26127"))
    ) |>
    calculate_daily() |>
    ungroup()
  return(diet_data)
}

data <- data %>%
  calculate_food_intake()


# test
library(tidyverse)
targets::tar_config_set(store = here::here("_targets"))
data <- targets::tar_read(data_with_covariates)

sampled_data <- data %>%
  sample_n(1000, replace = FALSE)

data <- data |>
  mutate(
    hummus_guac = rowSums(pick(matches("p26144")), na.rm = TRUE) / 2,
    peas_corn = rowSums(pick(matches("p26115")), na.rm = TRUE) / 2,
    legume_daily = rowSums(pick(matches("p26086|p26101|p26136|p26137|hummus_guac|peas_corn")), na.rm = TRUE) / p20077,
    legume_daily_15 = legume_daily / 15,
    red_meat_daily = rowSums(pick(matches("p26066|p26100|p26104|p26117")), na.rm = TRUE) / p20077,
    red_meat_daily_15 = red_meat_daily / 15,
    proc_meat_daily = rowSums(pick(matches("p26122")), na.rm = TRUE) / p20077,
    proc_meat_daily_15 = proc_meat_daily / 15,
    animal_foods = rowSums(pick(matches(
      "p26069|p26121|
      p26070|p26109|p26132|p26149|
      p26062|p26063|p26084|p26087|p26096|p26099|p26102|p26103|p26131|p26133|p26150|p26154|
      p26088|
      p26129|p26130|
      p26116|p26135|p26139|p26134"
      )), na.rm = TRUE) / p20077,
    hpdi = rowSums(pick(matches(
      "p26071|p26074|p26075|p26076|p26077|p26078|p26105|p26114|
      p26089|p26090|p26091|p26092|p26093|p26094|
      p26106|p26107|p26108|
      p26110|p26111|p26112|
      p26081|p26082|p26141|p26142|p26148|
      p26065|p26098|p26123|p26125|p26143|p26146|p26147|
      hummus_guac|peas_corn"
      )), na.rm = TRUE) / p20077,
    updi = rowSums(pick(matches(
      "p26068|p26072|p26073|p26079|p26083|p26113|
      p26118|p26119|p26120|
      p26095|
      p26097|p26128|p26145|
      p26064|p26080|p26085|p26140|
      p26124|p26126|p26127
      "
      )), na.rm = TRUE) / p20077,
    alc_beverage_daily = rowSums(pick(matches(
      "p26067|p26138|p26151|p26152|p26153"
      )), na.rm = TRUE) / p20077,
    alcohol_daily = rowSums(pick(matches("p26030")), na.rm = TRUE) / p20077,
    total_weight_food_daily = legume_daily + red_meat_daily + proc_meat_daily +
      animal_foods + hpdi + updi + alc_beverage_daily
  )
