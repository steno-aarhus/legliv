model1t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    other_foods_daily + total_weight_food_daily +
    sex,
  data = data
)

m1t <- model1t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Legume category",
  ) %>%
  modify_caption("**No intake of legumes vs. quartiles of daily legume intake** (N = {N})")


model2t <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_category + red_meat_daily + proc_meat_daily +
    other_foods_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking + alcohol_daily +
    gall_disease + met_synd,
  data = data
)

m2t <- model2t %>%
  tbl_regression(
    exponentiate = T,
    include = legume_category,
    label = legume_category ~ "Legume category",
  ) %>%
  modify_caption("**Full cohort** (N = {N})")

tbl_stack <-
  tbl_stack(list(m1t, m2t), group_header = c("Model 1", "Model 2")) %>%
  modify_header(label = "**15 g/day substitution**")
tbl_stack
data_legume_quintile <- data %>%
  select(legume_category, legume_daily)

mean_legume <- data_legume_quintile %>%
  group_by(legume_category) %>%
  summarise(mean_legume_daily = mean(legume_daily, na.rm = TRUE))

tbl_mean_legume <- mean_legume %>%
  tbl_summary()
tbl_category <- data_legume_quintile %>%
  select(-legume_daily) %>%
  tbl_summary()
tbl_mean_legume
tbl_category
tbl_merge_sum <- tbl_merge(
  list(tbl_category, tbl_mean_legume)
)
tbl_merge_sum

tbl_merge <- tbl_merge(
  tbls = list(m1t, m2t),
  tab_spanner = c("**Model 1**", "**Model 2**")
)
tbl_merge

# tbl_merge %>%
#   as_gt() %>% # convert to gt table
#   gt::gtsave( # save table as image
#     filename = "table-legume_quintile.png", path = "~/legliv/doc/Images"
#   )
