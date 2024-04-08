library(survival)
library(gtsummary)

model1t_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    other_foods_daily + total_weight_food_daily,
  data = data_hcc
)

model2t_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    other_foods_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking + alcohol_daily +
    wc,
  data = data_hcc
)

model1r_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    other_foods_daily + total_weight_food_daily,
  data = data_hcc
)

model2r_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    other_foods_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking + alcohol_daily +
    wc,
  data = data_hcc
)

model1p_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    other_foods_daily + total_weight_food_daily,
  data = data_hcc
)


model2p_hcc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    other_foods_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking + alcohol_daily +
    wc,
  data = data_hcc
)


model1t_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    other_foods_daily + total_weight_food_daily,
  data = data_icc
)

model2t_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 +
    other_foods_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking + alcohol_daily +
    wc,
  data = data_icc
)

model1r_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    other_foods_daily + total_weight_food_daily,
  data = data_icc
)

model2r_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + proc_meat_daily_15 +
    other_foods_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking + alcohol_daily +
    wc,
  data = data_icc
)

model1p_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    other_foods_daily + total_weight_food_daily,
  data = data_icc
)

model2p_icc <- coxph(
  Surv(time = status_age, event = status == "Liver cancer") ~
    legume_daily_15 + red_meat_daily_15 +
    other_foods_daily + total_weight_food_daily +
    sex +
    education + tdi + spouse +
    exercise + smoking + alcohol_daily +
    wc,
  data = data_icc
)

m1t_hcc <- model1t_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 1",
  ) %>%
  modify_caption("**Stratified on type of liver cancer** (N = {N})")

m2t_hcc <- model2t_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 2",
  )

m1r_hcc <- model1r_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 1",
  )

m2r_hcc <- model2r_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 2",
  )

m1p_hcc <- model1p_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 1",
  )

m2p_hcc <- model2p_hcc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 2",
  )

m1t_icc <- model1t_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 1",
  )

m2t_icc <- model2t_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 2",
  )

m1r_icc <- model1r_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 1",
  ) %>%
  bold_p(t = 0.05)

m2r_icc <- model2r_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 2",
  )

m1p_icc <- model1p_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 1",
  )

m2p_icc <- model2p_icc %>%
  tbl_regression(
    exponentiate = T,
    include = legume_daily_15,
    label = legume_daily_15 ~ "Model 2",
  )

row1 <- tbl_merge(list(m1t_hcc, m1r_hcc, m1p_hcc), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"))
row2 <- tbl_merge(list(m2t_hcc, m2r_hcc, m2p_hcc))
row3 <- tbl_merge(list(m1t_icc, m1r_icc, m1p_icc), tab_spanner = c("Legumes for total meat", "Legumes for red meat", "Legumes for processed meat"))
row4 <- tbl_merge(list(m2t_icc, m2r_icc, m2p_icc))

table_cancer_type <-
  tbl_stack(list(row1, row2, row3, row4), group_header = c("Hepatocellular carcinoma", "Hepatocellular carcinoma", "Intrahepatic cholangiocarcinoma", "Intrahepatic cholangiocarcinoma")) %>%
  bold_labels() %>%
  modify_header(label = "**15 g/day substitution**")

table_cancer_type %>%
  as_gt() %>% # convert to gt table
  gt::gtsave( # save table as image
    filename = "table-cancer-subtype.png", path = "~/legliv/doc/Images"
  )
