# Main analysis

# step 1: defining survival time

# follow-up starts at participants last completed Oxford WebQ
# Right censuring will be due to the following events, whichever comes first:
# 1. most recent registry date for the full follow-up for the outcomes
# 2. date of death
# 3. date of liver cancer diagnosis
# 4. loss to follow-up

# step 2: substitution analysis
# Loading package
library(survival)

# Model 1:
# substitution in grams:
model_1_total_g <- ukb_data |>
  coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + total_caloric_intake + other_food_components,
    data = _
  )

model_1_processed_g <- ukb_data |>
  coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + total_caloric_intake + other_food_components,
    data = _
  )

model_1_unprocessed_g <- ukb_data |>
  coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + total_caloric_intake + other_food_components,
    data = _
  )

# substitution in calories:
model_1_total_c <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + total_caloric_intake + other_food_components,
          data = _
    )

model_1_processed_c <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + total_caloric_intake + other_food_components,
          data = _
    )

model_1_unprocessed_c <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + total_caloric_intake + other_food_components,
          data = _
    )

# Model 2:
# substitution in grams:
model_2_total_g <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + education + tds + ethnicity + bmi + phys_active + smoking + alcohol + wc + t2d + cholelith + cholecyst + total_caloric_intake + other_food_components,
          data = _
    )

model_2_processed_g <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + education + tds + ethnicity + bmi + phys_active + smoking + alcohol + wc + t2d + cholelith + cholecyst + total_caloric_intake + other_food_components,
          data = _
    )

model_2_unprocessed_g <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + education + tds + ethnicity + bmi + phys_active + smoking + alcohol + wc + t2d + cholelith + cholecyst + total_caloric_intake + other_food_components,
          data = _
    )

# substitution in calories
model_2_total_c <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + education + tds + ethnicity + bmi + phys_active + smoking + alcohol + wc + t2d + cholelith + cholecyst + total_caloric_intake + other_food_components,
          data = _
    )

model_2_processed_c <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + education + tds + ethnicity + bmi + phys_active + smoking + alcohol + wc + t2d + cholelith + cholecyst + total_caloric_intake + other_food_components,
          data = _
    )

model_2_unprocessed_c <- ukb_data |>
    coxph(Surv(age, event = liver_cancer == "liver_cancer") ~ exposure + sex + education + tds + ethnicity + bmi + phys_active + smoking + alcohol + wc + t2d + cholelith + cholecyst + total_caloric_intake + other_food_components,
          data = _
    )

