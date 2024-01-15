# Main analysis

# step 1: defining survival time

# follow-up starts at participants last completed Oxford WebQ
# Right censuring will be due to the following events, whichever comes first:
# 1. most recent registry date for the full follow-up for the outcomes
# 2. date of death
# 3. date of liver cancer diagnosis
# 4. loss to follow-up

# From time-to-event lecture notes
# We create the time-dependent covariate (postwin) using the function tmerge.
# The purpose of tmerge is to merge time-dependent information (covariate) to a
# survival data set. This is done in two steps: 1) a initial setup step where a
# data set with delayed entry variable tstart, the time variable tstop and a
# status indicator of which we name (here status). 2) We merge the time
# dependent information. Note that the create data set (here
# academy_nominated_split) obtain attributes, which are used in further adding
# time dependent covariates. Any tidy manipulation of the created data set
# will remove these attributes.
temp <- ukb_dataset
# Step 1.
ukb_dataset_split <- tmerge(temp, temp,
                                  id=id,
                                  status = event(right_censoring, as.numeric(liver_cancer==1)),
                                  tstart = finished_oxford_webq,
                                  tstop = right_censoring
)
# Step 2.
ukb_dataset_split <- tmerge(ukb_dataset_split, academy_wins,
                                  id=identity,
                                  postwin=tdc(firstwinage))


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

