# Template
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata() +
    sex + education + spouse + exercise + smoking + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(
  sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
  bmi_category = "Overweight", ethnicity = "White",
  diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# Sex
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(sex) +
    education + spouse + exercise + smoking + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(sex = c("Female", "Male"),
                       education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       bmi_category = "Overweight", ethnicity = "White",
                       diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# Smoking
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(smoking) +
    sex + education + spouse + exercise + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(smoking = c("Never", "Previous", "Current"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes",
                       bmi_category = "Overweight", ethnicity = "White",
                       diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:3
  )

# Ethnicity
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(ethnicity) +
    sex + education + spouse + exercise + smoking + bmi_category +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(ethnicity = c("White", "Other"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       bmi_category = "Overweight",
                       diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# Living alone
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(spouse) +
    sex + education + exercise + smoking + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(spouse = c("Yes", "No"),
                       sex = "Female", education = "High", exercise = "Yes", smoking = "Current",
                       bmi_category = "Overweight", ethnicity = "White",
                       diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# Education
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(education) +
    sex + spouse + exercise + smoking + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(education = c("High", "Intermediate", "Low"),
                       sex = "Female", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       bmi_category = "Overweight", ethnicity = "White",
                       diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:3
  )

# Exercise
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(exercise) +
    sex + education + spouse + smoking + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(exercise = c("Yes", "No"),
                       sex = "Female", education = "High", spouse = "Yes", smoking = "Current",
                       bmi_category = "Overweight", ethnicity = "White",
                       diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# BMI
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(bmi_category) +
    sex + education + spouse + exercise + smoking + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(bmi_category = c("Normal weight", "Overweight", "Obese"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       ethnicity = "White",
                       diabetes = "No", nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:3
  )

# Diabetes
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(diabetes) +
    sex + education + spouse + exercise + smoking + bmi_category + ethnicity +
    nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(diabetes = c("Yes", "No"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       bmi_category = "Overweight", ethnicity = "White",
                       nafld = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# NAFLD
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(nafld) +
    sex + education + spouse + exercise + smoking + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(nafld = c("Yes","No"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       bmi_category = "Overweight", ethnicity = "White",
                       diabetes = "No", cholelith = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# Cholelithiasis
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(cholelith) +
    sex + education + spouse + exercise + smoking + bmi_category + ethnicity +
    diabetes + nafld + cystectomy,
  data = data
)
newdata <- expand_grid(cholelith = c("Yes","No"),
  sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
  bmi_category = "Overweight", ethnicity = "White",
  diabetes = "No", nafld = "No", cystectomy = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )


# Cystectomy
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(cystectomy) +
    sex + education + spouse + exercise + smoking + bmi_category + ethnicity +
    diabetes + nafld + cholelith + cystectomy,
  data = data
)
newdata <- expand_grid(cystectomy = c("Yes","No"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       bmi_category = "Overweight", ethnicity = "White",
                       diabetes = "No", nafld = "No", cholelith = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )
