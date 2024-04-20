# Template
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata() +
    sex + education + spouse + exercise + smoking,
  data = data
)
newdata <- expand_grid(
  sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current"
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
    education + spouse + exercise + smoking,
  data = data
)
newdata <- expand_grid(sex = c("Female", "Male"),
                       education = "High", spouse = "Yes", exercise = "Yes", smoking = "Never"
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
    sex + spouse + exercise + smoking,
  data = data
)
newdata <- expand_grid(education = c("High", "Intermediate", "Low"),
                       sex = "Female", spouse = "Yes", exercise = "Yes", smoking = "Never",
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:3
  )

# Living alone
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(spouse) +
    sex + education + exercise + smoking,
  data = data
)
newdata <- expand_grid(spouse = c("Yes", "No"),
                       sex = "Female", education = "High", exercise = "Yes", smoking = "Never"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )

# Exercise
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(exercise) +
    sex + education + spouse + smoking,
  data = data
)
newdata <- expand_grid(exercise = c("Yes", "No"),
                       sex = "Female", education = "High", spouse = "Yes", smoking = "Never",
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
    sex + education + spouse + exercise,
  data = data
)
newdata <- expand_grid(smoking = c("Never", "Ever"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:3
  )


