# Template
fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata() +
    sex + education + spouse + exercise + smoking + gall_disease + met_synd,
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
    education + spouse + exercise + smoking + gall_disease + met_synd,
  data = data
)
newdata <- expand_grid(sex = c("Female", "Male"),
                       education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       gall_disease = "No", met_synd = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )


fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(met_synd) +
    sex + education + spouse + exercise + smoking + gall_disease,
  data = data
)
newdata <- expand_grid(met_synd = c("Yes", "No"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       gall_disease = "No"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )


fit <- coxph(
  Surv(time = study_time, event = status == "Liver cancer") ~ strata(gall_disease) +
    sex + education + spouse + exercise + smoking + met_synd,
  data = data
)
newdata <- expand_grid(gall_disease = c("No", "Yes"),
                       sex = "Female", education = "High", spouse = "Yes", exercise = "Yes", smoking = "Current",
                       met_synd = "Yes"
)
survfit(fit, newdata) |>
  plot(
    fun = (function(x) log(-log(x))),
    main = " ",
    col = 1:2
  )
