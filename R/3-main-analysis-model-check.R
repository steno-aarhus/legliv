# Template
fit <- coxph(Surv(time = study_time, event = status == "Liver cancer") ~ strata() +
                 sex + education + spouse + exercise + smoking + bmi_category + ethnicity +
                 diabetes + nafld + cholelith + cystectomy,
             data = data)
newdata <- expand_grid(=c(), sex="Female", education="High", spouse="Yes", exercise="Yes", smoking="Current",
                       bmi_category="Overweight", ethnicity="White",
                       diabetes="No", nafld="No", cholelith="No", cystectomy="No")
survfit(fit, newdata) |>
    plot(fun = (function(x) log(-log(x))),
         main=" ",
         col=1:2)

fit <- coxph(Surv(time = study_time, event = status == "Liver cancer") ~ strata(smoking) +
                 sex + education + spouse + exercise + bmi_category + ethnicity +
                 diabetes + nafld + cholelith + cystectomy,
             data = data)
newdata <- expand_grid(smoking=c("Never","Previous","Current"), sex="Female", education="High", spouse="Yes", exercise="Yes",
                       bmi_category="Overweight", ethnicity="White",
                       diabetes="No", nafld="No", cholelith="No", cystectomy="No")
survfit(fit, newdata) |>
    plot(fun = (function(x) log(-log(x))),
         main=" ",
         col=1:3)

fit <- coxph(Surv(time = study_time, event = status == "Liver cancer") ~ strata(ethnicity) +
                 sex + education + spouse + exercise + smoking + bmi_category +
                 diabetes + nafld + cholelith + cystectomy,
             data = data)
newdata <- expand_grid(ethnicity=c("White","Other"), sex="Female", education="High", spouse="Yes", exercise="Yes", smoking="Current",
                       bmi_category="Overweight",
                       diabetes="No", nafld="No", cholelith="No", cystectomy="No")
survfit(fit, newdata) |>
    plot(fun = (function(x) log(-log(x))),
         main=" ",
         col=1:2)
