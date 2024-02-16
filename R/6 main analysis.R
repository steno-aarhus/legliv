library(survival)
library(parameters)

fit_cox <- coxph(Surv(time = time, event = status=="liver cancer") ~ ,
                 data = data)
fit_cox %>%
    parameters(exponentiate = T)

data %>%
    summary()
