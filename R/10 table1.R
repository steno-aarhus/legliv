library(boot)
library(table1)

data2 <- data

label(data2$age_at_baseline) <- "Age"
label(data2$sex) <- "Sex"
label(data2$education) <- "Educational level"
label(data2$tdi) <- "Townsend Deprivation Index"
label(data2$exercise) <- "Physical activity"
label(data2$smoking) <- "Smoking"
label(data2$bmi_category) <- "Body mass index"
label(data2$wc) <- "Waist circumference"
label(data2$sex) <- "Sex"
label(data2$ethnicity) <- "Ethnicity"
label(data2$diabetes) <- "Type 2 diabetes"
label(data2$nafld) <- "NAFLD"
label(data2$cholelith) <- "Cholelithiasis"
label(data2$cystectomy) <- "Cholecystectomy"
label(data2$alc_liver) <- "Alcoholic liver disease"

units(data2$age_at_baseline) <- "years"
units(data2$wc) <- "cm"

table1(~ age_at_baseline + sex + education + tdi + exercise + smoking + bmi_category + wc + ethnicity +
           diabetes + nafld + cholelith + cystectomy + alc_liver | status, data=data2,
       overall = c(left = "Cohort"))

