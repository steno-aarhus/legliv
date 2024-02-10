# Table 1
library(parameters)
library(magrittr)
library(Hmisc)

# age

table1 <- data %>%
    select(age_at_baseline)

table1 %>%
    summary()

table1_liv <- data_liver %>%
    select(age_at_baseline)

table1_liv %>%
    summary()

# Sex
table1 <- data %>%
    select(sex)

table1 %>%
    group_by(sex) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)

table1_liv <- data_liver %>%
    select(sex)

table1_liv %>%
    group_by(sex) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)

# Socioeconomic status

# Education

table1 <- data %>%
    select(education)

# each participants have multiple categories. Recoding to 1 of 3 categories:
table1 <- table1 %>%
    mutate(education2 = case_when(
        grepl("College", education, ignore.case = TRUE) ~ "high",
        grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("O levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("CSEs", education, ignore.case = TRUE) ~ "low",
        grepl("NVQ or HND", education, ignore.case = TRUE) ~ "low",
        grepl("Other professional", education, ignore.case = TRUE) ~ "low",
        grepl("None of the above", education, ignore.case = TRUE) ~ "low",
        TRUE ~ as.character(education)
    ))

table1 %>%
    group_by() %>%
    count(education2) %>%
    mutate(percentage = n / sum(n) * 100)

table1 %>%
    summary

table1 %>%
    ggplot(aes(x = education2)) +
    geom_bar()

# For liver data

table1_liv <- data_liver %>%
    select(education)

table1_liv <- table1_liv %>%
    mutate(education2 = case_when(
        grepl("College", education, ignore.case = TRUE) ~ "high",
        grepl("A levels/AS levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("O levels", education, ignore.case = TRUE) ~ "intermediate",
        grepl("CSEs", education, ignore.case = TRUE) ~ "low",
        grepl("NVQ or HND", education, ignore.case = TRUE) ~ "low",
        grepl("Other professional", education, ignore.case = TRUE) ~ "low",
        grepl("None of the above", education, ignore.case = TRUE) ~ "low",
        TRUE ~ as.character(education)
    ))

table1_liv %>%
    group_by() %>%
    count(education2) %>%
    mutate(percentage = n / sum(n) * 100)

table1_liv %>%
    ggplot(aes(x = education2)) +
    geom_bar()

# Townsend deprivation index

table1 <- data %>%
    select(tdi)

table1 %>%
    summary()

table1 %>%
    ggplot(aes(x = tdi)) +
    geom_histogram()

table1 %>%
    ggplot(aes(sample = tdi)) +
    stat_qq() +
    stat_qq_line()

table1_liv <- data_liver %>%
    select(tdi)

table1_liv %>%
    summary()

table1_liv %>%
    ggplot(aes(x=tdi))+
    geom_histogram(bins = 10)

table1_liv %>%
    ggplot(aes(sample=tdi)) +
    stat_qq()+
    stat_qq_line()

install.packages('parameters')
library(parameters)

wilcox.test(table1$tdi, table1_liv$tdi ) |>
    parameters()


# Living alone, computed from number in household

table1 <- data %>%
    select(spouse)

table1 %>%
    group_by() %>%
    count(spouse) %>%
    arrange() %>%
    mutate(percentage = n/sum(n)*100) %>%
    print(n=37)


table1_liv <- data_liver %>%
    select(spouse)

table1_liv %>%
    group_by() %>%
    count(spouse) %>%
    arrange() %>%
    mutate(percentage = n/sum(n)*100) %>%
    print(n=37)


# physical activity

# Creating MET-categories:
cut_points <- c(-Inf, 600, 1200, Inf)
labels <- c( "low MET", "med MET", "high MET")

#data with only MET-variable:
table1 <- data %>%
    select(phys_acti)

# categorising according to MET-variable:
table1 <- table1 %>%
    mutate(met_cat = cut(phys_acti, breaks = cut_points, labels = labels, include.lowest = TRUE))

table1 %>%
    group_by() %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)

# 14.1% is NA. Removing these:

table1 %>%
    group_by() %>%
    filter(!is.na(met_cat)) %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)

# For liver cancer

#data with only MET-variable:
table1_liv <- data_liver %>%
    select(phys_acti)

# categorising according to MET-variable:
table1_liv <- table1_liv %>%
    mutate(met_cat = cut(phys_acti, breaks = cut_points, labels = labels, include.lowest = TRUE))

table1_liv %>%
    group_by() %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)

# 20.6% is NA. Removing these:

table1_liv %>%
    group_by() %>%
    filter(!is.na(met_cat)) %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)


# Smoking

data %>%
    group_by() %>%
    count(smoking) %>%
    mutate(percentage = n/sum(n)*100)

data_liver %>%
    group_by() %>%
    count(smoking) %>%
    mutate(percentage = n/sum(n)*100)


# Body mass index (BMI)

table1 <- data %>%
    select(bmi)

# Checking cohort BMI:

table1 %>%
    ggplot(aes(x = bmi)) +
    geom_histogram(na.rm = T)

table1 %>%
    ggplot(aes(sample = bmi)) +
    stat_qq(na.rm = T) +
    stat_qq_line(na.rm = T)

# smiling qq-plot. Data are not normally distributed. Trying log-transformation:

table1 <- table1 %>%
    mutate(logbmi = log(bmi))

table1 %>%
    ggplot(aes(x = logbmi)) +
    geom_histogram(na.rm = T)

table1 %>%
    ggplot(aes(sample = logbmi)) +
    stat_qq(na.rm = T) +
    stat_qq_line(na.rm = T)

# Looks normally distributed. Backtransforming to get mean:

table1 %$%
    exp(smean.cl.normal(logbmi))

table1 %$%
    exp(quantile(logbmi, probs = 0.25, na.rm = T))

table1 %$%
    exp(quantile(logbmi, probs = 0.75, na.rm = T))

# For liver cancer:

table1_liv <- data_liver %>%
    select(bmi)

# Checking cohort BMI:

table1_liv %>%
    ggplot(aes(x = bmi)) +
    geom_histogram(bins = 10, na.rm = T)

table1_liv %>%
    ggplot(aes(sample = bmi)) +
    stat_qq(na.rm = T) +
    stat_qq_line(na.rm = T)

# smiling qq-plot. Data are not normally distributed. Trying log-transformation:

table1_liv <- table1_liv %>%
    mutate(logbmi = log(bmi))

table1_liv %>%
    ggplot(aes(x = logbmi)) +
    geom_histogram(bins = 10, na.rm = T)

table1_liv %>%
    ggplot(aes(sample = logbmi)) +
    stat_qq(na.rm = T) +
    stat_qq_line(na.rm = T)

# Looks normally distributed. Backtransforming to get mean:

table1_liv %$%
    exp(smean.cl.normal(logbmi))

table1_liv %$%
    exp(quantile(logbmi, probs = 0.25, na.rm = T))

table1_liv %$%
    exp(quantile(logbmi, probs = 0.75, na.rm = T))

# Creating BMI categories:

# Defining intervals and creating labels:

cut_points <- c(-Inf, 24.99, 29.99, Inf)
labels <- c( "Normal weight", "Overweight", "Obese")

table1 <- table1 %>%
    mutate(bmi_category = cut(bmi, breaks = cut_points, labels = labels, include.lowest = TRUE))

table1 %>%
    group_by() %>%
    count(bmi_category) %>%
    mutate(percentage = n / sum(n) * 100)

# For liver cancer

table1_liv <- table1_liv %>%
    mutate(bmi_category = cut(bmi, breaks = cut_points, labels = labels, include.lowest = TRUE))

table1_liv %>%
    group_by() %>%
    count(bmi_category) %>%
    mutate(percentage = n / sum(n) * 100)

t.test(table1$logbmi, table1_liv$logbmi) %>%
    parameters()


# waist cirumference:

table1 <- data %>%
    select(wc)

table1 %>%
    ggplot(aes(x = wc)) +
    geom_histogram()

table1 %>%
    ggplot(aes(sample = wc)) +
    stat_qq() +
    stat_qq_line()

# smiling line, not normal distributed. log-transforming:

table1 <- table1 %>%
    mutate(logwc = log(wc))

table1 %>%
    ggplot(aes(x = logwc)) +
    geom_histogram()

table1 %>%
    ggplot(aes(sample = logwc)) +
    stat_qq() +
    stat_qq_line()

table1 %$%
    exp(smean.cl.normal(logwc))

table1 %$%
    exp(quantile(logwc, probs = 0.25, na.rm = T))

table1 %$%
    exp(quantile(logwc, probs = 0.75, na.rm = T))

# For liver cancer

table1_liv <- data_liver %>%
    select(wc)

table1_liv %>%
    ggplot(aes(x = wc)) +
    geom_histogram(bins = 10)

table1_liv %>%
    ggplot(aes(sample = wc)) +
    stat_qq() +
    stat_qq_line()

table1_liv %>%
    summary()
