# Time to event dataset:

# For cohort:
data_t2e <- data %>%
    select(birth_year,
           month_of_birth,
           dead_date,
           age_dead,
           dead_cause,
           cancer_date0,
           cancer_age0,
           cancer0,
           cancer_date1,
           cancer_age1,
           cancer1,
           cancer_date2,
           cancer_age2,
           cancer2,
           cancer_date3,
           cancer_age3,
           cancer3,
           ques_comp_n,
           ques_comp_t0,
           ques_comp_t1,
           ques_comp_t2,
           ques_comp_t3,
           ques_comp_t4)

# For liver cancer cases:

data_t2e_liver <- data_liver %>%
    select(birth_year,
           month_of_birth,
           dead_date,
           age_dead,
           dead_cause,
           cancer_date0,
           cancer_age0,
           cancer0,
           cancer_date1,
           cancer_age1,
           cancer1,
           cancer_date2,
           cancer_age2,
           cancer2,
           cancer_date3,
           cancer_age3,
           cancer3,
           ques_comp_n,
           ques_comp_t0,
           ques_comp_t1,
           ques_comp_t2,
           ques_comp_t3,
           ques_comp_t4)
