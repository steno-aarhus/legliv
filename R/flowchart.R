library(ggconsort)
library(tidyverse)

targets::tar_config_set(store = here::here("_targets"))
data2 <- targets::tar_read(data_as_baseline)
data1 <- targets::tar_read(base_data)
data1 <- data1 %>%
  dplyr::mutate(id = dplyr::row_number())

data2 <- data2 %>%
  dplyr::mutate(id = dplyr::row_number())

data2 <- data2 %>%
  select(id, p20077, cancer_hcc_date, cancer_icc_date, icd10_hcc_date, icd10_icc_date, baseline_start_date)

data1 <- data1 %>%
  select(id, p20077, p191)

data <- full_join(data1, data2, by = "id")
data <- data %>%
  mutate(
    cancer_before = if_else(
      cancer_hcc_date <= baseline_start_date |
        cancer_icc_date <= baseline_start_date |
        icd10_hcc_date <= baseline_start_date |
        icd10_icc_date <= baseline_start_date, "Yes", "No"
    )
  )
ready_flowchart <-
  data %>%
  cohort_start("Whole UK Biobank") %>%
  cohort_define(
    one_ques = .full %>% filter(p20077.x >= 1),
    two_ques = one_ques %>% filter(p20077.x >= 2),
    liver = two_ques %>% filter(is.na(cancer_before)),
    l2fu = two_ques %>% filter(is.na(p191) | p191 >= baseline_start_date),
    both = two_ques %>% filter(is.na(cancer_before)) %>% filter(is.na(p191) | p191 >= baseline_start_date) %>% filter(!is.na(baseline_start_date)),
    miss = two_ques %>% filter(!is.na(baseline_start_date)),
    one_ques_excluded = anti_join(.full, one_ques, by = "id"),
    two_ques_excluded = anti_join(one_ques, two_ques, by = "id"),
    liver_excluded = anti_join(two_ques, liver, by = "id"),
    l2fu_excluded = anti_join(two_ques, l2fu, by = "id"),
    miss_excluded = anti_join(two_ques, miss, by = "id")
  ) %>%
  cohort_label(
    one_ques = "One or more 24-hour recall",
    one_ques_excluded = "Excluded",
    two_ques = "Two or more 24-hour recall",
    two_ques_excluded = "Excluded",
    liver_excluded = "Liver cancer diagnosis before start of follow-up",
    l2fu_excluded = "Lost to follow-up before baseline",
    both = "Included",
    miss_excluded = "Missing diet data"
  )

study_cohorts
summary(study_cohorts)

flowchart <- ready_flowchart %>%
  consort_box_add(
    "full", 0, 50, cohort_count_adorn(study_cohorts, .full)
  ) %>%
  consort_box_add(
    "exclusions", 20, 40, glue::glue(
      '{cohort_count_adorn(study_cohorts, one_ques_excluded)}<br>
      ')
  ) %>%
  consort_box_add(
    "included", 0, 30, cohort_count_adorn(study_cohorts, one_ques)
  ) %>%
  consort_arrow_add(
    end = "exclusions", end_side = "left", start_x = 0, start_y = 40
  ) %>%
  consort_arrow_add(
    "full", "bottom", "included", "top"
  ) %>%
  consort_box_add(
    "exclusion", 20, 20, glue::glue(
      '{cohort_count_adorn(study_cohorts, two_ques_excluded)}<br>
      ')
  ) %>%
  consort_arrow_add(
    end = "exclusion", end_side = "left", start_x = 0, start_y = 20
  ) %>%
  consort_box_add(
    "included2", 0, 10, cohort_count_adorn(study_cohorts, two_ques)
  ) %>%
  consort_arrow_add(
    "included", "bottom", "included2", "top"
  ) %>%
  consort_box_add(
    "exclusion2", 20, 0, glue::glue(
      '{cohort_count_adorn(study_cohorts, liver_excluded)}<br>
      {cohort_count_adorn(study_cohorts, l2fu_excluded)}<br>
      {cohort_count_adorn(study_cohorts, miss_excluded)}<br>
      ')
  ) %>%
  consort_arrow_add(
    end = "exclusion2", end_side = "left", start_x = 0, start_y = 0
  ) %>%
  consort_box_add(
    "included3", 0, -10, cohort_count_adorn(study_cohorts, both)
  ) %>%
  consort_arrow_add(
    "included2", "bottom", "included3", "top"
  )

study_consort %>%
  ggplot() +
  geom_consort() +
  theme_consort(margin_h = 16.6, margin_v = 4)
