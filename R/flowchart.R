library(ggconsort)
library(tidyverse)

targets::tar_config_set(store = here::here("_targets"))
data_flowchart <- targets::tar_read(data_flowchart)

ready_flowchart <-
  data_flowchart %>%
  cohort_start("Whole UK Biobank") %>%
  cohort_define(
    one_ques = .full %>% filter(p20077 >= 1),
    two_ques = one_ques %>% filter(p20077 >= 2),
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
    one_ques_excluded = "No Oxford WebQ",
    two_ques = "Two or more 24-hour recall",
    two_ques_excluded = "One Oxford WebQ",
    liver_excluded = "Liver cancer diagnosis before start of follow-up",
    l2fu_excluded = "Lost to follow-up before baseline",
    both = "Included in study",
    miss_excluded = "Missing diet data"
  )

study_cohorts
summary(study_cohorts)

flowchart <- ready_flowchart %>%
  consort_box_add(
    "full", 0, 50, cohort_count_adorn(ready_flowchart, .full)
  ) %>%
  consort_box_add(
    "first_exclusion", 10, 40, glue::glue(
      '
      {cohort_count_adorn(ready_flowchart, one_ques_excluded)}<br>
      {cohort_count_adorn(ready_flowchart, two_ques_excluded)}<br>
      ')
  ) %>%
  consort_box_add(
    "two_ques_box", 0, 30, cohort_count_adorn(ready_flowchart, two_ques)
  ) %>%
  consort_arrow_add(
    end = "first_exclusion", end_side = "left", start_x = 0, start_y = 40
  ) %>%
  consort_arrow_add(
    "full", "bottom", "two_ques_box", "top"
  ) %>%
  consort_box_add(
    "second_exclusion", 10, 20, glue::glue(
      '{cohort_count_adorn(ready_flowchart, liver_excluded)}<br>
      {cohort_count_adorn(ready_flowchart, l2fu_excluded)}<br>
      {cohort_count_adorn(ready_flowchart, miss_excluded)}<br>
      ')
  ) %>%
  consort_arrow_add(
    end = "second_exclusion", end_side = "left", start_x = 0, start_y = 20
  ) %>%
  consort_box_add(
    "included_cohort", 0, 10, cohort_count_adorn(ready_flowchart, both)
  ) %>%
  consort_arrow_add(
    "two_ques_box", "bottom", "included_cohort", "top"
  )

flowchart %>%
  ggplot() +
  geom_consort() +
  theme_consort(margin_h = 20, margin_v = 1)
  # ggtext::geom_richtext(
  #   aes(x = -40, y = 40, label = "Oxford WebQ was administered to:<br>~ 70,000 participants at initial assessment visit<br>~ 320,000 participants via e-mail")
  # )
