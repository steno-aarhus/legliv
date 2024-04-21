library(ggconsort)
library(tidyverse)

ready_flowchart <-
  data_flowchart %>%
  cohort_start("UK Biobank") %>%
  cohort_define(
    one_ques = .full %>% filter(p20077 >= 1),
    two_ques = one_ques %>% filter(p20077 >= 2),
    liver = two_ques %>% filter(is.na(cancer_before)),
    l2fu = two_ques %>% filter(is.na(p191) | p191 >= baseline_start_date),
    both = two_ques %>% filter(is.na(cancer_before)) %>% filter(is.na(p191) | p191 >= baseline_start_date) %>% filter(!is.na(baseline_start_date)),
    miss = two_ques %>% filter(!is.na(baseline_start_date)),
    two = two_ques %>% filter(p20077 == 2),
    three = two_ques %>% filter(p20077 == 3),
    four = two_ques %>% filter(p20077 == 4),
    five = two_ques %>% filter(p20077 == 5),
    one_ques_excluded = anti_join(.full, one_ques, by = "id"),
    two_ques_excluded = anti_join(one_ques, two_ques, by = "id"),
    liver_excluded = anti_join(two_ques, liver, by = "id"),
    l2fu_excluded = anti_join(two_ques, l2fu, by = "id"),
    miss_excluded = anti_join(two_ques, miss, by = "id")
  ) %>%
  cohort_label(
    one_ques = "One or more 24-hour recall",
    one_ques_excluded = "No Oxford WebQ",
    two_ques = "Two or more Oxford WebQs",
    two_ques_excluded = "One Oxford WebQ",
    liver_excluded = "Liver cancer before baseline",
    l2fu_excluded = "Lost to follow-up before baseline",
    both = "Included in study",
    miss_excluded = "Missing diet data",
    two = "2 Oxford WebQs",
    three = "3 Oxford WebQs",
    four = "4 Oxford WebQs",
    five = "5 Oxford WebQs"
  )

flowchart <- ready_flowchart %>%
  consort_box_add(
    "full", 0, 50, cohort_count_adorn(ready_flowchart, .full)
  ) %>%
  consort_box_add(
    "first_inclusion", 0, 35, glue::glue(
      '
      {cohort_count_adorn(ready_flowchart, two_ques)}<br>
      <br>
      {cohort_count_adorn(ready_flowchart, two)}<br>
      {cohort_count_adorn(ready_flowchart, three)}<br>
      {cohort_count_adorn(ready_flowchart, four)}<br>
      {cohort_count_adorn(ready_flowchart, five)}
      '
    )
  ) %>%
  consort_box_add(
    "second_inclusion", 0, 10, cohort_count_adorn(ready_flowchart, both)
  ) %>%
  consort_box_add(
    "first_exclusion", 10, 42.5, glue::glue(
      '
      {cohort_count_adorn(ready_flowchart, one_ques_excluded)}<br>
      {cohort_count_adorn(ready_flowchart, two_ques_excluded)}
      '
      )
  ) %>%
  consort_box_add(
    "second_exclusion", 10, 18, glue::glue(
      '
      {cohort_count_adorn(ready_flowchart, liver_excluded)}<br>
      {cohort_count_adorn(ready_flowchart, l2fu_excluded)}<br>
      {cohort_count_adorn(ready_flowchart, miss_excluded)}
      '
      )
  ) %>%
  consort_arrow_add(
    "full", "bottom", "first_inclusion", "top"
  ) %>%
  consort_arrow_add(
    "first_inclusion", "bottom", "second_inclusion", "top"
  ) %>%
  consort_arrow_add(
    end = "first_exclusion", end_side = "left", start_x = 0, start_y = 42.5
  ) %>%
  consort_arrow_add(
    end = "second_exclusion", end_side = "left", start_x = 0, start_y = 18
  )

flowchart <- flowchart %>%
  ggplot() +
  geom_consort() +
  theme_consort(margin_h = 16.4, margin_v = 1)
  # ggtext::geom_richtext(
  #   aes(x = -40, y = 40, label = "Oxford WebQ was administered to:<br>~ 70,000 participants at initial assessment visit<br>~ 320,000 participants via e-mail")
  # )
flowchart
