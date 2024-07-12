
create_flowchart <- function(data) {
  ready_flowchart <-
    data %>%
    cohort_start("UK Biobank") %>%
    cohort_define(
      one_ques = .full %>% filter(p20077 >= 1),
      two_ques = one_ques %>% filter(p20077 >= 2),
      liver = two_ques %>% filter(
        is.na(liver_cancer_date) |
          liver_cancer_date >= baseline_start_date
      ),
      l2fu = two_ques %>% filter(is.na(p191) |
                                   p191 >= baseline_start_date),
      both = two_ques %>%
        filter(is.na(p191) | p191 >= baseline_start_date) %>%
        filter(!is.na(baseline_start_date)),
      all = two_ques %>%
        filter(is.na(p191) | p191 >= baseline_start_date) %>%
        filter(!is.na(baseline_start_date)) %>%
        filter(
          is.na(liver_cancer_date) |
            liver_cancer_date >= baseline_start_date
        ),
      miss = two_ques %>% filter(!is.na(baseline_start_date)),
      two = two_ques %>% filter(p20077 == 2),
      three = two_ques %>% filter(p20077 == 3),
      four = two_ques %>% filter(p20077 == 4),
      five = two_ques %>% filter(p20077 == 5),
      one_ques_excluded = anti_join(.full, one_ques, by = "id"),
      two_ques_excluded = anti_join(one_ques, two_ques, by = "id"),
      liver_excluded = anti_join(two_ques, liver, by = "id"),
      l2fu_excluded = anti_join(two_ques, l2fu, by = "id"),
      miss_excluded = anti_join(two_ques, miss, by = "id"),
      both_excluded = anti_join(two_ques, both, by = "id")
    ) %>%
    cohort_label(
      one_ques = "One or more 24-hour recall",
      one_ques_excluded = "No Oxford WebQ",
      two_ques = "Two or more Oxford WebQs",
      two_ques_excluded = "One Oxford WebQ",
      liver_excluded = "Liver cancer before baseline",
      l2fu_excluded = "Lost to follow-up before baseline",
      all = "Included in study",
      miss_excluded = "Missing diet data",
      both_excluded = "Loss to follow-up before baseline<br>or missing diet data",
      two = "2 Oxford WebQs",
      three = "3 Oxford WebQs",
      four = "4 Oxford WebQs",
      five = "5 Oxford WebQs"
    )

  flowchart_make <- ready_flowchart %>%
    consort_box_add(
      name = "full",
      x = 0,
      y = 50,
      label = cohort_count_adorn(
        ready_flowchart,
        .full,
        .label_fn = function(cohort, label, count) {
          glue::glue("{label} (n = {comma(count)})")
        }
      )
    ) %>%
    consort_box_add(
      name = "first_inclusion",
      x = 0,
      y = 35,
      label = glue::glue(
        '
      {cohort_count_adorn(ready_flowchart, two_ques, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}<br>
      <br>
      {cohort_count_adorn(ready_flowchart, two, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}<br>
      {cohort_count_adorn(ready_flowchart, three, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}<br>
      {cohort_count_adorn(ready_flowchart, four, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}<br>
      {cohort_count_adorn(ready_flowchart, five, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}
      '
      )
    ) %>%
    consort_box_add(
      name = "second_inclusion",
      x = 0,
      y = 10,
      label = cohort_count_adorn(
        ready_flowchart,
        all,
        .label_fn = function(cohort, label, count) {
          glue::glue("{label} (n = {comma(count)})")
        }
      )
    ) %>%
    consort_box_add(
      name = "first_exclusion",
      x = 5,
      y = 42.5,
      label = glue::glue(
        '
      Exclusion of participants with:<br><br>
      {cohort_count_adorn(ready_flowchart, one_ques_excluded, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}<br>
      {cohort_count_adorn(ready_flowchart, two_ques_excluded, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}
      '
      )
    ) %>%
    consort_box_add(
      name = "second_exclusion",
      x = 5,
      y = 18,
      label = glue::glue(
        '
      Exclusions due to:<br><br>
      {cohort_count_adorn(ready_flowchart, liver_excluded, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}<br>
      {cohort_count_adorn(ready_flowchart, both_excluded, .label_fn = function(cohort, label, count) {glue::glue("{label} (n = {comma(count)})")})}
      '
      )
    ) %>%
    consort_arrow_add("full", "bottom", "first_inclusion", "top") %>%
    consort_arrow_add("first_inclusion", "bottom", "second_inclusion", "top") %>%
    consort_arrow_add(
      end = "first_exclusion",
      end_side = "left",
      start_x = 0,
      start_y = 42.5
    ) %>%
    consort_arrow_add(
      end = "second_exclusion",
      end_side = "left",
      start_x = 0,
      start_y = 18
    )

  flowchart <- flowchart_make %>%
    ggplot() +
    geom_consort_arrow() +
    geom_consort_box() +
    theme_void() +
    theme(plot.margin = margin(0.5, 13, 1, 7.5, unit = "lines"))
}
