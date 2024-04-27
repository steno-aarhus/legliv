library(tidyverse)
data <- data %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  filter(p20077 >= 2)
data1 <- data %>%
  select(id, p20077, starts_with("p104900")) %>%
  pivot_longer(
    cols = starts_with("p104900"),
      names_to = "name",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    phys_vig = case_when(
      value == "None" ~ 0,
      value == "Under 10 minutes" ~ 5,
      value == "10-30 minutes" ~ 20,
      value == "30-60 minutes" ~ 45,
      value == "1-2 hours" ~ 90,
      value == "2-4 hours" ~ 180,
      value == "4-6 hours" ~ 300,
      value == "6+ hours" ~ 360
    )
  ) %>%
  group_by(id) %>%
  arrange(value, .by_group = TRUE) %>%
  summarise(mean_phys_vig = mean(phys_vig))

data1 %>% group_by(value) %>% summarise(n())

data2 <- data1 %>%
  mutate(
    phys_vigorous = case_when(
      p104900_i0 == "None" ~ 0
    )
  )
