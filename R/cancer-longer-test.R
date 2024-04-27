library(tidyverse)
data <- targets::tar_read(data_with_split_column)

data3 <- data %>%
  select(matches("p41270|p41280|p40006|p40005|id|baseline_start_date|p40001|p40000|p191")) %>%
  pivot_longer(
    cols = matches("_a[0-9]*$|_i[0-9]*$"),
    names_to = c(".value", "a"),
    names_sep = "_"
  ) %>%
  select(id, p41270var, p40006, p41280, p40005, baseline_start_date, p40001, p40000, p191) %>%
  filter(str_detect(p41270var, "C22.0|C22.1")|str_detect(p40006, "C22.0|C22.1")) %>%
  pivot_longer(
    cols = matches("p41280|p40005"),
    names_to = "cancer",
    values_to = "date"
  ) %>%
  filter(!is.na(date)) %>%
  group_by(id) %>%
  arrange(date, .by_group = TRUE) %>%
  slice_head() %>%
  rename(liver_cancer_date = date) %>%
  select(id, liver_cancer_date)

data <- data %>%
  left_join(data3, by = "id")

data <- data %>%
  select(matches("p41270|p41280|p40006|p40005|id|baseline_start_date|p40001|p40000|p191")) %>%
  pivot_longer(
    cols = matches("_a[0-9]*$|_i[0-9]*$"),
    names_to = c(".value", "a"),
    names_sep = "_"
  ) %>%
  select(id, p41270var, p40006, p41280, p40005, baseline_start_date, p40001, p40000, p191)



data2 <- data %>%
  select(id, baseline_start_date, liver_cancer_date, p40000_i0, p191) %>%
  mutate(cens_date = if_else(is.na(liver_cancer_date) & is.na(p40000_i0) & is.na(p191), as.Date("2022-12-31"), NA)) %>%
  pivot_longer(
    cols = matches("liver_cancer_date|p40000_i0|p191|cens_date"),
    names_to = "status",
    values_to = "date"
  ) %>%
  filter(date > baseline_start_date) %>%
  group_by(id) %>%
  arrange(date, .by_group = TRUE) %>%
  slice_head() %>%
  filter(date > baseline_start_date)
