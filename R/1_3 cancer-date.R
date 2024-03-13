# Convert to long format
data_long <- data %>%
    select(starts_with("p40006"),starts_with("p40005"), id) %>%
    pivot_longer(cols = starts_with("p40006"), names_to = "cancer_index", values_to = "cancer_diagnosis") %>%
    pivot_longer(cols = starts_with("p40005"), names_to = "date_index", values_to = "cancer_date") %>%
    mutate(cancer_index = parse_number(cancer_index),
           date_index = parse_number(date_index)) %>%
    filter(cancer_diagnosis == "C22.0 Liver cell carcinoma" |
               cancer_diagnosis == "C22.1 Intrahepatic bile duct carcinoma") %>%
    select(-cancer_diagnosis)

# Extract the corresponding cancer diagnosis dates
data_long <- data_long %>%
    mutate(cancer_date1 = lead(cancer_date, order_by = cancer_index))

# Convert back to wide format
data_wide <- data_long %>%
    pivot_wider(names_from = date_index, values_from = cancer_date)
