# Ethnicity

data_eth <- data %>%
    select(ethnicity)
view(data_eth)

data_eth %>%
    group_by(ethnicity) %>%
    summarise() %>%
    print(n=30)

# Recoding variables similar to UKB Showcase variables:
data_eth <- data_eth %>%
    mutate(ethnicity2 = case_when(
        ethnicity %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        ethnicity %in% c("Mixed", "White and Black Caribbean", "White and Black African", "White and Asian", "Any other mixed background") ~ "Mixed",
        ethnicity %in% c("Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", "Any other Asian background") ~ "Asian or Asian British",
        ethnicity %in% c("Black or Black British", "Caribbean", "African", "Any other Black background") ~ "Black or Black British",
        TRUE ~ as.character(ethnicity)
    ))

data_eth %>%
    count(ethnicity2) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    print()


# For liver cancer
data_eth_liver <- data_liver %>%
    select(ethnicity)
view(data_eth_liver)

data_eth_liver %>%
    group_by(ethnicity) %>%
    summarise() %>%
    print(n=30)

data_eth_liver <- data_eth_liver %>%
    mutate(ethnicity2 = case_when(
        ethnicity %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        ethnicity %in% c("Mixed", "White and Black Caribbean", "White and Black African", "White and Asian", "Any other mixed background") ~ "Mixed",
        ethnicity %in% c("Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", "Any other Asian background") ~ "Asian or Asian British",
        ethnicity %in% c("Black or Black British", "Caribbean", "African", "Any other Black background") ~ "Black or Black British",
        TRUE ~ as.character(ethnicity)
    ))

data_eth_liver %>%
    count(ethnicity2) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    print(n=23)

