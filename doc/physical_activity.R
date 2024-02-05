# physical activity

# Creating MET-categories:
cut_points <- c(-Inf, 600, 1200, Inf)
labels <- c( "low MET", "med MET", "high MET")

#data with only MET-variable:
data_act <- data %>%
    select(phys_acti)

# categorising according to MET-variable:
data_act <- data_act %>%
    mutate(met_cat = cut(phys_acti, breaks = cut_points, labels = labels, include.lowest = TRUE))

data_act %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)

# n=17931 (14.1%) is NA. Removing these:

data_act %>%
    filter(!is.na(met_cat)) %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)


# Same for liver data:
data_act_liver <- data_liver %>%
    select(phys_acti)

data_act_liver <- data_act_liver %>%
    mutate(met_cat = cut(phys_acti, breaks = cut_points, labels = labels, include.lowest = TRUE))

data_act_liver %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)

# n=24 (20.9%) is NA.

data_act_liver %>%
    filter(!is.na(met_cat)) %>%
    count(met_cat) %>%
    mutate(percentage = n / sum(n) * 100)
