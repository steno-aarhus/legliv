# Diet variables


data <- data %>%
    rowwise() %>%
    mutate(leg_mean = mean(c_across(legumes0:legumes4), na.rm = T),
           red_meat_mean = mean(c_across(red_meat0:red_meat4), na.rm = T),
           proc_meat_mean = mean(c_across(proc_meat0:proc_meat4), na.rm = T),
           food_weig_mean = mean(c_across(food_weig0:food_weig4), na.rm = T),
           red_meat_mean = mean(c_across(food_weig0:food_weig4), na.rm = T)
    )

data %>%
    filter(!is.na(leg_mean)) %>%
    summary()

data %>%
    ggplot(aes(x = leg_mean)) +
    geom_histogram()


#for liver data

data_leg_liv <- data_leg_liv %>%
    rowwise() %>%
    mutate(leg_mean = mean(c_across(legumes0:legumes4), na.rm = T))

data_leg_liv %>%
    filter(!is.na(leg_mean)) %>%
    summary()

data_leg %>%
    ggplot(aes(x = leg_mean)) +
    geom_histogram()
