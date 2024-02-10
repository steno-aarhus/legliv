
#WARNING runs for a very long time when applied.

# Function to separate multiple ICD codes in a cell
separate_icd <- function(df, column_name, new_column_prefix, max_columns = 131, sep = "|") {
    # Replace NA values in the column with an empty string
    df <- df %>%
        mutate(!!sym(column_name) := ifelse(is.na(!!sym(column_name)), "", !!sym(column_name)))

    # Get maximum number of ICD codes in a single cell
    max_icd_count <- df %>%
        mutate(icd_count = str_count(!!sym(column_name), sep) + 1) %>%
        pull(icd_count) %>%
        max()

    # Determine the number of columns to create
    num_columns <- min(max_icd_count, max_columns)

    # Create a sequence for the maximum number of ICD codes
    icd_columns <- paste0(new_column_prefix, "_a", seq_len(num_columns - 1))

    # Separate ICD codes into separate columns
    df %>%
        separate(!!sym(column_name), into = c("icd10_a0", icd_columns), sep = "\\|", fill = "right") %>%
        mutate(across(starts_with(new_column_prefix), str_trim))
}

# Apply the function to your dataframe
data <- separate_icd(df = data, column_name = "icd10", new_column_prefix = "icd10", max_columns = 131)



data %>%
    filter(!is.na(icd10_a130)) %>%
    select(icd10_a130) %>%
    print()


filtered_data <- data_liver %>%
   mutate(across(starts_with("p41280_a"), ~ replace(., . > baseline_start_date, NA)),
         across(starts_with("p41281_a"), ~ replace(., . > baseline_start_date, NA)))



# This function works only on data_liver dataset

# Function to separate multiple ICD codes in a cell
separate_icd <- function(df, column_name, new_column_prefix, sep = "|") {
    # Get maximum number of ICD codes in a single cell
    max_icd_count <- df %>%
        mutate(icd_count = str_count(!!sym(column_name), sep) + 1) %>%
        pull(icd_count) %>%
        max()

    # Determine the number of columns to create
    num_columns <- min(max_icd_count, max_columns)

    # Create a sequence for the maximum number of ICD codes
    icd_columns <- paste0(new_column_prefix, "_a", seq_len(num_columns - 1))

    # Separate ICD codes into separate columns
    df %>%
        separate(!!sym(column_name), into = c("icd10_a0", icd_columns), sep = "\\|", fill = "right") %>%
        mutate(across(starts_with(new_column_prefix), str_trim))
}

# Apply the function to your dataframe
data_liver <- separate_icd(df = data_liver, column_name = "icd10", new_column_prefix = "icd10", max_columns = 131)

# Apply the function to your dataframe
data <- separate_icd(df = data, column_name = "icd10", new_column_prefix = "icd10", max_columns = 131)
