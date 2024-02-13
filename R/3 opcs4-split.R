# Function to separate multiple ICD codes in a cell
separate_opcs4 <- function(df, column_name, new_column_prefix, max_columns = 73, sep = "|") {
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
        separate(!!sym(column_name), into = c("opcs4_a0", icd_columns), sep = "\\|", fill = "right") %>%
        mutate(across(starts_with(new_column_prefix), str_trim))

    data <- data %>%
        mutate(opcs4_a0 = na_if(opcs4_a0, ""))
}

# Apply the function to your dataframe
data <- separate_opcs4(df = data, column_name = "opcs4", new_column_prefix = "opcs4", max_columns = 73)


