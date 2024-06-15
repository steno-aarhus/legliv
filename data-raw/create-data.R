# Keep only the necessary variables for RAP -------------------------------

library(magrittr)

# Update if necessary.
ukbAid::rap_variables %>%
  readr::write_csv(here::here("data-raw/rap-variables.csv"))

# Create the project dataset and save inside RAP --------------------------

# Uncomment and run the below lines **ONLY AFTER** selecting the variables you want.
# After running this code and creating the csv file in the main RAP project
# folder, comment it out again so you don't accidentally run it anymore (unless
# you need to re-create the dataset).

readr::read_csv(here::here("data-raw/rap-variables.csv")) %>%
  dplyr::pull(id) %>%
  ukbAid::create_csv_from_database(username = "nielsbock")
