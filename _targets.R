# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Tell targets your needed packages.
package_deps <- desc::desc_get_deps()$package |>
  stringr::str_subset("^R$", negate = TRUE)

# Set target options:
tar_option_set(
  packages = package_deps,
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # This likely isn't necessary for most UK Biobank users at SDCA/AU.
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
)

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# Or just some files:
source(here::here("R/functions.R"))

# Things to run in order to work.
list(
  # TODO: Uncomment this *after* finishing running `data-raw/create-data.R`
  tar_target(
    name = project_data_path,
    # TODO: This will eventually need to be changed to "parquet".
    command = ukbAid::download_data(file_ext = "csv", username = "nielsbock"),
    format = "file"
  ),
  tar_target(
    name = base_data,
    command = readr::read_csv(project_data_path)
  ),
  tar_target(
    name = readied_data,
    command = base_data |>
      ready_data() |>
      remove_timestamp()
  ),
  tar_target(
    name = icd10_subset,
    command = icd10_longer_subset(readied_data)
  ),
  tar_target(
    name = cancer_subset,
    command = cancer_longer_subset(readied_data)
  ),
  tar_target(
    name = data_with_icd10_cancer,
    command = readied_data |>
      # TODO: All these left_joins aren't necessary, but I will look at that later.
      left_join(icd10_hcc(icd10_subset), by = "id") |>
      left_join(icd10_icc(icd10_subset), by = "id") |>
      left_join(cancer_hcc(cancer_subset), by = "id") |>
      left_join(cancer_icc(cancer_subset), by = "id")
  ),
  tar_target(
    name = data_as_baseline,
    command = data_with_icd10_cancer |>
      baseline_date()
  ),
  tar_target(
    name = data_with_covariates,
    command = data_as_baseline |>
      covariates()
  ),
  tar_target(
    name = data_with_met_synd,
    command = data_with_covariates |>
      metabolic_syndrome()
  ),
  tar_target(
    name = data_with_other_variables,
    command = data_with_met_synd |>
      other_variables()
  ),
  tar_target(
    name = data_with_diet,
    command = data_with_other_variables |>
      calculate_food_intake()
  ),
  tar_target(
    name = data_with_other_diet,
    command = data_with_diet |>
      food_intake_extra()
  ),
  tar_target(
    name = data_with_legume_stratified,
    command = data_with_other_diet |>
      legume_strat()
  ),
  tar_target(
    name = data_with_birth_date,
    command = data_with_legume_stratified |>
      birth_date()
  ),
  tar_target(
    name = data_with_baseline_age,
    command = data_with_birth_date |>
      baseline_age()
  ),
  tar_target(
    name = data_with_l2fu,
    command = data_with_baseline_age |>
      follow_up()
  ),
  tar_target(
    name = data_with_eofu,
    command = data_with_l2fu |>
      end_of_follow_up()
  ),
  tar_target(
    name = data_without_liver_cancer_before,
    command = remove_liver_before(data_with_eofu)
  ),
  tar_target(
    name = data_cleaned,
    command = data_with_eofu %>%
      anti_join(data_without_liver_cancer_before, by = "id")
  ),
  tar_target(
    name = data_with_hcc,
    command = cancer_is_hcc(data_cleaned)
  ),
  tar_target(
    name = data_with_icc,
    command = cancer_is_icc(data_cleaned)
  ),
  tar_target(
    name = data_with_alc,
    command = remove_high_alcohol(data_cleaned)
  ),
  tar_target(
    name = data_with_misreporter,
    command = remove_misreporter(data_cleaned)
  ),
  tar_target(
    name = data_with_3_ques_comp,
    command = filter_ques_comp(data_cleaned)
  )
)
