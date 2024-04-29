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
    name = data_with_id,
    command = base_data |>
      data_id()
  ),
  tar_target(
    name = data_with_covariates,
    command = data_with_id |>
      covariates() |>
      metabolic_syndrome() |>
      other_variables()
  ),
  tar_target(
    name = readied_data,
    command = data_with_covariates |>
      two_ques_only() |>
      remove_timestamp() |>
      baseline_date() |>
      birth_date() |>
      baseline_age()
  ),
  tar_target(
    name = data_with_diet,
    command = readied_data |>
      calculate_food_intake() |>
      food_intake_extra() |>
      legume_strat()
  ),
  tar_target(
    name = data_with_split_column,
    command = data_with_diet |>
      split_column()
  ),
  tar_target(
    name = icd_subset,
    command = icd_longer_subset(data_with_split_column)
  ),
  tar_target(
    name = cancer_subset,
    command = cancer_longer_subset(data_with_split_column)
  ),
  tar_target(
    name = data_with_liver_cancer,
    command = data_with_split_column |>
      # TODO: All these left_joins aren't necessary, but I will look at that later.
      left_join(liver_cancer_main(cancer_subset), by = "id") |>
      left_join(liver_cancer_hcc(cancer_subset), by = "id") |>
      left_join(liver_cancer_icc(cancer_subset), by = "id")
  ),
  tar_target(
    name = data_prepare_main,
    command = data_with_liver_cancer |>
      remove_before_baseline_main()
  ),
  tar_target(
    name = data_prepare_hcc,
    command = data_with_liver_cancer |>
      remove_before_baseline_hcc()
  ),
  tar_target(
    name = data_prepare_icc,
    command = data_with_liver_cancer |>
      remove_before_baseline_icc()
  ),
  tar_target(
    name = data_main,
    command = data_prepare_main |>
      left_join(end_of_follow_up_main(data_prepare_main), by = "id") |>
      make_status_age()
  ),
  tar_target(
    name = data_with_hcc,
    command = data_prepare_hcc |>
      left_join(end_of_follow_up_hcc(data_prepare_hcc), by = "id") |>
      make_status_age()
  ),
  tar_target(
    name = data_with_icc,
    command = data_prepare_icc |>
      left_join(end_of_follow_up_icc(data_prepare_icc), by = "id") |>
      make_status_age()
  ),
  tar_target(
    name = data_with_alc,
    command = data_main |>
      remove_high_alcohol()
  ),
  tar_target(
    name = data_with_misreporter,
    command = data_main |>
      remove_misreporter()
  ),
  tar_target(
    name = data_with_3_ques_comp,
    command = data_main |>
      filter_ques_comp()
  ),
  tar_target(
    name = data_full_reduced,
    command = data_with_id |>
      reduce_full_data()
  ),
  tar_target(
    name = data_baseline_reduced,
    command = data_with_liver_cancer |>
      reduce_baseline_data()
  ),
  tar_target(
    name = data_flowchart,
    command = data_full_reduced |>
      left_join(data_baseline_reduced, by = "id")
  ),
  tar_target(
    name = data_liver_disease,
    command = data_main |>
      left_join(icd_liver_disease(icd_subset), by = "id")
  ),
  tar_target(
    name = data_without_liver_disease,
    command = data_liver_disease |>
      remove_liver_disease_before()
  )
)
