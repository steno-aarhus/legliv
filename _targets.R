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
source(here::here("R/gtsummary-theme.R"))
source(here::here("R/table-baseline.R"))
source(here::here("R/table-diet.R"))
source(here::here("R/table-main.R"))
source(here::here("R/table-legume-quartiles.R"))
source(here::here("R/table-cancer-type.R"))

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
    name = readied_data,
    command = data_with_id |>
      other_variables() |>
      two_ques_only() |>
      remove_timestamp() |>
      baseline_date() |>
      birth_date() |>
      baseline_age() |>
      remove_ques_error()
  ),
  tar_target(
    name = data_with_covariates,
    command = readied_data |>
      covariates() |>
      metabolic_syndrome()
  ),
  tar_target(
    name = data_with_diet,
    command = data_with_covariates |>
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
    name = cancer_subset_death,
    command = cancer_longer_subset_death(data_with_split_column)
  ),
  tar_target(
    name = data_with_liver_cancer,
    command = data_with_split_column |>
      # TODO: All these left_joins aren't necessary, but I will look at that later.
      left_join(liver_cancer_main(cancer_subset), by = "id") |>
      left_join(liver_cancer_hcc(cancer_subset), by = "id") |>
      left_join(liver_cancer_icc(cancer_subset), by = "id") |>
      left_join(liver_cancer_main_death(cancer_subset_death), by = "id")
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
    name = data_prepare_death,
    command = data_with_liver_cancer |>
      remove_before_baseline_death()
  ),
  tar_target(
    name = data_main,
    command = data_prepare_main |>
      left_join(end_of_follow_up_main(data_prepare_main), by = "id") |>
      make_status_age() |>
      reduce_dataset()
  ),
  tar_target(
    name = data_with_hcc,
    command = data_prepare_hcc |>
      left_join(end_of_follow_up_hcc(data_prepare_hcc), by = "id") |>
      make_status_age() |>
      reduce_dataset()
  ),
  tar_target(
    name = data_with_icc,
    command = data_prepare_icc |>
      left_join(end_of_follow_up_icc(data_prepare_icc), by = "id") |>
      make_status_age() |>
      reduce_dataset()
  ),
  tar_target(
    name = data_with_death,
    command = data_prepare_death |>
      left_join(end_of_follow_up_death(data_prepare_death), by = "id") |>
      make_status_age() |>
      reduce_dataset()
  ),
  tar_target(
    name = data_with_alc,
    command = data_main |>
      high_alcohol()
  ),
  tar_target(
    name = data_with_misreporter,
    command = data_main |>
      energy_outlier()
  ),
  tar_target(
    name = data_with_3_ques_comp,
    command = data_main |>
      filter_ques_comp()
  ),
  tar_target(
    name = data_flowchart,
    command = data_with_id |>
      reduce_full_data() |>
      left_join(reduce_baseline_data(data_with_liver_cancer), by = "id")
  ),
  tar_target(
    name = flowchart,
    command = data_flowchart |>
      create_flowchart()
  ),
  tar_target(
    name = data_without_liver_disease,
    command = data_main |>
      left_join(icd_liver_disease(icd_subset), by = "id") |>
      remove_liver_disease_before()
  ),
  tar_target(
    name = data_without_cancer,
    command = data_main |>
      left_join(icd_any_cancer(cancer_subset), by = "id") |>
      remove_any_cancer_before()
  ),
  tar_target(
    name = data_nosoy,
    command = data_main |>
      remove_soymilk()
  ),
  tar_target(
    name = gt_theme,
    command = my_theme()
  ),
  tar_target(
    name = table_one,
    command = data_main |>
      create_table_one(gt_theme)
  ),
  tar_target(
    name = table_diet,
    command = data_main |>
      create_table_diet(gt_theme)
  ),
  tar_target(
    name = table_main_list,
    command = data_main |>
      create_table_main(gt_theme)
  ),
  tar_target(
    name = table_legume_quartiles_list,
    command = data_main |>
      create_table_legume_quartiles(gt_theme)
  ),
  tar_target(
    name = table_hcc,
    command = data_with_hcc |>
      prepare_table_hcc(gt_theme)
  ),
  tar_target(
    name = table_icc,
    command = data_with_icc |>
      prepare_table_icc(gt_theme)
  ),
  tar_target(
    name = table_cancer_type,
    command = create_table_cancer(table_hcc$row1,table_hcc$row2,table_hcc$row3,
                                  table_icc$row4,table_icc$row5,table_icc$row6, gt_theme)
  )
)
