library(ukbAid)

stop("To prevent accidental sourcing of this script.")

# Protocol ----------------------------------------------------------------

path <- here::here("doc/protocol.qmd")
pdf_path <- zen_create_protocol_pdf(path)
metadata <- zen_create_protocol_metadata(path)
client <- zen_create_file_record(
  path = pdf_path,
  metadata = metadata,
  token = zen_get_token(),
  # Test it in the sandbox first
  sandbox = FALSE
)

# Preprint ----------------------------------------------------------------

path <- here::here("doc/report.pdf")
# This needs to be manual because the Rmd template uses a non-standard YAML header.
metadata <- list(
  # Take title from the YAML header of the protocols file.
  title = here::here("doc/report.Rmd") |>
    rmarkdown::yaml_front_matter() |>
    _$title,
  creator = here::here("doc/protocol.qmd") |>
    ukbAid:::zen_get_file_metadata_authors(),
  accessRights = "open",
  format = "publication",
  description = glue::glue("Preprint for a research project using UK Biobank. {ukbAid::statements$paper}"),
  isPartOf = list(ukbAid:::zen_get_project_github_url())
)
client <- zen_create_file_record(
  paths = path,
  metadata = metadata,
  token = zen_get_token(),
  # Test it in the sandbox first
  sandbox = FALSE
)
