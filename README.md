# legliv: *Association between substitution of red meat with legumes and risk of primary liver cancer in UK Biobank participants: A prospective cohort study*

-   Protocol:
    [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11670569.svg)](https://doi.org/10.5281/zenodo.11670569)
-   Preprint:
    [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12666778.svg)](https://doi.org/10.5281/zenodo.12666778)

With this project, we will statistically model replacement of red meat
consumption with legumes in relation to incident primary liver cancer
and assess whether the association is mediated through non-alcoholic
fatty liver disease (NAFLD).

This research project uses the UK Biobank Resource under Application
Number 81520.

# Installing and setting up the project

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `.Rproj` file and
running this command in the console:

``` r
pak::pak()
```

To build the reports and papers to PDF, you'll need to install TinyTeX:

``` r
tinytex::install_tinytex()
```

## Steps to download the data and reproduce the analysis

The `data-raw/` folder contains the scripts to select, process, and
prepare the data on the RAP to eventually be downloaded.

The steps to take to select the variables you want, create the CSV file
on the RAP and download to your project on RAP. The order is:

1.  Select the variables you want in `data-raw/project-variables.csv`.
2.  Follow the instructions in the `data-raw/create-data.R` script and
    run it to create the CSV file on the RAP server.
3.  Run `targets::tar_make()` to download the CSV file to `data/` and
    get the project analysis to the current state.

# Brief description of folder and file contents

The following folders contain:

-   `data/`: Will contain the UK Biobank data (not saved to Git) as well
    as the intermediate results output files.

-   `data-raw/`: Contains the R script to download the data, as well as
    the CSV files that contain the project variables and the variable
    list as named in the RAP.

-   `doc/`: This file contains the R Markdown, Word, or other types of
    documents with written content, like the manuscript and protocol.

-   `R/`: Contains the R scripts and functions to create the figures,
    tables, and results for the project.
