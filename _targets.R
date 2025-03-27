# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:

# Run the R scripts in the source/ folder with your custom functions:
tar_source(files = "source")

tar_option_set(
  packages = c("tibble", "tidyverse", "googlesheets4")
)
options(gargle_oauth_email = "pieter.verschelde@inbo.be")


# execute targets
list(
  tar_target(
    data_input,
    read_gsheet_data(
      icp_key = "1-Arczbqcx46s9RO4Fmn77Jh0aG4C2aYPdqo18CqREvc",
      ic_key = "1m_bETGzGrPd49cpCtjYoiGH9cantpY3_2tIBF95DxlM",
      sheet = "QRY Dataset",
      lod_sheet = "Detectielimieten")),
  tar_target(
    data_all,
    clean_data(data_input)
  ),
  tar_target(
    first_plot,
    check_cleaned_data(data_all)
  )
)
