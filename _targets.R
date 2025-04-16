# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(targets)
library(crew) #distributed computing
library(tarchetypes) # extra targets funcionality
library(tidyverse)
library(tibble)
library(patchwork) #for ggplot2 multiplot
library(lme4)
library(merTools) #for prediction intervals
conflicted::conflicts_prefer(dplyr::select)

## SOME interesting functions to run:

#tar_make()
#tar_visnetwork()
#data_all <- tar_read(data_all)
#elements_list <- tar_read(elements_list)
#proffer::pprof(tar_make(callr_function = NULL))

#tar_destroy()
#use_targets()

# Set target options:

# Run the R scripts in the source/ folder with your custom functions:
tar_source(files = "source")

tar_option_set(
  packages = c("tibble", "tidyverse", "googlesheets4", "lme4", "merTools"),
  controller = crew_controller_local(workers = 3)
)
options(gargle_oauth_email = "pieter.verschelde@inbo.be")
tar_option_set(error = "continue")

# source supporting functions
fun_files <- list.files(pattern = "^fun_.*\\.R$",
                        path = "source",
                        full.names = TRUE)
for (file in fun_files) {
  tar_source(file)
}

#source material:
#https://docs.google.com/spreadsheets/d/1-Arczbqcx46s9RO4Fmn77Jh0aG4C2aYPdqo18CqREvc
#https://docs.google.com/spreadsheets/d/1m_bETGzGrPd49cpCtjYoiGH9cantpY3_2tIBF95DxlM


list(
  #lees data uit google sheets
  tar_target(
    data_input,
    read_gsheet_data(
      icp_key = "1-Arczbqcx46s9RO4Fmn77Jh0aG4C2aYPdqo18CqREvc",
      ic_key = "1m_bETGzGrPd49cpCtjYoiGH9cantpY3_2tIBF95DxlM",
      sheet = "QRY Dataset",
      lod_sheet = "Detectielimieten")
  ),
  #maak de analysedata: combineer de datasets; bereken log10, detectielimieten, type en matrix
  tar_target(
    data_all,
    create_analysis_data(data_input)
  ),
  #bekijk de ruwe data
  tar_target(
    first_plot,
    check_cleaned_data(data_all)
  ),
  #maak de lijst met unieke elementen waarvoor de analyse wordt gedaan
  tar_target(
    elements_list,
    unique(data_all$element)),
  #maak de vereenvoudigde dataset met enkel de projectdata en grondwater waarbij minstens 1 waarde > loq
  tar_target(
    data_simple,
    create_simple_data(data_all)
  ),
  #maak de analyse (afhankelijk van de concentratie van de elementen)
  tar_map(
    values = tar_read(elements_list) %>% tibble(element = .),
    names = element,
    tar_target(
      models_simple_diff,
      analyse_element_simple_diff(data = data_simple, chem_element = element)
    )
  ),
  tar_map(
    values = tar_read(elements_list) %>% tibble(element = .),
    names = element,
    tar_target(
      models_simple_fmod,
      analyse_element_simple_fmod(data = data_simple, chem_element = element)
    )
  ),
  #maak het rapport
  tar_render(report,
             "Report.Rmd"
  )
)

