read_gsheet_data <- function(icp_key, ic_key, sheet, lod_sheet){

  #lees detectielimieten
  detection_limits <-
    bind_rows(read_sheet(icp_key, lod_sheet),
              read_sheet(ic_key, lod_sheet)) |>
    rename(instrument = "toestel",
           loq = "lod")

  #lees icp data
  data_icp <- read_sheet(icp_key, sheet) |>
    select(-`Test number`) |>
    mutate(instrument = "ICP",
           row_nr = paste0("ICP", sprintf("%04d", row_number()))) |>
    pivot_longer(cols = Al:Zn, names_to = "element", values_to = "value") |>
    mutate(Filter = ifelse(Filter == "0,20 µm",
                           "f_20_micron",
                           ifelse(Filter == "0,45 µm",
                                  "f_45_micron",
                                  NA_character_)),
           umid = interaction(Labo_ID, instrument, element),)

  #lees ic data
  data_ic <- read_sheet(ic_key, sheet) |>
    mutate(instrument = "IC",
           row_nr = paste0("IC", sprintf("%04d", row_number()))) |>
    pivot_longer(cols = Cl:Ca,
                 names_to = "element",
                 values_to = "value") |>
    mutate(Filter = ifelse(Filter == "0,20 µm",
                           "f_20_micron",
                           ifelse(Filter == "0,45 µm",
                                  "f_45_micron",
                                  NA_character_)),
           umid = interaction(Labo_ID, instrument, element))

  return(list(icp = data_icp, ic = data_ic, loq = detection_limits))
}
